namespace IronKernel

module Parser =

    open FParsec
    open Ast
    open Errors
    open Source

    let symbol : Parser<char,unit> = anyOf  "!#$%|*+-/:<=>?@^_~."

    let comment = pchar ';' >>. restOfLine true
    let whiteSpace = anyOf " \t\r\n" |>> fun x -> string x

    let ws  = skipMany  (whiteSpace <|> comment)
    let ws1 = skipMany1 (whiteSpace <|> comment)

    let endBy p sep = many ( p .>> sep)

    let stringLiteral  : Parser<string,unit>=
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
        between (pstring "\"") (pstring "\"")
                (stringsSepBy normalCharSnippet escapedChar)


    let parseString  : Parser<LispVal,unit> = stringLiteral |>> makeObj

    let parseAtom : Parser<LispVal,unit> =
        parse {
            let! first = letter <|> symbol
            let! rest = manyChars (letter <|> digit <|> symbol)

            return
                if first.Equals(':') then Keyword rest
                else
                    let atom = first.ToString() + rest
                    match atom with
                    | "#t" -> Bool(true)
                    | "#f" -> Bool(false)
                    | "#inert" -> Inert
                    | _    -> Atom atom }

    // We want to support decimal or hexadecimal numbers with an optional minus
    // sign. Integers may have an 'L' suffix to indicate that the number should
    // be parsed as a 64-bit integer.
    let numberFormat =     NumberLiteralOptions.AllowMinusSign
                       ||| NumberLiteralOptions.AllowFraction
                       ||| NumberLiteralOptions.AllowExponent
                       ||| NumberLiteralOptions.AllowHexadecimal
                       ||| NumberLiteralOptions.AllowSuffix

    let pnumber : Parser<LispVal, unit> =
        let parser = numberLiteral numberFormat "number"
        fun stream ->
            let reply = parser stream
            if reply.Status = Ok then
                let nl = reply.Result // the parsed NumberLiteral
                if nl.SuffixLength = 0
                   || (   nl.IsInteger
                       && nl.SuffixLength = 1 && nl.SuffixChar1 = 'L')
                then
                    try
                        let result = if nl.IsInteger then
                                         if nl.SuffixLength = 0 then
                                             int32 nl.String :> obj |> Obj
                                         else
                                             int64 nl.String :> obj |> Obj
                                     else
                                         if nl.IsHexadecimal then
                                             float (floatOfHexString nl.String) :> obj |> Obj
                                         else
                                             float (float nl.String) :> obj |> Obj
                        Reply(result)
                    with
                    | :? System.OverflowException as e ->
                        stream.Skip(-nl.String.Length)
                        Reply(FatalError, messageError e.Message)
                else
                    stream.Skip(-nl.SuffixLength)
                    Reply(Error, messageError "invalid number suffix")
            else // reconstruct error reply
                Reply(reply.Status, reply.Error)

    let parseNumber = pnumber

    let private sourcePosition (position: FParsec.Position) : SourcePosition =
        { offset = position.Index
          line = position.Line
          column = position.Column }

    let private sourceSpan (startPosition: FParsec.Position) (endPosition: FParsec.Position) =
        { sourceName = startPosition.StreamName
          startPosition = sourcePosition startPosition
          endPosition = sourcePosition endPosition }

    let private locatedDatum parser : Parser<LocatedValue, unit> =
        parse {
            let! startPosition = getPosition
            let! value = parser
            let! endPosition = getPosition
            let kind =
                match value with
                | Atom name -> LAtom name
                | other -> LLiteral other
            return { kind = kind; span = sourceSpan startPosition endPosition }
        }

    let parseLocatedString = locatedDatum parseString
    let parseLocatedNumber = locatedDatum parseNumber
    let parseLocatedAtom = locatedDatum parseAtom

    let private parseLocatedExpr, parseLocatedExprRef =
        createParserForwardedToRef<LocatedValue, unit> ()

    let private parseLocatedDottedMarker =
        locatedDatum (skipChar '&' >>% Atom "&")

    let private parseLocatedList : Parser<LocatedValue list,unit> =
        sepEndBy (parseLocatedDottedMarker <|> parseLocatedExpr) ws1
    let private parseLocatedArray : Parser<LocatedValue array,unit> =
        sepEndBy parseLocatedExpr ws1 |>> List.toArray
    let private parseLocatedQuoted : Parser<LocatedValue,unit> =
        parse {
            let! startPosition = getPosition
            do! skipChar '\''
            let! quoted = parseLocatedExpr
            let! endPosition = getPosition
            return
                { kind = LQuote quoted
                  span = sourceSpan startPosition endPosition }
        }

    do parseLocatedExprRef.Value <-
        parseLocatedString
        <|> parseLocatedNumber
        <|> parseLocatedAtom
        <|> parseLocatedQuoted
        <|> parse {
                let! startPosition = getPosition
                do! skipChar '('
                do! ws
                let! values = parseLocatedList
                let! kind =
                    let isDottedMarker value =
                        match value.kind with
                        | LAtom "&" -> true
                        | _ -> false
                    match values |> List.tryFindIndex isDottedMarker with
                    | None -> preturn (LList values)
                    | Some markerIndex ->
                        let head = List.take markerIndex values
                        match List.skip (markerIndex + 1) values with
                        | [tail] when not (isDottedMarker tail) -> preturn (LDottedList(head, tail))
                        | _ -> fail "dotted list requires exactly one tail"
                do! skipChar ')'
                let! endPosition = getPosition
                return
                    { kind = kind
                      span = sourceSpan startPosition endPosition }
            }
        <|> parse {
                let! startPosition = getPosition
                do! skipChar '['
                do! ws
                let! values = parseLocatedArray
                do! skipChar ']'
                let! endPosition = getPosition
                return
                    { kind = LVector values
                      span = sourceSpan startPosition endPosition }
            }

    let parseExpr : Parser<LispVal, unit> = parseLocatedExpr |>> toLispVal

    let private conciseParseMessage (message: string) =
        let lines = message.Replace("\r\n", "\n").Split('\n')
        match
            lines
            |> Array.tryFindIndex (fun line ->
                line.StartsWith("Expecting", System.StringComparison.Ordinal)
                || line.StartsWith("Unexpected", System.StringComparison.Ordinal)
                || line.StartsWith("The parser", System.StringComparison.Ordinal))
        with
        | Some index -> System.String.Join(System.Environment.NewLine, lines.[index..])
        | None -> "invalid syntax"

    let private readLocatedOrThrow parser sourceName input =
        match runParserOnString parser () sourceName input with
        | Success(result,_,_) -> Choice2Of2 result
        | Failure(message, parserError, _) ->
            let position = parserError.Position
            let point = sourcePosition position
            let span =
                { sourceName = position.StreamName
                  startPosition = point
                  endPosition = point }
            let line = sourceLineAt input position.Line
            throwError (LocatedError(span, line, Parser(conciseParseMessage message)))

    let readLocatedExpr sourceName input =
        readLocatedOrThrow (ws >>. parseLocatedExpr .>> ws .>> eof) sourceName input

    let readLocatedExprList sourceName input =
        readLocatedOrThrow (ws >>. many (parseLocatedExpr .>> ws) .>> eof) sourceName input

    let readExprFromSource sourceName input =
        match readLocatedExpr sourceName input with
        | Choice1Of2 error -> throwError error
        | Choice2Of2 value -> succeed (toLispVal value)

    let readExprListFromSource sourceName input =
        match readLocatedExprList sourceName input with
        | Choice1Of2 error -> throwError error
        | Choice2Of2 values -> values |> List.map toLispVal |> succeed

    let readExpr = readExprFromSource ""
    let readExprList = readExprListFromSource ""
