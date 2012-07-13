namespace IronKernel

module Parser =
    
    open FParsec
    open Ast
    open Errors
    
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
            let atom = first.ToString() + rest
            return 
                match atom with 
                | "#t" -> Bool(true)
                | "#f" -> Bool(false)
                | "#inert" -> Inert
                |_     -> Atom atom } 

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
   
    let rec parseList : Parser<LispVal,unit> = (sepEndBy (parseExpr) ws1)  |>> List
    
    and parseDottedList :  Parser<LispVal,unit> = 
        parse {
            let! head = endBy parseExpr spaces1
            let! tail = skipChar '&' >>. spaces1 >>. parseExpr
            return DottedList(head,tail)
        }
    
    and parseQuoted : Parser<LispVal,unit> =
        parse {
            do! skipChar '\''
            let! x = parseExpr
            return List [Atom("quote");x]
        } 

    and parseExpr : Parser<LispVal,unit> = 
        parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parse {
                do! ws
                do! skipChar '('
                do! ws
                let! x = (attempt parseDottedList) <|> parseList
                do! skipChar ')'
                return x
            }
        
    let readOrThrow parser input = 
        match run parser input  with
        |Success(result,_,_) -> Choice2Of2(result)
        |Failure(err,_,_) -> throwError <| Parser(err) 

    let readExpr = readOrThrow parseExpr
    let readExprList = readOrThrow  (endBy parseExpr ws)