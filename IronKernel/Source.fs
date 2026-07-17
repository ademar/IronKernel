namespace IronKernel

module Source =

    open Ast

    type LocatedValue = {
        kind : LocatedValueKind
        span : SourceSpan
    }

    and LocatedValueKind =
        | LAtom of string
        | LList of LocatedValue list
        | LDottedList of LocatedValue list * LocatedValue
        | LVector of LocatedValue array
        | LLiteral of LispVal
        | LQuote of LocatedValue

    let spanOf value = value.span

    let rec toLispVal value =
        match value.kind with
        | LAtom name -> Atom name
        | LList values -> List (List.map toLispVal values)
        | LDottedList (head, tail) ->
            DottedList (List.map toLispVal head, toLispVal tail)
        | LVector values -> Vector (Array.map toLispVal values)
        | LLiteral literal -> literal
        | LQuote quoted -> List [Atom "quote"; toLispVal quoted]

    let sourceLineAt (source: string) (line: int64) =
        if line < 1L then None
        else
            source.Replace("\r\n", "\n").Split('\n')
            |> Array.tryItem (int line - 1)

    let contains outer inner =
        outer.sourceName = inner.sourceName
        && outer.startPosition.offset <= inner.startPosition.offset
        && outer.endPosition.offset >= inner.endPosition.offset
