namespace IronKernel

/// Generates ordinary F# source for persisted managed artifacts.
/// Unsupported dynamic forms fail during artifact construction instead of
/// silently embedding source for parsing or expression compilation at startup.
module StaticCompiler =

    open System
    open System.Globalization
    open System.Text
    open Ast
    open Ir

    let private quote (value: string) = sprintf "%A" value

    let private sequenceOptions values =
        let mutable result = Some []
        for value in List.rev values do
            match value, result with
            | Some item, Some items -> result <- Some(item :: items)
            | _ -> result <- None
        result

    let rec private emitValue = function
        | Atom value -> Some("Atom " + quote value)
        | List values ->
            values
            |> List.map emitValue
            |> sequenceOptions
            |> Option.map (fun emitted -> "List [" + String.concat "; " emitted + "]")
        | DottedList(head, tail) ->
            match head |> List.map emitValue |> sequenceOptions, emitValue tail with
            | Some emittedHead, Some emittedTail ->
                Some("DottedList([" + String.concat "; " emittedHead + "], " + emittedTail + ")")
            | _ -> None
        | Bool value -> Some(if value then "Bool true" else "Bool false")
        | Inert -> Some "Inert"
        | Nil -> Some "Nil"
        | Keyword value -> Some("Keyword " + quote value)
        | Obj null -> Some "Obj null"
        | Obj (:? string as value) -> Some("Obj(box " + quote value + ")")
        | Obj (:? byte as value) -> Some(sprintf "Obj(box %uy)" value)
        | Obj (:? int as value) -> Some(sprintf "Obj(box %d)" value)
        | Obj (:? int64 as value) -> Some(sprintf "Obj(box %dL)" value)
        | Obj (:? float32 as value) ->
            Some("Obj(box " + value.ToString("R", CultureInfo.InvariantCulture) + "f)")
        | Obj (:? double as value) ->
            let literal = value.ToString("R", CultureInfo.InvariantCulture)
            Some("Obj(box " + (if literal.Contains('.') then literal else literal + ".0") + ")")
        | Vector values ->
            values
            |> Array.map emitValue
            |> Array.toList
            |> sequenceOptions
            |> Option.map (fun emitted -> "Vector [|" + String.concat "; " emitted + "|]")
        | _ -> None

    let private emitExpression expression =
        let rec emit = function
            | CLit value
            | CQuote value ->
                emitValue value
                |> Option.map (fun emitted -> "continueEval env cont (" + emitted + ")")
            | CVar name ->
                Some(
                    "match getVar env " + quote name + " with\n"
                    + "    | Choice1Of2 error -> throwError error\n"
                    + "    | Choice2Of2 value -> continueEval env cont value")
            | COperate(CVar name, operands) ->
                operands
                |> List.map emitValue
                |> sequenceOptions
                |> Option.map (fun emitted ->
                    "Helpers.AppNamed(env, cont, " + quote name + ", [|"
                    + String.concat "; " emitted + "|])")
            | CLocated(_, _, inner) -> emit inner
            | _ -> None
        emit expression

    let generateProgram profile expressions =
        let emitted = expressions |> List.map emitExpression
        if emitted |> List.exists Option.isNone then
            Error "The program contains a form or constant unsupported by the static backend"
        else
            let forms = emitted |> List.choose id
            let profileName =
                match profile with
                | Minimal -> "Minimal"
                | Safe -> "Safe"
                | Unrestricted -> "Unrestricted"
            let output = StringBuilder()
            output.AppendLine("open System") |> ignore
            output.AppendLine("open IronKernel.Ast") |> ignore
            output.AppendLine("open IronKernel.Choice") |> ignore
            output.AppendLine("open IronKernel.Compiler") |> ignore
            output.AppendLine("open IronKernel.Errors") |> ignore
            output.AppendLine("open IronKernel.Eval") |> ignore
            output.AppendLine("open IronKernel.Runtime") |> ignore
            output.AppendLine("open IronKernel.SymbolTable") |> ignore
            output.AppendLine() |> ignore
            for index, body in List.indexed forms do
                output.Append("let private form").Append(index).AppendLine(" env cont =") |> ignore
                for line in body.Split('\n') do
                    output.Append("    ").AppendLine(line) |> ignore
                output.AppendLine() |> ignore
            output.AppendLine("[<EntryPoint>]") |> ignore
            output.AppendLine("let main _ =") |> ignore
            output.Append("    let env = makePrimitiveBindingsForProfile ").AppendLine(profileName) |> ignore
            output.AppendLine("    let cont = newContinuation env") |> ignore
            output.AppendLine("    let forms = [|") |> ignore
            for index in 0 .. forms.Length - 1 do
                output.Append("        form").Append(index).AppendLine() |> ignore
            output.AppendLine("    |]") |> ignore
            output.AppendLine("    let mutable result = returnM Inert") |> ignore
            output.AppendLine("    let mutable index = 0") |> ignore
            output.AppendLine("    let mutable running = true") |> ignore
            output.AppendLine("    while index < forms.Length && running do") |> ignore
            output.AppendLine("        result <- forms.[index] env cont") |> ignore
            output.AppendLine("        running <- match result with Choice2Of2 _ -> true | Choice1Of2 _ -> false") |> ignore
            output.AppendLine("        index <- index + 1") |> ignore
            output.AppendLine("    match result with") |> ignore
            output.AppendLine("    | Choice1Of2 error ->") |> ignore
            output.AppendLine("        eprintfn \"%s\" (showError error)") |> ignore
            output.AppendLine("        1") |> ignore
            output.AppendLine("    | Choice2Of2 Inert -> 0") |> ignore
            output.AppendLine("    | Choice2Of2 value ->") |> ignore
            output.AppendLine("        printfn \"%s\" (showVal value)") |> ignore
            output.AppendLine("        0") |> ignore
            Ok(output.ToString())