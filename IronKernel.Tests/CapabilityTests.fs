module IronKernel.Tests.CapabilityTests

open Xunit

open IronKernel.Ast
open IronKernel.Capabilities
open IronKernel.Emit
open IronKernel.Errors
open IronKernel.Runtime
open IronKernel.SymbolTable
open IronKernel.Tests.TestHelpers

let private expectUnbound env name =
    match getVar env name with
    | Choice1Of2 (UnboundVar _) -> ()
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 value -> failwithf "%s unexpectedly resolved to %s" name (showVal value)

[<Fact>]
let ``profiles expose only their declared host authority`` () =
    let minimal = makePrimitiveBindingsForProfile Minimal
    let safe = makePrimitiveBindingsForProfile Safe
    let unrestricted = makePrimitiveBindingsForProfile Unrestricted

    for name in [ "."; "new"; ".get"; ".set"; "clr-open"; "clr-alias"; "clr-type"; "clr-opens"; "load"; "read-contents"; "Console.write-line"; "await-task"; "task-delay" ] do
        expectUnbound minimal name

    for name in [ "."; "new"; ".get"; ".set"; "clr-open"; "clr-alias"; "clr-type"; "clr-opens"; "load"; "read-contents"; "print"; "await-task"; "task-delay" ] do
        expectUnbound safe name

    match getVar safe "Console.write-line", getVar unrestricted "." with
    | Choice2Of2 (Applicative _), Choice2Of2 (PrimitiveOperative _) -> ()
    | values -> failwithf "missing expected profile bindings: %A" values

[<Fact>]
let ``safe generated bindings make direct typed CLR calls`` () =
    let env = makePrimitiveBindingsForProfile Safe
    assertEval env "(String.concat \"Iron\" \"Kernel\")" (Obj ("IronKernel" :> obj))

    match evalIn env "(Math.sqrt 81)" with
    | Obj (:? double as value) -> Assert.Equal(9.0, value)
    | value -> failwith ("unexpected sqrt result " + showVal value)

[<Fact>]
let ``generated binding rejects use after authority is removed`` () =
    let safe = makePrimitiveBindingsForProfile Safe
    let minimal = makePrimitiveBindingsForProfile Minimal
    let generated = getVar safe "String.concat" |> function Choice2Of2 value -> value | Choice1Of2 error -> failwith (showError error)
    ignore (defineVar minimal "String.concat" generated)

    match evalRaw Interpreted minimal "(String.concat \"a\" \"b\")" with
    | Choice1Of2 (CapabilityDenied _) -> ()
    | result -> failwithf "stolen generated binding escaped its capability: %A" result

[<Fact>]
let ``raw reflection rejects use from a safe environment`` () =
    let unrestricted = makePrimitiveBindingsForProfile Unrestricted
    let safe = makePrimitiveBindingsForProfile Safe
    let rawDot = getVar unrestricted "." |> function Choice2Of2 value -> value | Choice1Of2 error -> failwith (showError error)
    ignore (defineVar safe "." rawDot)

    match evalRaw Interpreted safe "(. System.Guid NewGuid)" with
    | Choice1Of2 (CapabilityDenied _) -> ()
    | result -> failwithf "stolen reflection binding escaped its capability: %A" result

[<Fact>]
let ``child environments can only attenuate parent authority`` () =
    let safe = makePrimitiveBindingsForProfile Safe
    let unrestricted = makePrimitiveBindingsForProfile Unrestricted
    let child = newEnv [safe; unrestricted]

    Assert.True(has (GeneratedClr "safe") child)
    Assert.False(has RawClrInterop child)
    Assert.False(has HostIO child)

[<Fact>]
let ``safe bootstrap loads stdlib without granting source loading`` () =
    match bootstrapEnvForProfile Safe with
    | Choice1Of2 error -> failwith (showError error)
    | Choice2Of2 env ->
        expectUnbound env "load"
        assertEval env "((lambda (x) x) 42)" (Obj (42 :> obj))
        assertEval env "(String.concat \"safe\" \"-mode\")" (Obj ("safe-mode" :> obj))

[<Fact>]
let ``stolen async primitive checks the invoking environment`` () =
    let unrestricted = makePrimitiveBindingsForProfile Unrestricted
    let safe = makePrimitiveBindingsForProfile Safe
    let awaitPrimitive =
        getVar unrestricted "await-task"
        |> function
            | Choice2Of2 value -> value
            | Choice1Of2 error -> failwith (showError error)
    ignore (defineVar safe "await-task" awaitPrimitive)

    match evalRaw Interpreted safe "(await-task #inert)" with
    | Choice1Of2 (CapabilityDenied _) -> ()
    | result -> failwithf "stolen async primitive escaped its capability: %A" result
