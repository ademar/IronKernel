module IronKernel.Tests.InteropTests

open Xunit
open IronKernel.Ast
open IronKernel.Tests.TestHelpers

[<Fact>]
let ``new and static method call`` () =
    let env = freshEnv ()
    match evalIn env "(. System.Guid NewGuid)" with
    | Obj (:? System.Guid) -> ()
    | v -> failwith (showVal v)

[<Fact>]
let ``string format via CLR`` () =
    let env = freshEnv ()
    match evalIn env "(. System.String Format \"{0}-{1}\" 1 2)" with
    | Obj (:? string as s) -> Assert.Equal("1-2", s)
    | v -> failwith (showVal v)

[<Fact>]
let ``static property get DateTime`` () =
    let env = freshEnv ()
    match evalIn env "(.get System.DateTime UtcNow)" with
    | Obj (:? System.DateTime) -> ()
    | v -> failwith (showVal v)

[<Fact>]
let ``path combine and file roundtrip`` () =
    let env = freshEnv ()
    let dir = System.IO.Path.GetTempPath()
    let file = System.IO.Path.Combine(dir, "ironkernel-interop-test.txt")
    try
        let escaped = file.Replace("\\", "\\\\")
        ignore (evalIn env (sprintf "(define path \"%s\")" escaped))
        ignore (evalIn env "(. System.IO.File WriteAllText path \"kernel\")")
        match evalIn env "(. System.IO.File ReadAllText path)" with
        | Obj (:? string as s) -> Assert.Equal("kernel", s)
        | v -> failwith (showVal v)
    finally
        if System.IO.File.Exists file then System.IO.File.Delete file

[<Fact>]
let ``console write returns inert`` () =
    [
        "(. System.Console Write \"\")", Inert
    ] |> evalSession
