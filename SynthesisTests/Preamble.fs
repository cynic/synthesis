(*
IGNORE THIS FILE -- IT CONTAINS SOME HELPER FUNCTIONS.

You want to look at UnitTests.fs instead.
*)
module Preamble
open FsUnit
[<assembly: NUnit.Framework.Parallelizable(NUnit.Framework.ParallelScope.Children)>]
    do ()

let shouldFail f =
    (fun () -> ignore (f ())) |> should throw typeof<System.Exception>