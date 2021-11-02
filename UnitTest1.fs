module Lab2.UnitTest1

open NUnit.Framework
open Lab2

[<Test>]
let ``Empty graphs are equal`` () =
    let g1 = Graph.Empty
    let g2 = Graph.Empty
    Assert.AreEqual(g1, g2)
