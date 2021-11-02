module Lab2.UnitTest1

open NUnit.Framework
open Lab2

[<Test>]
let ``Empty graphs are equal`` () =
    let g1 = Graph.Empty
    let g2 = Graph.Empty
    Assert.AreEqual(g1, g2)

[<Test>]
let ``Empty and non-empty graphs are not equal`` () =
    let g1 = Graph.Empty
    let g2 = Graph.edge (1, 2) 3 |> Graph.singleEdge
    Assert.AreNotEqual(g1, g2)

[<Test>]
let ``Single element graphs with the same element are equal`` () =
    let g1 = Graph.edge (1, 2) 3 |> Graph.singleEdge
    let g2 = Graph.edge (1, 2) 3 |> Graph.singleEdge
    Assert.AreEqual(g1, g2)

[<Test>]
let ``Single element graphs with different elements are not equal`` () =
    let g1 = Graph.edge (1, 2) 3 |> Graph.singleEdge
    let g2 = Graph.edge (1, 2) 4 |> Graph.singleEdge
    Assert.AreNotEqual(g1, g2)
