module Lab2.PropertyTest1

open FsCheck.NUnit
open Lab2

[<Property>]
let ``Graph is equal to itself`` (graph: int Graph.Graph) = graph = graph

[<Property>]
let ``Complement of two graphs is complement of union and right graph``
    (left: int Graph.Graph)
    (right: int Graph.Graph)
    =
    let union = Graph.union left right
    let compLeft = Graph.complement left right
    let compUnion = Graph.complement union right
    compLeft = compUnion
