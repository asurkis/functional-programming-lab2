module Lab2.PropertyTest1

open FsCheck.NUnit
open Lab2

let rec validGraph graph =
    match graph with
    | Graph.Empty -> true
    | Graph.Next (edge, next) ->
        match next with
        | Graph.Empty -> true
        | Graph.Next (nextEdge, _) -> edge.Nodes < nextEdge.Nodes && validGraph next

[<Property>]
let ``Graph is equal to itself`` (graph: int Graph.Graph) = graph = graph

[<Property>]
let ``Union of valid graphs is a valid graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    not (validGraph left && validGraph right)
    || validGraph (Graph.union left right)

[<Property>]
let ``Complement of valid graphs is a valid graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    not (validGraph left && validGraph right)
    || validGraph (Graph.complement left right)

[<Property>]
let ``Complement of graphs is complement of union and right graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    let union = Graph.union left right
    let compLeft = Graph.complement left right
    let compUnion = Graph.complement union right

    not (validGraph left && validGraph right)
    || (compLeft = compUnion)

[<Property>]
let ``Union of graphs is associative`` (a: int Graph.Graph) (b: int Graph.Graph) (c: int Graph.Graph) =
    not (validGraph a && validGraph b && validGraph c)
    || Graph.union (Graph.union a b) c = Graph.union a (Graph.union b c)

[<Property>]
let ``Union of graphs is commutative`` (left: int Graph.Graph) (right: int Graph.Graph) =
    let complement = Graph.complement left right
    let unionLeft = Graph.union complement right
    let unionRight = Graph.union right complement

    not (validGraph left && validGraph right)
    || unionLeft = unionRight
