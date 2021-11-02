module Lab2.PropertyTest1

open FsCheck
open FsCheck.NUnit
open Lab2

let rec isGraphValid graph =
    match graph with
    | Graph.Empty -> true
    | Graph.Next (edge, next) ->
        let struct (na, nb) = edge.Nodes

        na < nb
        && match next with
           | Graph.Empty -> true
           | Graph.Next (nextEdge, _) -> edge.Nodes < nextEdge.Nodes && isGraphValid next

[<Property>]
let ``Graph is equal to itself`` (graph: int Graph.Graph) = graph = graph

[<Property>]
let ``Union of valid graphs is a valid graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    not (isGraphValid left && isGraphValid right)
    || isGraphValid (Graph.union left right)

[<Property>]
let ``Complement of valid graphs is a valid graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    not (isGraphValid left && isGraphValid right)
    || isGraphValid (Graph.complement left right)

[<Property>]
let ``Empty graph is identity element for union`` (graph: int Graph.Graph) =
    not (isGraphValid graph)
    || Graph.union graph Graph.Empty = graph

[<Property>]
let ``Complement with empty graph produces the base graph`` (graph: int Graph.Graph) =
    not (isGraphValid graph)
    || Graph.complement graph Graph.Empty = graph

[<Property>]
let ``Complement of empty graph is empty graph`` (graph: int Graph.Graph) =
    not (isGraphValid graph)
    || Graph.complement Graph.Empty graph = Graph.Empty

[<Property>]
let ``Union of graph with itself is itself`` (graph: int Graph.Graph) =
    not (isGraphValid graph)
    || Graph.union graph graph = graph

[<Property>]
let ``Complement of graph with itself is empty graph`` (graph: int Graph.Graph) =
    not (isGraphValid graph)
    || Graph.complement graph graph = Graph.Empty

[<Property>]
let ``Complement of graphs is complement of union and right graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    let union = Graph.union left right
    let compLeft = Graph.complement left right
    let compUnion = Graph.complement union right

    not (isGraphValid left && isGraphValid right)
    || (compLeft = compUnion)

[<Property>]
let ``Union of graphs is associative`` (a: int Graph.Graph) (b: int Graph.Graph) (c: int Graph.Graph) =
    not (isGraphValid a && isGraphValid b && isGraphValid c)
    || Graph.union (Graph.union a b) c = Graph.union a (Graph.union b c)

[<Property>]
let ``Union of graphs is commutative`` (left: int Graph.Graph) (right: int Graph.Graph) =
    let complement = Graph.complement left right
    let unionLeft = Graph.union complement right
    let unionRight = Graph.union right complement

    not (isGraphValid left && isGraphValid right)
    || unionLeft = unionRight

[<Property>]
let ``Identity mapping is identity`` (graph: int Graph.Graph) =
    not (isGraphValid graph)
    || Graph.map (fun e -> e.Weight) graph = graph

[<Property>]
let ``Double mapping``
    (Fun f: Function<int Graph.Edge, int>)
    (Fun g: Function<int Graph.Edge, int>)
    (graph: int Graph.Graph)
    =
    let composed (e: int Graph.Edge) = f e |> Graph.edge e.Nodes |> g

    not (isGraphValid graph)
    || (graph |> Graph.map f |> Graph.map g) = (Graph.map composed graph)

[<Property>]
let ``True filter produces identical graph`` (graph: int Graph.Graph) =
    let filter _ = true

    not (isGraphValid graph)
    || Graph.filter filter graph = graph

[<Property>]
let ``False filter produces empty graph`` (graph: int Graph.Graph) =
    let filter _ = false

    not (isGraphValid graph)
    || Graph.filter filter graph = Graph.Empty

[<Property>]
let ``Product of filters`` (Fun f) (Fun g) (graph: int Graph.Graph) =
    let composed e = f e && g e

    let filtered =
        graph |> Graph.filter f |> Graph.filter g

    isGraphValid graph
    ==> ((graph |> Graph.filter f |> Graph.filter g) = (Graph.filter composed graph))

[<Property>]
let ``Sum of filters`` (Fun f) (Fun g) (graph: int Graph.Graph) =
    let composed e = f e || g e
    let filtered1 = Graph.filter f graph
    let filtered2 = Graph.filter g graph

    isGraphValid graph
    ==> ((Graph.union filtered1 filtered2) = (Graph.filter composed graph))

[<Property>]
let ``Map and filter``
    (Fun f: Function<int Graph.Edge, bool>)
    (Fun g: Function<int Graph.Edge, int>)
    (graph: int Graph.Graph)
    =
    let composed (e: int Graph.Edge) = g e |> Graph.edge e.Nodes |> f
    let mapFilter = graph |> Graph.map g |> Graph.filter f

    let filterMap =
        graph |> Graph.filter composed |> Graph.map g

    isGraphValid graph ==> (mapFilter = filterMap)
