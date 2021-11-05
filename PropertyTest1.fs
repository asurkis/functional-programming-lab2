module Lab2.PropertyTest1

open FsCheck
open FsCheck.NUnit
open Lab2

let genEdge (acc, _) (PositiveInt a, PositiveInt b, w) =
    acc + a, Graph.edge (acc + a, acc + a + b) w

let rec listToGraph =
    function
    | [] -> Graph.Empty
    | e :: es -> Graph.Next(e, listToGraph es)

let transformList firstEdge list =
    list
    |> List.scan genEdge (0, firstEdge)
    |> List.map snd
    |> List.skip 1
    |> listToGraph

let rec generateGraph () =
    Gen.map2 transformList
    <| Arb.generate
    <| Gen.listOf Arb.generate

type GraphGenerator =
    static member Graph() =
        { new Arbitrary<'a Graph.Graph>() with
            member _.Generator = generateGraph ()
            member _.Shrinker _ = Seq.empty }

let rec isGraphValid =
    function
    | Graph.Empty -> true
    | Graph.Next ({ Graph.Edge.Nodes = (na, nb)
                    Graph.Edge.Weight = _ },
                  _) when na >= nb -> false
    | Graph.Next (_, Graph.Empty) -> true
    | Graph.Next (edge, Graph.Next (nextEdge, nextElement)) ->
        edge.Nodes < nextEdge.Nodes
        && isGraphValid (Graph.Next(nextEdge, nextElement))

[<NUnit.Framework.SetUp>]
let setup () =
    Arb.register<GraphGenerator> () |> ignore
    ()

[<Property>]
let ``Generated graph is valid`` (graph: int Graph.Graph) = isGraphValid graph

[<Property>]
let ``Graph is equal to itself`` (graph: int Graph.Graph) = graph = graph

[<Property>]
let ``Union of valid graphs is a valid graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    isGraphValid (Graph.union left right)

[<Property>]
let ``Complement of valid graphs is a valid graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    isGraphValid (Graph.complement left right)

[<Property>]
let ``Empty graph is identity element for union`` (graph: int Graph.Graph) = Graph.union graph Graph.Empty = graph

[<Property>]
let ``Complement with empty graph produces the base graph`` (graph: int Graph.Graph) =
    Graph.complement graph Graph.Empty = graph

[<Property>]
let ``Complement of empty graph is empty graph`` (graph: int Graph.Graph) =
    Graph.complement Graph.Empty graph = Graph.Empty

[<Property>]
let ``Union of graph with itself is itself`` (graph: int Graph.Graph) = Graph.union graph graph = graph

[<Property>]
let ``Complement of graph with itself is empty graph`` (graph: int Graph.Graph) =
    Graph.complement graph graph = Graph.Empty

[<Property>]
let ``Complement of graphs is complement of union and right graph`` (left: int Graph.Graph) (right: int Graph.Graph) =
    let union = Graph.union left right
    let compLeft = Graph.complement left right
    let compUnion = Graph.complement union right
    compLeft = compUnion

[<Property>]
let ``Union of graphs is associative`` (a: int Graph.Graph) (b: int Graph.Graph) (c: int Graph.Graph) =
    Graph.union (Graph.union a b) c = Graph.union a (Graph.union b c)

[<Property>]
let ``Union of graphs is commutative`` (left: int Graph.Graph) (right: int Graph.Graph) =
    let complement = Graph.complement left right
    let unionLeft = Graph.union complement right
    let unionRight = Graph.union right complement
    unionLeft = unionRight

[<Property>]
let ``Identity mapping is identity`` (graph: int Graph.Graph) =
    Graph.map (fun e -> e.Weight) graph = graph

[<Property>]
let ``Double mapping`` (Fun (f: int Graph.Edge -> int)) (Fun (g: int Graph.Edge -> int)) (graph: int Graph.Graph) =
    let composed (e: int Graph.Edge) = f e |> Graph.edge e.Nodes |> g
    (graph |> Graph.map f |> Graph.map g) = (Graph.map composed graph)

[<Property>]
let ``True filter produces identical graph`` (graph: int Graph.Graph) =
    let filter _ = true
    Graph.filter filter graph = graph

[<Property>]
let ``False filter produces empty graph`` (graph: int Graph.Graph) =
    let filter _ = false
    Graph.filter filter graph = Graph.Empty

[<Property>]
let ``Product of filters`` (Fun f) (Fun g) (graph: int Graph.Graph) =
    let composed e = f e && g e
    (graph |> Graph.filter f |> Graph.filter g) = (Graph.filter composed graph)

[<Property>]
let ``Sum of filters`` (Fun f) (Fun g) (graph: int Graph.Graph) =
    let composed e = f e || g e
    let filtered1 = Graph.filter f graph
    let filtered2 = Graph.filter g graph
    (Graph.union filtered1 filtered2) = (Graph.filter composed graph)

[<Property>]
let ``Map and filter`` (Fun (f: int Graph.Edge -> bool)) (Fun (g: int Graph.Edge -> int)) (graph: int Graph.Graph) =
    let composed (e: int Graph.Edge) = g e |> Graph.edge e.Nodes |> f
    let mapFilter = graph |> Graph.map g |> Graph.filter f

    let filterMap =
        graph |> Graph.filter composed |> Graph.map g

    mapFilter = filterMap
