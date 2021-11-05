module Lab2.Graph

type 'T Edge =
    { Nodes: struct (int * int)
      Weight: 'T }

type 'T Graph =
    | Empty
    | Next of 'T Edge * 'T Graph

let edge struct (na, nb) weight =
    { Nodes = if na < nb then na, nb else nb, na
      Weight = weight }

let singleEdge edge = Next(edge, Empty)

let rec union left right =
    match left, right with
    | Empty, _ -> right
    | _, Empty -> left
    | Next (currl, nextl), Next (currr, _) when currl.Nodes < currr.Nodes -> Next(currl, union nextl right)
    | Next (currl, _), Next (currr, nextr) when currl.Nodes > currr.Nodes -> Next(currr, union left nextr)
    | Next (currl, nextl), Next (_, nextr) -> Next(currl, union nextl nextr)

let rec complement left right =
    match left, right with
    | Empty, _ -> Empty
    | _, Empty -> left
    | Next (currl, _), Next (currr, nextr) when currl.Nodes > currr.Nodes -> complement left nextr
    | Next (currl, nextl), Next (currr, nextr) when currl.Nodes = currr.Nodes -> complement nextl nextr
    | Next (currl, nextl), _ -> Next(currl, complement nextl right)

let rec filter f graph =
    match graph with
    | Empty -> Empty
    | Next (curr, next) when f curr -> Next(curr, filter f next)
    | Next (_, next) -> filter f next

let rec map f graph =
    match graph with
    | Empty -> Empty
    | Next (curr, next) -> Next(edge curr.Nodes (f curr), map f next)

let rec withEdge edge graph = edge |> singleEdge |> union graph

let rec withoutEdge nodes graph =
    complement graph (edge nodes () |> singleEdge)
