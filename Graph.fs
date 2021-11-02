module Lab2.Graph

type 'T Edge =
    { Nodes: struct (int * int)
      Weight: 'T }

type 'T Graph =
    | Empty
    | Next of struct ('T Edge * 'T Graph)

let edge struct (na, nb) weight =
    { Nodes = if na < nb then na, nb else nb, na
      Weight = weight }

let singleEdge edge = Next(edge, Empty)

let rec union left right =
    match left with
    | Empty -> right
    | Next (currl, nextl) ->
        match right with
        | Empty -> left
        | Next (currr, nextr) ->
            if currl.Nodes < currr.Nodes then
                Next(currl, union nextl right)
            elif currl.Nodes > currr.Nodes then
                Next(currr, union left nextr)
            else
                Next(currl, union nextl nextr)

let rec complement left right =
    match left with
    | Empty -> Empty
    | Next (currl, nextl) ->
        match right with
        | Empty -> left
        | Next (currr, nextr) ->
            if currl.Nodes < currr.Nodes then
                Next(currl, complement nextl right)
            else
                complement left nextr

let rec filter f graph =
    match graph with
    | Empty -> Empty
    | Next (curr, next) ->
        let filtered = filter f next

        if f curr then
            Next(curr, filtered)
        else
            filtered

let rec map f graph =
    match graph with
    | Empty -> Empty
    | Next (curr, next) ->
        let mapped = f curr
        Next(edge curr.Nodes mapped, map f next)

let rec withEdge edge graph = edge |> singleEdge |> union graph

let rec withoutEdge nodes graph =
    complement graph (edge nodes () |> singleEdge)
