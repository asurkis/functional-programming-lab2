module Graph =
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

    let rec merge left right =
        match left with
        | Empty -> right
        | Next (currl, nextl) ->
            match right with
            | Empty -> left
            | Next (currr, nextr) ->
                if currl.Nodes < currr.Nodes then
                    Next(currl, merge nextl right)
                elif currl.Nodes > currr.Nodes then
                    Next(currr, merge left nextr)
                else
                    Next(currl, merge nextl nextr)

    let rec filter f graph =
        match graph with
        | Empty -> Empty
        | Next (curr, next) ->
            let filtered = filter f next

            if f curr then
                Next(curr, filtered)
            else
                filtered

    let map f graph =
        match graph with
        | Empty -> Empty
        | Next (curr, next) ->
            let mapped = f curr
            Next(edge curr.Nodes mapped, next)

    let rec withEdge edge graph = edge |> singleEdge |> merge graph

    let rec withoutEdge nodes graph =
        let isNotSameEdge e = e.Nodes <> nodes
        filter isNotSameEdge graph

[<EntryPoint>]
let main _ =
    let graph = Graph.Empty

    let more =
        Graph.withEdge (Graph.edge struct (1, 2) 5) graph

    printfn "%A" more
    0
