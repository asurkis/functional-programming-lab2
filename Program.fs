open Lab2

[<EntryPoint>]
let main _ =
    let g1 = Graph.Empty
    let g2 = Graph.edge (0, 0) 0 |> Graph.singleEdge
    let union = Graph.union g1 g2

    printfn "%A" g1
    printfn "%A" g2
    printfn "%A" union
    printfn "%A" (Graph.complement g1 g2)
    printfn "%A" (Graph.complement union g2)

    match union with
    | Graph.Empty -> ()
    | Graph.Next (edgeu, _) ->
        match g2 with
        | Graph.Empty -> ()
        | Graph.Next (edger, _) -> printfn "(%A = %A) = %A" edgeu.Nodes edger.Nodes (edgeu.Nodes = edger.Nodes)

    0
