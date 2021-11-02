open Lab2.Graph

[<EntryPoint>]
let main _ =
    let graph =
        Next struct ({ Nodes = struct (0, 1); Weight = 0 }, Empty)

    let f _ = true
    let g e = e.Weight
    let filtered = filter f graph
    let mapped = map g graph
    printfn "%A" graph
    printfn "%A" mapped
    printfn "%A" filtered
    printfn ""
    printfn "%A" (map g filtered)
    printfn "%A" (filter f mapped)
    printfn "%A" (map g filtered = filter f mapped)
    0
