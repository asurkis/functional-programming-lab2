open Lab2

[<EntryPoint>]
let main _ =
    let left, right =
        (Graph.Next struct ({ Nodes = struct (1, 1); Weight = -6 }, Graph.Empty),
         Graph.Next
             struct ({ Nodes = struct (-1, 2); Weight = -4 },
                     Graph.Next struct ({ Nodes = struct (0, 0); Weight = 0 }, Graph.Empty)))

    // let complement = Graph.complement left right
    // let u1 = Graph.union complement right
    // let u2 = Graph.union right complement
    let union = Graph.union left right
    let c1 = Graph.complement left right
    let c2 = Graph.complement union right

    printfn "left  = %A" left
    printfn "right = %A" right
    printfn "left + right = %A" union
    printfn "left - right = %A" c1
    printfn "(left + right) - right = %A" c2
    printfn "equality = %A" (c1 = c2)
    0
