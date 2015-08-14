open System

type Token =
    | Integer of int
    | Operator of string
    | Eof

let CharToString list =
    new System.String(Array.ofList list)

let Tokenize (text: char list list) =
    text |> Seq.mapi(fun i x ->
        match x with
        | hd :: tl when Char.IsDigit hd -> Some(Integer(x |> CharToString |> Int32.Parse))
        | ['+'] | ['-']-> Some(Operator(x |> CharToString))
//        | '\n' when i = text.Length-1 -> Some(Eof)
        | _ -> None
    )

let Interpret tokens =
    let filteredTokens = tokens |> Seq.filter (fun x -> x <> None)

    match filteredTokens |> Array.ofSeq with
    | [| Some(Integer(x)); Some(Operator("+")); Some(Integer(y)); |] -> Some(x + y)
    | [| Some(Integer(x)); Some(Operator("-")); Some(Integer(y)); |] -> Some(x - y)
    | _ -> None

let PreprocessInput (text: string) =
    let breakIntoDigitGroups el (acc : char list list) =
        match el with
        | _ when System.Char.IsDigit el -> (el :: List.head acc) :: (List.tail acc)
        | _ -> [] :: [[el]] @ acc
        
    List.foldBack breakIntoDigitGroups (List.ofSeq text) [ [] ]

[<EntryPoint>]
let main argv = 
    while true
        do
            printf "Calc> "
            let input = System.Console.ReadLine()
            let result = input |> PreprocessInput |> Tokenize |> Interpret
            match result with
            | Some result -> printfn "Result: %i" result
            | None -> printfn "Could not parse, try again"
    0 // return an integer exit code
