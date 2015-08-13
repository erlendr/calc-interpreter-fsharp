open System

type Token =
    | Integer of int
    | Plus
    | Eof

let Tokenize (text: string) =
    text |> Seq.mapi(fun i x ->
        match x with
        | x when Char.IsDigit x -> Some(Integer(Int32.Parse (sprintf "%c" x)))
        | '+' -> Some(Plus)
        | '\n' when i = text.Length-1 -> Some(Eof)
        | _ -> None
    )

let Interpret tokens =
    match tokens |> Array.ofSeq with
    | [| Some(Integer(x)); Some(Plus); Some(Integer(y)); |] -> Some(x + y)
    | _ -> None

[<EntryPoint>]
let main argv = 
    while true
        do
            printf "Input x+y expression: "
            let input = System.Console.ReadLine()
            let result = input |> Tokenize |> Interpret
            match result with
            | Some result -> printfn "Result: %i" result
            | None -> printfn "Could not parse, try again"
    0 // return an integer exit code
