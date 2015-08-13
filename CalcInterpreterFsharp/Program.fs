open System

let INTEGER = "INTEGER"
let PLUS = "PLUS"
let EOF = "EOF"

type Token =
    | Integer of int
    | Plus
    | Eof

let Tokenize (text: string) =
    text |> Seq.mapi(fun i x ->
        let result = 
            match x with
            | x when Char.IsDigit x -> Integer(Int32.Parse (sprintf "%c" x))
            | '+' -> Plus
            | '\n' when i = text.Length-1 -> Eof
            | _ -> failwith "unknown token"
        result
    )

let Interpret tokens =
    match tokens |> Array.ofSeq with
    | [| 
        Integer(x);
        Plus;
        Integer(y);
      |] -> x + y
    | _ -> failwith "unknown syntax, not x+y"

[<EntryPoint>]
let main argv = 
    while true
        do
            printf "Input x+y expression: "
            let input = System.Console.ReadLine()
            let result = input |> Tokenize |> Interpret
            printfn "Result: %i" result
    0 // return an integer exit code
