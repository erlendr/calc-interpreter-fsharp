open System

let INTEGER = "INTEGER"
let PLUS = "PLUS"
let EOF = "EOF"

type Token = {
    Type: string; // token type: INTEGER, PLUS or EOF
    Value: string; // token value: 0-9, '+' or None
}

let TokenToString (token : Token) =
     String.Format("Token({0}, {1})", token.Type, token.Value)

let Tokenize (text: string) =
    text |> Seq.mapi(fun i x ->
        let result = 
            match x with
            | x when Char.IsDigit x -> { Type = INTEGER; Value = (sprintf "%c" x); }
            | '+' -> { Type = PLUS; Value = "+"; }
            | '\n' when i = text.Length-1 -> { Type = EOF; Value = "None"; }
            | _ -> failwith "unknown token"
        result
    )

let Interpret tokens =
    match tokens |> Array.ofSeq with
    | [| 
        {Type = "INTEGER"; Value = x};
        {Type = "PLUS"; };
        {Type = "INTEGER"; Value = y};
      |] -> (Int32.Parse x) + (Int32.Parse y)
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
