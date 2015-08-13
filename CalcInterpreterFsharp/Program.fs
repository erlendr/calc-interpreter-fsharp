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

let Interpret (text: string) =
    let pos = 0
    let currentToken = null
    
    0

[<EntryPoint>]
let main argv = 
    let input = System.Console.ReadLine()
    printf "You wrote: %s" input
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
