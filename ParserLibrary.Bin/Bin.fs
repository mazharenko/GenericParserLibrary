namespace ParserLibrary

open ParserLibrary.Generic

module Bin = 
    type ByteToken(value : byte) = 
        struct
            interface IParserToken
            member __.Value = value
            override __.ToString() = sprintf "<%i>" value
            static member op_Explicit (token : ByteToken) : byte = token.Value
        end
    
    type ByteParserPosition = 
        struct
            val mutable private position : int option
            private new(position) = { position = position }
            interface IParserPosition
            member __.ByteNumber = __.position
            static member Create() = ByteParserPosition(None)
            static member Create(position : int) = ByteParserPosition(Some position)
            static member (+) (p : ByteParserPosition, shift : int) = 
                match shift with
                | num when num < 0 -> invalidArg "shift" "negative shift is not supported"
                | 0 -> p
                | _ -> 
                    match p.ByteNumber with
                    | None -> shift - 1
                    | Some num -> shift + num
                    |> ByteParserPosition.Create
        end
    
    let PackByte = ByteToken >> (fun x -> x :> IParserToken)
    
    let UnpackByte(token : IParserToken) = 
        match token with
        | :? ByteToken as byteToken -> byteToken.Value
        | _ -> invalidArg "token" "only ByteToken is supported"
    
    // TODO: make it possible to read from stream
    type ByteInputState(bytes : byte [], position : ByteParserPosition) = 
        class
            new(bytes) = ByteInputState(bytes, ByteParserPosition.Create())
            interface IInputState with
                member __.Position = position :> _
                member __.Next() = 
                    let nextPosition = position + 1
                    match bytes.Length > nextPosition.ByteNumber.Value with
                    | true -> 
                        let token = 
                            (bytes, nextPosition.ByteNumber.Value)
                            ||> Array.get
                            |> PackByte
                            |> Some
                        token, ByteInputState(bytes, nextPosition) :> _
                    | false -> None, __ :> _
        end
    
    module Parsers = 
        let pbyte (byteToMatch : byte) = 
            // label is just the character
            let label = sprintf "%o" byteToMatch
            let predicate ch = (ch = byteToMatch)
            read <?> (UnpackByte >> predicate) <//> label
    
    let run parser bytes = ByteInputState bytes |> runOnInput parser
