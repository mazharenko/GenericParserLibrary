module ParserLibrary

open System
open System.Collections.Generic

type IParserToken = 
    interface
    end

type IParserPosition = 
    interface
    end

type IInputState = 
    interface
        abstract Position : IParserPosition
        abstract Next : unit -> IParserToken option * IInputState
    end

type ParserLabel = string

type ParserError = string

// Result type
type Result<'a> = 
    | Success of 'a
    | Failure of ParserLabel * ParserError * IParserPosition
    
let (|Succeded|Failed|) result : Choice<'a, ParserLabel * ParserError * IParserPosition> = 
    match result with 
    | Success (res) -> Succeded res
    | Failure (label, error, position) -> Failed (label, error, position)

/// A Parser structure has a parsing function & label
type Parser<'a> = 
    { parseFn : IInputState -> Result<'a * IInputState>
      label : ParserLabel }

// =============================================
// Label related
// =============================================
/// get the label from a parser
let getLabel parser = 
    // get label
    parser.label

/// update the label in the parser
let setLabel parser newLabel = 
    // change the inner function to use the new label
    let newInnerFn input = 
        let result = parser.parseFn input
        match result with
        | Success s -> 
            // if Success, do nothing
            Success s
        | Failure(oldLabel, err, pos) -> 
            // if Failure, return new label
            Failure(newLabel, err, pos)
    { // return the Parser
      parseFn = newInnerFn
      label = newLabel }

/// infix version of setLabel
let (<//>) = setLabel

// Read a token from an input 
let read = 
    let label = "unknown"
    
    let innerFn (input : IInputState) = 
        let tokenOpt, remainingInput = input.Next()
        match tokenOpt with
        | None -> 
            let err = "No more input"
            let pos = remainingInput.Position
            Failure(label, err, pos)
        | Some token -> Success(token, remainingInput)
    { // return the parser
      parseFn = innerFn
      label = label }

let condition parser predicate = 
    let label = parser.label
    
    let innerFn input = 
        let result = parser.parseFn input
        match result with
        | Succeded (token, state) -> 
            if predicate token then Success(token, state)
            else 
                let err = sprintf "Unexpected '%O'" token
                let pos = input.Position
                Failure(label, err, pos)
        | Failed fail -> 
            // if Failure, do nothing
            Failure fail
    { parseFn = innerFn
      label = label }

let (<?>) = condition

/// Run the parser on a InputState
let runOnInput parser input = 
    // call inner function with input
    parser.parseFn input

/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p = 
    let label = "unknown"
    
    let innerFn input = 
        let result1 = runOnInput p input
        match result1 with
        | Failed fail -> 
            // return error from parser1
            Failure fail
        | Succeded (value1, remainingInput) -> 
            // apply f to get a new parser
            let p2 = f value1
            // run parser with remaining input
            runOnInput p2 remainingInput
    { parseFn = innerFn
      label = label }

/// Infix version of bindP
let (>>=) p f = bindP f p

/// Lift a value to a Parser
let returnP x = 
    let label = sprintf "%A" x
    let innerFn input = 
        // ignore the input and return x
        Success(x, input)
    { // return the inner function
      parseFn = innerFn
      label = label }

/// apply a function to the value inside a parser
let mapP f = bindP (f >> returnP)

/// infix version of mapP
let (<!>) = mapP

/// "piping" version of mapP
let (|>>) x f = mapP f x

/// apply a wrapped function to a wrapped value
let applyP fP xP = fP >>= (fun f -> xP >>= (fun x -> returnP (f x)))

/// infix version of apply
let (<*>) = applyP

/// lift a two parameter function to Parser World
let lift2 f xP yP = returnP f <*> xP <*> yP

/// Combine two parsers as "A andThen B"
let andThen p1 p2 = 
    let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
    p1
    >>= (fun p1Result -> p2 >>= (fun p2Result -> returnP (p1Result, p2Result)))
    <//> label

/// Infix version of andThen
let (.>>.) = andThen

/// Combine two parsers as "A orElse B"
let orElse p1 p2 = 
    let label = sprintf "%s orElse %s" (getLabel p1) (getLabel p2)
    
    let innerFn input = 
        // run parser1 with the input
        let result1 = runOnInput p1 input
        // test the result for Failure/Success
        match result1 with
        | Success result -> 
            // if success, return the original result
            result1
        | Failure _ -> 
            // if failed, run parser2 with the input
            let result2 = runOnInput p2 input
            // return parser2's result
            result2
    { // return the inner function
      parseFn = innerFn
      label = label }

/// Infix version of orElse
let (<|>) = orElse

/// Choose any of a list of parsers
let choice listOfParsers = List.reduce (<|>) listOfParsers

let rec sequence parserList = 
    // define the "cons" function, which is a two parameter function
    let cons head tail = head :: tail
    // lift it to Parser World
    let consP = lift2 cons
    // process the list of parsers recursively
    match parserList with
    | [] -> returnP []
    | head :: tail -> consP head (sequence tail)

/// Tries every parser like choice; if succeeds, tries the rest until every fails or succeds 
let all (listOfParsers : Parser<'a> list) = 
    let enrich parser = (fun x -> (parser, x)) <!> parser
    
    let rec innerFn (parsers : Parser<'a> list) input = 
        let enriched = parsers |> List.map enrich
        let result1 = runOnInput (choice enriched) input
        match result1 with
        | Failed failure -> 
            // return error from parser choice
            Failure failure
        | Succeded ((parserSucceeded, value), remainingInput) -> 
            // filter original parsers list
            let newParsers = 
                parsers |> List.filter (fun p -> not (LanguagePrimitives.PhysicalEquality p parserSucceeded))
            // run remaining parsers
            match newParsers with
            | [] -> Success([ value ], remainingInput)
            | _ -> 
                match innerFn newParsers remainingInput with
                | Failure(label, err, pos) -> Failure(label, err, pos)
                | Success(nextResult, remainingInput2) -> Success(value :: nextResult, remainingInput2)
    { parseFn = innerFn listOfParsers
      label = "parsers list" }

/// (helper) match zero or more occurences of the specified parser
let rec parseZeroOrMore parser input = 
    // run parser with the input
    let firstResult = runOnInput parser input
    // test the result for Failure/Success
    match firstResult with
    | Failed _ -> ([], 
                           // if parse fails, return empty list
                           input)
    | Succeded (firstValue, inputAfterFirstParse) -> 
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue :: subsequentValues
        (values, remainingInput)

/// matches zero or more occurences of the specified parser
let many parser = 
    let label = sprintf "many %s" (getLabel parser)
    let rec innerFn input = 
        // parse the input -- wrap in Success as it always succeeds
        Success(parseZeroOrMore parser input)
    { parseFn = innerFn
      label = label }

/// matches one or more occurences of the specified parser
let many1 p = 
    let label = sprintf "many1 %s" (getLabel p)
    List.Cons <!> (p .>>. many p) <//> label

/// Parses an optional occurrence of p and returns an option value.
let opt p = 
    let label = sprintf "opt %s" (getLabel p)
    let some = p |>> Some
    let none = returnP None
    (some <|> none) <//> label
    
/// Keep only the result of the left side parser
let (.>>) p1 p2 = 
    // create a pair
    p1 .>>. p2 // then only keep the first value
               |> mapP (fun (a, b) -> a)

/// Keep only the result of the right side parser
let (>>.) p1 p2 = 
    // create a pair
    p1 .>>. p2 // then only keep the second value
               |> mapP (fun (a, b) -> b)

/// Keep only the result of the middle parser
let between p1 p2 p3 = p1 >>. p2 .>> p3

/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p            
    p .>>. many sepThenP 
    |>> fun (p,pList) -> p::pList

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []

let createParserForwardedToRef<'a>() = 
    let dummyParser = 
        let innerFn input : Result<'a * IInputState> = failwith "unfixed forwarded parser"
        { parseFn = innerFn
          label = "unknown" }
    
    // ref to placeholder Parser
    let parserRef = ref dummyParser
    // wrapper Parser
    let innerFn input = 
        // forward input to the placeholder
        runOnInput !parserRef input
    
    let wrapperParser = 
        { parseFn = innerFn
          label = "unknown" }
    wrapperParser, parserRef
