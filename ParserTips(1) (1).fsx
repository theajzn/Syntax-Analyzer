(*** 
        !!!!IMPORTANT!!!!

        This is *NOT* a full implementation of the grammar... 
        or even the same grammar as in the assignment.
        IT IS FOR ILLUSTRATION PURPOSES ONLY!!!
    
        !!!!IMPORTANT!!!!               
 ***)

// Sample Grammar.
(* 
   <program> ::= <stmt_list> $$
   <stmt_list> ::= <stmt> <stmt_list> | ε
   <stmt> ::= id := <expr> | read id | write <expr> | <for_stmt>
   <term_tail> ::=  <AddOp> <term> <term_tail> | ε
   <expr> ::= <term> <term_tail>
   <for_stmt> -> for <id> = <id> to <id> <step_stmt> do <stmt_list> done
   <step_stmt> -> ε   // not implemented!
   <term> ::= id 
*)


// Tokens
type Token =
    | READ  (* "read" token *) 
    | WRITE  (* "write" token *)
    | ASGN | FOR | EQL | TO | DO | DONE | ADD | COND  (* etc. *)
    | ID of string    // ID may be any token/string not above/below, but ID is also bound to a string value

    // Function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with
        | "read" -> READ        // "read" maps to the READ token
        | "write" -> WRITE      // "write" maps to the WRITE token
        | ":=" -> ASGN          // ":=" maps to the ASGN token
        | "+" | "-" -> ADD      // "+" and "-" map to the ADD token             
        | ">" | "<" | "==" -> COND // ">" "<" and "=" map to the COND token
        | "=" -> EQL            // "=" maps to the EQL token      
        | x -> ID x            // Everything else is an ID bound to a string value       



(* Example of a function to display debugging strings in the middle of a pipe-line *)
// print the message, then return the list unchanged
let degugDisplay msg list = 
    printfn $"{msg}\n\tRemaining List is: %A{list}\n"
    list


(* Initial Rule *)
// Start parsing by piping the list to the first rule
// TIP: to test a specific rule, just pipe the list to that rule instead of "program".async
let rec parse xs = 
    xs |> program


// <program> ::= <stmt_list> $$
and program xs = 
    xs |> stmt_list |> degugDisplay "Program"|> ``$$``


// As "$$" means "Top of Stack", this represents an "Empty List"
// Functions can be named virtually anything if in backticks.  This is a good way to name a function with a symbol.
and ``$$`` =
    function
    | [] -> printfn "Top Of Stack!"; ([] : Token list)
    | reminingElements -> failwithf $"Unprocessed Tokens: {reminingElements}"


// <stmt_list> ::= <stmt> <stmt_list> | ε
// stmt_list may be empty/epsilon, so no fail case is necessary. (if an epsilon production, the list will be returned unchanged)
and stmt_list lst =
    // Since FIRST(stmt_list) is { WRITE, FOR, IDany of the following, then it is a stmt, otherwise, it is an epsilon production.
    // "_" is a wildcard, so it will match any token. We don't care what it is, just that it is there.
    match lst with
    | WRITE :: _
    | FOR :: _          
    | ID _ :: _   -> lst |> stmt |> stmt_list
    | x -> x // ε case, so just return the list unchanged.


// (* Another Possibility for ther above. *)
// and stmt_list2 = function
//     | ( WRITE | FOR | ID _ ) :: _ as lst -> lst |> stmt |> stmt_list
//     | x -> x // ε case, so just return the list unchanged.


// <stmt> ::= id := <expr> | read id | write <expr> | <for_stmt>
and stmt lst =
    // For debugging!
    printfn $"In stmt rule: The token list  = {lst}" // the is a format specifier will print the whole list

    match lst with
    | ID _ :: ASGN :: xs    -> xs |> expr
    | WRITE :: xs           -> xs |> expr
    | READ :: ID _ :: xs    -> xs

    // This could have been exactly as in the first two cases, but see the for_stmt rule below
    // to see the difference. (This is only so the pattern can match completely in the for_stmt rule)
    | FOR :: _ -> lst |> for_stmt

    | _ -> failwithf $"Not a valid statement: {lst}" // no empty case allowed


// <expr> ::= <term> <term_tail>
and expr lst =
    // Nothing to check, so just follow and implement the rule as is.
    lst |> term |> term_tail


// <term_tail> ::=  <AddOp> <term> <term_tail> | ε
and term_tail =
    function
    | ADD :: xs ->
        printfn $"In term_tail: ADDOP='{xs}'" // Again, for debugging.
        xs |> term |> term_tail
    | x -> x  // ε case, so just return the list unchanged.


// <term> ::= id
and term =
    function
    | ID _ :: xs -> xs
    | _ -> failwithf "Expecting an id"


// for_stmt ->  for <id> = <id> to <id> <step_stmt> do <stmt_list> done
// ALMOST COMPLETE "for_stmt" RULE.
// NOTE: This function does a lot of things in different ways ONLY TO ILLUSTRATE DIFFERENT TECHNIQUES!!!
and for_stmt lst =
    // A nested helper function to match the "DONE" token and return the remaining list.
    let match_DONE =
        function
        | DONE :: xs -> xs
        | _ -> failwith "Expected DONE"

    // All "for loops" are structured as:  "for a = b to c do ... done"
    match lst with
        FOR :: ID a :: EQL :: ID b :: TO :: ID c :: xs

        -> xs 
        |> degugDisplay $"For values: for {a} = {b} to {c}" // Example of using a separate function as part of the pipe
        |> step_stmt
        |> function
            | DO :: doTail -> doTail // Example of making a decision right in the pipe!
            | _ -> failwith "Expected DO"
        |> stmt_list
        |> match_DONE
        
        | _ -> failwith "FOR expected"


// NOT IMPLEMENTED -- FAKE RULE:   (It just resolves to the input list, unchanged)
// This is Just to get the program to compile since "for_stmt" uses the "step_stmt" production)
and step_stmt = function
     | STEP :: ID :: xs -> xs
     | x -> x 





(****** Everything below this point is just for testing the parser. *******)

// Create a new function to parse a list of strings into a list of tokens.
let mapTokens = Array.toList >> List.map Token.tokenFromLexeme  

let getTokenList (str: string) = 
    System.Text.RegularExpressions.Regex.Split(str.Trim(), "\\s+") |> mapTokens

        
(***** Test of the above *****)
// A function to test the parser, in the assignment, this input is read from the console.
try
    // Preevaluated tokens from the lexemes... this is just for testing of the parser, not an implementation of the lexer.
    let pre_tokenized_sample =
            [ WRITE
              ID "x"
              ADD
              ID "y"
              FOR
              ID "i"
              EQL
              ID "1"
              TO
              ID "10"
              DO
              WRITE
              ID "i"
              DONE ]

    printfn $"Result: {parse pre_tokenized_sample}"

with Failure msg -> printfn $"Error: {msg}"
