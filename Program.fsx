(*
<program> ::= <stmt_list> $$
<stmt_list> ::= <stmt> <stmt_list> | ε
<stmt> ::= ID <id_tail> | <io_stmt> | <if_stmt> | <do_stmt> | <while_stmt>
<io_stmt> ::= <read_stmt> | <write_stmt>
<id_tail> ::= <fun_call> | <assignment>
<expr> ::= ID <expr_tail> | ( <expr> ) <expr_tail>
<expr_tail> ::= <arith_op> <expr> | ε
<arith_op> ::= - | + | * | /
<rel_oper> ::= > | < | ==
<cond> ::= <expr> <rel_oper> <expr>
<assignment> ::= := <expr>
<read_stmt> ::= READ ID
<write_stmt> ::= WRITE <expr>
<if_stmt> ::= IF <cond> THEN <stmt_list> <else_stmt>
<else_stmt> ::= ELSE <stmt_list> ENDIF | ENDIF
<fun_call> ::= <- ID ( <param_list> )
<param_list> ::= <expr> <param_tail>
<param_tail> ::= , <expr> <param_tail> | ε
<while_stmt> ::= WHILE <cond> DO <stmt_list> DONE
<do_stmt> ::= DO <stmt_list> until <cond>
ID ::= <any token>
*)
// Token Type  (Same as the "Lexer" enum in the Java version)
// !!! Remember "tokens" are "terminals"  ***NOT*** "productions"
//     Terminals are represented by Tokens/Types, productions are represented by functions.
type Token =
    | READ
    | WRITE
    | COMMA
    | FUNCTION
    | ASGN | IF | THEN | ELSE | ENDIF | WHILE | DO | DONE | COND | PARENTH | ADD | MULTIPLY 
    | ID of string

    // Member (of the type) function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with 
            |"read" -> READ
            | "write" -> WRITE
            | ":=" -> ASGN
            | "+" | "-" -> ADD
            | "*" | "/" -> MULTIPLY
            | ">" | "<" | "==" -> COND
            | "(" | ")" -> PARENTH
            | "if" -> IF
            | "then" -> THEN
            | "else" -> ELSE
            | "endif" -> ENDIF
            | "while" -> WHILE
            | "do" -> DO
            | "done" -> DONE
            | "," -> COMMA
            | "<-" -> FUNCTION
            | x -> ID x

 

let degugDisplay msg list = 
    printfn $"{msg}\n\tRemaining List is: %A{list}\n"
    list


// NOTE: The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"
let rec parse xs = 
    xs |> program


// <program> ::= <stmt_list> $$
and program xs = 
    xs |> read_stmnt |> degugDisplay "Program"|> ``$$``


// As "$$" means "Top of Stack", this represents an "Empty List"
// Functions can be named virtually anything if in backticks.  This is a good way to name a function with a symbol.
and ``$$`` =
    function
    | [] -> printfn "Top Of Stack!"; ([] : Token list)
    | reminingElements -> failwithf $"Unprocessed Tokens: {reminingElements}"

and read_stmnt lst = 
    match lst with
    | READ :: ID _ :: xs -> xs 
    | _ -> failwithf $"Not a valid statement: {lst}" // no empty case allowed
    
and write_stmnt lst = 
    match lst with
    | WRITE :: xs   -> xs |> expr

and expr lst =
    match lst with
    | 






(* **********************************************************************************************
   YOU MAY LEAVE THE FOLLOWING CODE AS IS.  IT IS NOT NECESSARY TO MODIFY IT FOR THIS ASSIGNMENT.
   *********************************************************************************************** *)

(* Get the user input and start parsing *)
open System.Text.RegularExpressions

// NOTE: To make the let assignment be a function that accepts no parameters,
// an "empty tuple" must be accepted in ML/SML/OCaml/F#.
let main () =

    // Convert a list of stings to Tokens:
    //    Split the String (which creates an Array)
    //             -> convert the Array to a List
    //             -> MAP the list of strings into a list of Tokens.
    //
    // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
    
    // 'mapTokens' is mainly it's own function as an example of the ">>" operator.
    let mapTokens = Array.toList >> List.map Token.tokenFromLexeme  

    // This is very ".NET" specific. Split is part of the .NET API.        
    let getTokenList (str: string) = Regex.Split(str.Trim(), "\\s+") |> mapTokens

    (* Begin Parsing Process *)
    let startParsing str =
        // Display our list of tokens...
        printfn $"\nInitial String: %s{str}"

        // Try to parse the list of tokens and display the results.
        try
            let tokenList = getTokenList str
            printfn $"Tokens Before Parsing: %A{tokenList}"
            let parsedList = parse tokenList

            if (parsedList.Length > 0) then
                printfn $"Parsing Failed becuase we have extra tokens! %A{parsedList}"
                printfn $"Extra Tokens:\t%A{parsedList}"
            else
                printfn "Done!"

        // If an exception ("failwith") is thrown, display the error message.
        with Failure msg ->
            printfn $"Error: %s{msg}"

    // Get the user input and start parsing
    let getUserInput () =
        printf "Enter (Space Delimited) String\n=> "
        
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.
        System.Console.ReadLine()

    in

    getUserInput () |>
        startParsing |>
        ignore  // Just ignore the result, as we are just printing results above.



(* EXAMPLE TEST DATA
    the small , slow dog quietly chases the fast cat up a tall tree .
*)

// Execute the main function!
main ()