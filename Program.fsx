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
    | ASGN
    | IF
    | THEN
    | ELSE
    | ENDIF
    | WHILE
    | DO
    | DONE
    | UNTIL
    | COND
    | PARENTH
    | ADD
    | MULTIPLY
    | ID of string

    // Member (of the type) function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with
        | "read" -> READ
        | "write" -> WRITE
        | ":=" -> ASGN
        | "+"
        | "-" -> ADD
        | "*"
        | "/" -> MULTIPLY
        | ">"
        | "<"
        | "==" -> COND
        | "("
        | ")" -> PARENTH
        | "if" -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | "endif" -> ENDIF
        | "while" -> WHILE
        | "do" -> DO
        | "done" -> DONE
        | "until" -> UNTIL
        | "," -> COMMA
        | "<-" -> FUNCTION
        | x -> ID x

let matchToken (theExpectedToken: Token) theList =
    match theList with
    // resolve to the rest of the list when head is the expected type.
    | head :: tail when head = theExpectedToken -> tail

    // head of list did not match the expected type, so we don't even care about "the rest" (_)
    | head :: _ -> failwithf $"Wrong Type! Expected %A{theExpectedToken} but found %A{head}"

    // Couldn't match anything!
    | _ -> failwithf $"Nothing to match! Expected a list with a head of type %A{theExpectedToken}"

let degugDisplay msg list =
    printfn $"{msg}\n\tRemaining List is: %A{list}\n"
    list


// NOTE: The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"
let rec parse xs = xs |> program


// <program> ::= <stmt_list> $$
and program xs =
    xs |> stmt_list |> degugDisplay "Program" |> ``$$``


// As "$$" means "Top of Stack", this represents an "Empty List"
// Functions can be named virtually anything if in backticks.  This is a good way to name a function with a symbol.
and ``$$`` =
    function
    | [] ->
        printfn "Top Of Stack!"
        ([]: Token list)
    | reminingElements -> failwithf $"Unprocessed Tokens: {reminingElements}"

//<stmt_list> ::= <stmt> <stmt_list> | ε
and stmt_list lst =
    match lst with
    | (READ | WRITE | IF | DO | WHILE) :: _ -> lst |> stmt |> stmt_list
    | x -> x

// and stmt lst =
//     match lst with
//     | ID _:: xs -> xs |> id_tail
//     |> io_stmnt |> if_stmt |> do_stmt |> while_stmt

//<stmt> ::= ID <id_tail> | <io_stmt> | <if_stmt> | <do_stmt> | <while_stmt>
and stmt xs =
    match xs with
    | ID _ :: remaining -> remaining |> id_tail
    | READ :: ID _ :: remaining -> remaining
    | WRITE :: remaining -> remaining |> expr
    | IF :: _ -> xs |> if_stmt
    | DO :: _ -> xs |> do_stmt
    | WHILE :: _ -> xs |> while_stmt
    | _ -> failwithf $"Invalid statement: {xs}"

//<id_tail> ::= <fun_call> | <assignment>
and id_tail xs =
    match xs with
    | FUNCTION :: remaining -> remaining |> fun_call
    | ASGN :: remaining -> remaining |> assignment
    | _ ->  failwithf $"Invalid id tail: {xs}"

//<assignment> ::= := <expr>
and assignment lst =
    match lst with
    | ASGN :: xs -> xs |> expr
    | _ -> failwithf $"Not a valid assignment statement: {lst}"


//<fun_call> ::= <- ID ( <param_list> )
and fun_call xs =
    match xs with
    | FUNCTION :: remaining -> remaining
    | ID _ :: remaining -> remaining
    | PARENTH :: remaining -> remaining |> param_list |> matchToken PARENTH
    | _ -> failwithf $"Invalid function call: {xs}"

//<expr> ::= ID <expr_tail> | ( <expr> ) <expr_tail>
and expr xs =
    match xs with
    | ID _ :: remaining -> remaining |> expr_tail
    | PARENTH :: remaining -> remaining |> expr |> matchToken PARENTH |> expr_tail
    | _ -> failwithf $"Invalid expression: {xs}"


//<expr_tail> ::= <arith_op> <expr> | ε
and expr_tail lst =
    match lst with
    | (ADD | MULTIPLY) :: _ -> lst |> arith_op |> expr
    | x -> x



//<param_list> ::= <expr> <param_tail>
and param_list xs =
    xs |> expr |> param_tail

//<param_tail> ::= , <expr> <param_tail> | ε
and param_tail xs =
    match xs with
    | COMMA :: remaining -> remaining |> expr |> param_tail
    | x -> x
    

//<if_stmt> ::= IF <cond> THEN <stmt_list> <else_stmt>
and if_stmt =
    function
    | IF :: xs -> xs |> cond_expr |> matchToken THEN |> stmt_list |> else_stmt
    | x :: xs -> failwithf $"Invalid if statement: {xs}"
    | [] -> failwith "Statement should not be empty"

//<cond_expr> ::= <expr> <rel_oper> <expr>
and cond_expr xs =
    match xs with
    | ID _ :: remaining -> remaining |> expr
    | PARENTH :: remaining -> remaining |> expr 
    | COND :: remaining -> remaining |> rel_op
    | _ -> failwithf $"Invalid conditional expression: {xs}"

//<else_stmt> ::= ELSE <stmt_list> ENDIF | ENDIF
and else_stmt xs =
    match xs with
    | ELSE :: remaining -> remaining |> stmt_list |> matchToken ENDIF 
    | ENDIF :: remaining -> remaining
    | _ -> failwithf $"Invalid else statement: {xs}"

//<rel_oper> ::= > | < | ==
and rel_op xs =
    match xs with
    | COND :: xs -> xs
    | _ -> failwithf $"Invalid relational operator: {xs}"

//<while_stmt> ::= WHILE <cond_expr> DO <stmt_list> DONE
and while_stmt xs =
    match xs with
    | WHILE :: remaining -> remaining |> cond_expr |> matchToken DO |> stmt_list |> matchToken DONE
    | _ -> failwith $"Invalid while statement: {xs}"


//<do_stmt> ::= DO <stmt_list> until <cond_expr>
and do_stmt xs =
    match xs with
    | DO :: remaining -> remaining |> stmt_list |> matchToken UNTIL |> cond_expr
        
    | _ -> failwithf $"Invalid do statement: {xs}"

//<read_stmt> ::= READ ID
and read_stmnt lst =
    match lst with
    | READ :: ID _ :: xs -> xs
    | _ -> failwithf $"Not a valid statement: {lst}" // no empty case allowed

//<write_stmt> ::= WRITE <expr>
and write_stmnt lst =
    match lst with
    | WRITE :: xs -> xs |> expr
    | _ -> failwithf $"Not a valid statement: {lst}"

//<io_stmt> ::= <read_stmt> | <write_stmt>
and io_stmnt xs =
    match xs with
    | READ :: xs -> xs  |> read_stmnt 
    | WRITE :: xs -> xs |> write_stmnt
    | _ -> failwithf $"Not a valid statement: {xs}"

//<arith_op> ::= - | + | * | /
and arith_op xs =
    match xs with
    | ADD :: xs -> xs
    | MULTIPLY :: xs -> xs
    | _ -> failwithf $"Invalid arithmetic operator: {xs}"



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
    let getTokenList (str: string) =
        Regex.Split(str.Trim(), "\\s+") |> mapTokens

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
                printfn $"The Sentence %s{str} follows the grammar"

        // If an exception ("failwith") is thrown, display the error message.
        with Failure msg ->
            printfn $"The Sentence %s{str} is incorrect because it is: %s{msg}"

    // Get the user input and start parsing
    let getUserInput () =
        printf "Enter (Space Delimited) String\n=> "

        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.
        System.Console.ReadLine()

    in

    getUserInput () |> startParsing |> ignore // Just ignore the result, as we are just printing results above.


// Execute the main function!
main ()
