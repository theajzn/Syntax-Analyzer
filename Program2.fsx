type Token =
    |ID of string 
    
    

    |ID of string
    |ASSIGN_STMT
    |ELSE_STMT
    | IF_STMT
    | END_IF_STMT
    |ENDWHILE_STMT
    |WHILE_STMT
    |THEN_STMT
    |DONE_STMT
    |STEP_STMT
    | DO_STMT
    | TO_STMT
    | FOR_STMT
    | OPER_OP of string
    | MULTIPLICATION_OP
    | ADDITION_OP
    | ID_stmt 
    | OPEN_PARENTHESIS
    | CLOSE_PARENTHESIS
    | READ of string 
    | WRITE of string 
    | NUMBER



    with static member tokenFromLexeme str =
        match str with
        
            | x->ID x
            |":="ASSIGN_STMT
            |"ELSE"->ELSE_STMT
            |"FI"->END_IF_STMT
            | "IF" |  -> IF_STMT
            |"OD"->ENDWHILE_STMT
            |"WHILE"->WHILE_STMT
            |"THEN"->THEN_STMT
            |"DONE"DONE_STMT
            |"STEP"->STEP_STMT
            | "DO" -> DO_STMT
            |"TO"->TO_STMT
            |"FOR"->FOR_STMT
            |"<"|">"|"="|->OPER_OP str
            |"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|->NUMBER
            |"*"->MULTIPLICATION_OP
            |"("->OPEN_PARENTHESIS
            |")"->CLOSE_PARENTHESIS
            |"+"->ADDITION_OP
            |"WRITE"->WRITE
            |"READ"->READ


let rec parse theList = PROGRAM theList

// <PROGRAM> : <STMT_LIST>
and PROGRAM lst = STMT_LIST


// <STMT_LIST> ::= CONJUNCTION <sentence> | EOS
and STMT_LIST list = 
    match list with 
    | ID x :: ASSIGN_STMT :: _ -> list |> STMT
    | READ :: _ -> list |> STMT
    | WRITE :: _ -> list |> STMT
    | xs -> xs 
      


// <STMT> ::= 
and STMT list=
   match list with
    | ID x:: ASSIGN_STMT ::xs -> xs |> EXPR 
    |READ:: xs->xs |> ID
    |WRITE:: xs->xs |> EXPR
    |IF_STMT::xs->xs |> IF_STMT
    |FOR_STMT::xs->xs |> FOR_STMT
    | _ -> failwithf $"not valid: %A{list}"
    


// <EXPR> ::= TERM TERM_TAIL
and EXPR =function

    |TERM :: xs->xs|>TERM_TAIL
    |[]-> failwith"empty"
    |x-> failwith $" expected expr, but found:%A(x)"



    

// <TERM_TAIL> ADDITION_OP TERM TERM_TAIL| ε
and TERM_TAIL =
    function
    | ADDITION_OP :: xs -> xs |> TERM|> TERM_TAIL
    | xs -> xs // <Empty> is permitted
    |_-> failwith"none added"




    // <TERM>  FACTOR FACTOR_TAIL
and TERM =
     |FACTOR :: xs -> xs |> FACTOR_TAIL
     




// <FACTOR_TAIL> ::=  MULTIPLICATION_OP FACTOR FACTOR_TAIL|EMPTY
and FACTOR_TAIL =
    function
    | MULTIPLICATION_OP :: xs -> xs|> FACTOR|>FACTOR_TAIL
    | xs->xs// empty
    
    


    //<FACTOR> ( EXPR)
    and FACTOR=
        function
        |OPEN_PARENTHESIS:: EXPR:: CLOSE_PARENTHESIS:: xs->xs
        | ID:: xs->xs
        |NUMBER::xs->xs
        |x::xs->" NUMBER OR ID EXPECTED BUT FOUND: %A "x
        |[]->failwith" input ended"



and ADDITION_OP =
    function       
| ADDITION_OP :: xs -> xs
| x :: xs -> failwithf "Expected add operator but found: %A" x
|_ -> failwith "must be addition or subtraction "


and MULTIPLICATION_OP =
    function 
| MULTIPLICATION_OP :: xs -> xs
| x :: xs -> failwithf "Expected multiplacatioon operator but found: %A" x
| [] -> failwith "multiplication operator should not be empty"



and OPEN_PARENTHESIS =
    function
| OPEN_PARENTHESIS :: xs -> xs
| x -> failwithf "Expected open parenthesis, but found: %A" x



and CLOSE_PARENTHESIS =
    function
| CLOSE_PARENTHESIS :: xs -> xs
| x -> failwithf "Expected close parenthesis, but found: %A" x

and OPER_OP=
    function
    OPER_OP_::xs->xs
    |_->failwith"not an operation"


 and CONDITION list=
        list|>EXPR|>EXPR|>EXPR


 and FOR_STMT
        function
        |FOR_STMT::ID x::OPER_OP"="::ID s::TO_STMT::ID t ::DO_STMT::xs->xs|>STMT_LIST|>CHECKFORDONE
        |FOR_STMT::ID x::OPER_OP"="::ID s::TO_STMT::ID t ::STEP_STMT::ID u ::DO_STMT::xs->xs|>STMT_LIST|>CHECKFORDONE


and CHECKFORDONE=
    function
    |DONE_STMT::xs->xs
    |_->failwith"NOT  STMT"



and STEP_STMT
function
|STEP_STMT::ID x::xs->xs
|_->failwith"NOT STMT"


and IF_STMT
function
|IF_STMT::xs->xs|>IF_STMT_TAIL|> CONDITION
|_->failwith"NOT DONE STMT"



and IF_STMT_TAIL
function
|THEN_STMT::xs->xs|> ELSE_STMT|>STMT_LIST
|_->failwith"NOT IF STMT"


and ELSE_STMT
function
|>ELSE_STMT::xs->xs|>STMT_LIST|>ELSE_STMT_TAIL


and |>ELSE_STMT_TAIL
function
 END_IF_STMT:: xs->xs
 |_ -> failwith"NOT ELSE STMT"

 this is where nndami stopped


 ## EVERYThing below is part of then old file and it maybe used





  

(* Get the user input and start parsing *)

// NOTE: To make the let assignment be a function that accepts no parameters,
// an "empty tuple" must be accepted in ocaml and older version of F#, ie. "let promptAndGo () = "
let promptAndGo (): unit =
    (* Begin Parsing Process *)
    let startParsing (str: string) = // explicit tye is necessary because Split works with a few types and actual type is otherwise not yet known.
        // Split the String (which creates an Array) -> convert the Array to a List -> MAP the list of strings into a list of Tokens.
        // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
        let tokenList = str.Split " " |> Array.toList |> List.map Token.tokenFromLexeme

        // Display our list of tokens...
        printfn $"\nInitial String: %s{str}\nToken List: %A{tokenList}\n"

        // Work the magic...
        try
            let parsedList = parse tokenList in printfn $"The Final List:\n\t%A{parsedList}"
            if (parsedList.Length > 0) then
                printfn $"Parsing Failed becuase we have extra tokens! %A{parsedList}"
            else 
                printfn "Parsing was Successful!"
                
        with Failure msg ->
            printfn $"Error: %s{msg}"

    let getUserInput =
        printf "Enter String: "
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.
        System.Console.ReadLine()

     in

    startParsing getUserInput |> ignore



(* EXAMPLE TEST DATA
the small , slow dog quietly chases the fast cat up a tall tree .
*)
promptAndGo ()

// Uncomment the following to pause at the end if (and only if) it's running in a terminal which dissapears upon running.
// printfn "Press a key to Continue."; System.Console.ReadKey(true) |> ignore
