open System
open PhoneBook

let help =
    """List of the available commands:
* exit | e | quit | q -- exit from application
* help | h -- print this message
* add | a NAME PHONE -- add new person in phone book
* findByName | fbn NAME -- find a record by a given name
* findByPhone | fbp PHONE -- find a record by a given phone
* print | p -- print all record in phone book
* save | s FILE -- save all records to file
* load | l FILE -- load all records from file
"""

let rec getInput records =
    printf "> "
    let input = Console.ReadLine()
    let inputAsList = Seq.toList (input.Trim().Split())

    match inputAsList with
    | [] -> getInput records
    | command :: args -> parse args records command

and parse args records =
    function
    | ("exit" | "quit" | "e" | "q") -> run Exit records
    | ("help" | "h") -> run Help records
    | ("add" | "a") -> takeTwoArgs Add records args
    | ("findByName" | "fbn") -> takeOneArg FindByName records args
    | ("findByPhone" | "fbp") -> takeOneArg FindByPhone records args
    | ("print" | "p") -> run Print records
    | ("save" | "s") -> takeOneArg Save records args
    | ("load" | "l") -> takeOneArg Load records args
    | _ -> printfn "Unknown command"

and takeOneArg command records =
    function
    | [ arg ] -> run (command arg) records
    | _ ->
        printfn "Invalid arguments"
        getInput records

and takeTwoArgs command records =
    function
    | [ arg1; arg2 ] -> run (command (arg1, arg2)) records
    | _ ->
        printfn "Invalid arguments"
        getInput records

and run command records =
    match command with
    | Exit -> Environment.Exit(0)
    | Help -> printf "%s" help
    | Add(name, phone) -> getInput (Util.addRecord records name phone)
    | FindByName name ->
        match Util.findByName records name with
        | Some record -> printfn "%s" record.phone
        | None -> printfn "No phone associated with this name found"
    | FindByPhone phone ->
        match Util.findByPhone records phone with
        | Some record -> printfn "%s" record.name
        | None -> printfn "No name associated with this phone found"
    | Print -> Util.printRecords records
    | Save filename -> Util.saveToFile records filename
    | Load filename ->
        match Util.loadFromFile filename with
        | Some readRecords -> getInput readRecords
        | None -> printfn "File does not exists"

    getInput records

printfn "Phone Book Application"
printfn "Enter a command or `help` for help"
let _ = getInput []
