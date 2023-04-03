namespace PhoneBook

type CliCommand =
    | Exit
    | Help
    | Add of string * string
    | FindByName of string
    | FindByPhone of string
    | Print
    | Save of string
    | Load of string
