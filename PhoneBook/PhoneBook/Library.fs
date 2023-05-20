namespace PhoneBook

open System.IO
open System.Text.Json


type PhoneRecord = { name: string; phone: string }

module Util =
    let addRecord records name phone =
        { name = name; phone = phone } :: records

    let printRecords records =
        records
        |> List.sort
        |> Seq.iter (fun record -> printfn "%s: %s" record.name record.phone)

    let findByName records name =
        records |> Seq.tryFind (fun record -> record.name = name)

    let findByPhone records phone =
        records |> Seq.tryFind (fun record -> record.phone = phone)

    let saveToFile records path =
        let json = JsonSerializer.Serialize(records)
        File.WriteAllText(path, json)

    let loadFromFile path =
        match File.Exists path with
        | false -> None
        | true ->
            let json = File.ReadAllText(path)
            let records = JsonSerializer.Deserialize<PhoneRecord list>(json)
            Some records
