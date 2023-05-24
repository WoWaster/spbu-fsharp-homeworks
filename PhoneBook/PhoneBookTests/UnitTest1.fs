module PhoneBookTests

open NUnit.Framework
open FsUnit
open PhoneBook

[<Test>]
let ``Adding record to Nil is correct`` () =
    Util.addRecord [] "name" "123"
    |> should equal [ { name = "name"; phone = "123" } ]

let records = [ { name = "A"; phone = "123" }; { name = "B"; phone = "456" } ]

let newRecords =
    [ { name = "C"; phone = "789" }
      { name = "A"; phone = "123" }
      { name = "B"; phone = "456" } ]

[<Test>]
let ``Adding record to populated list is correct`` () =
    Util.addRecord records "C" "789" |> should equal newRecords

[<Test>]
let ``findByName with non-existing name should return None`` () =
    Util.findByName records "D" |> should equal None

[<Test>]
let ``findByName with existing name should return Some(phone)`` () =
    Util.findByName records "A" |> should equal (Some { name = "A"; phone = "123" })

[<Test>]
let ``findByPhone with non-existing phone should return None`` () =
    Util.findByPhone records "912" |> should equal None

[<Test>]
let ``findByPhone with existing phone should return Some(phone)`` () =
    Util.findByPhone records "456"
    |> should equal (Some { name = "B"; phone = "456" })

[<Test>]
let ``loadFromFile on non-existing file should return None`` () =
    Util.loadFromFile "non-existing_file.txt" |> should equal None

[<Test>]
let ``Serialization is correct`` () =
    let filename = "records.txt"
    Util.saveToFile records filename
    Util.loadFromFile filename |> should equal (Some records)
