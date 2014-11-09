open System
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#r "bin\Debug\FParsecCS.dll"
#r "bin\Debug\FParsec.dll"

open FParsec

type Json = JString of string
            | JNumber of float
            | JBool of bool
            | JNull
            | JList of Json list
            | JObject of Map<string, Json>


let value p str =
    match run p str with
    | Success(r, a, b) -> Some r
    | Failure(e, a, b) -> None

let test p str =
    match run p str with
    | Success(r, _, _) -> printfn "Sucess: %A" r
    | Failure(e,_,_) -> printfn "Error: %s" e

let json = 
    let str = pstring
    let ws = spaces

    let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()

    let jnull = stringReturn "null" JNull
    let jbool = (stringReturn "true" true <|> stringReturn "false" false) |>> JBool
    let jnumber = pfloat |>> JNumber

    let stringLiteral =
        let escape =
            anyOf "\"\\/bfnrt"
            |>> function
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c
        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (pstring "\"") (pstring "\"")
                (stringsSepBy normalCharSnippet escapedCharSnippet)
    let listBetweenStrings sOpen sClose pElement f =
        between (str sOpen) (str sClose)
                (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)
    let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)

    let jstring = stringLiteral |>> JString
    let jlist = listBetweenStrings "[" "]" jvalue JList
    let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)
    do jvalueRef := choice [jobject; jlist; jstring; jnumber; jbool; jnull]
    ws >>. jvalue .>> ws .>> eof

let opts =
    let isAsciiIdStart c =
        isAsciiLetter c || c = '_'

    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_'

    IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                      isAsciiIdContinue = isAsciiIdContinue)

let ident = identifier opts

run ident "1hello"

let hej = value json """
[
  {
    "_id": "54562ff7688fa4a5a9466479",
    "index": 0,
    "guid": "ca966cc3-755c-4b24-a2a4-eae161c3d3bb",
    "isActive": false,
    "balance": "$1,177.46",
    "picture": "http://placehold.it/32x32",
    "age": 23,
    "eyeColor": "blue",
    "name": "Snider Guzman",
    "gender": "male",
    "company": "BULLJUICE",
    "email": "sniderguzman@bulljuice.com",
    "phone": "+1 (942) 502-3804",
    "address": "230 Maple Street, Canby, Guam, 5709",
    "about": "Minim tempor anim et cillum. Cupidatat nulla eiusmod officia reprehenderit. Sint aliquip do consequat non sunt. Non exercitation quis veniam consequat irure laboris esse nulla. Excepteur cillum fugiat reprehenderit consequat ea id.\r\n",
    "registered": "2014-06-23T21:16:39 -02:00",
    "latitude": -25.173557,
    "longitude": -161.114685,
    "tags": [
      "id",
      "Lorem",
      "eiusmod",
      "exercitation",
      "aute",
      "aliquip",
      "ea"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Leigh Lowery"
      },
      {
        "id": 1,
        "name": "Adriana Stewart"
      },
      {
        "id": 2,
        "name": "Beard Brooks"
      }
    ],
    "greeting": "Hello, Snider Guzman! You have 4 unread messages.",
    "favoriteFruit": "apple"
  },
  {
    "_id": "54562ff70d62dcc3dfa28516",
    "index": 1,
    "guid": "2a7f5364-cb63-4f54-a202-cb1fe34a399c",
    "isActive": true,
    "balance": "$1,451.42",
    "picture": "http://placehold.it/32x32",
    "age": 40,
    "eyeColor": "blue",
    "name": "Roman York",
    "gender": "male",
    "company": "SLUMBERIA",
    "email": "romanyork@slumberia.com",
    "phone": "+1 (902) 580-2656",
    "address": "574 Montague Terrace, Clarence, Massachusetts, 8420",
    "about": "Consectetur sint esse est sint ex nulla eiusmod aliqua consectetur Lorem duis ad aliquip. Consequat nulla velit laborum deserunt sint labore. Dolor aute in aliquip officia voluptate adipisicing minim elit ea velit sunt ex excepteur.\r\n",
    "registered": "2014-07-12T23:24:50 -02:00",
    "latitude": -49.323052,
    "longitude": 21.262089,
    "tags": [
      "nulla",
      "exercitation",
      "nostrud",
      "labore",
      "velit",
      "consequat",
      "et"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Greene Alvarez"
      },
      {
        "id": 1,
        "name": "Janelle Chambers"
      },
      {
        "id": 2,
        "name": "Madge Abbott"
      }
    ],
    "greeting": "Hello, Roman York! You have 7 unread messages.",
    "favoriteFruit": "strawberry"
  },
  {
    "_id": "54562ff7bba183ca4cd043a7",
    "index": 2,
    "guid": "b750e362-fd64-431a-bb2b-2c58a30bf0ba",
    "isActive": false,
    "balance": "$3,614.83",
    "picture": "http://placehold.it/32x32",
    "age": 40,
    "eyeColor": "brown",
    "name": "Ida Yang",
    "gender": "female",
    "company": "XOGGLE",
    "email": "idayang@xoggle.com",
    "phone": "+1 (855) 577-3605",
    "address": "242 Vine Street, Corinne, Wyoming, 7419",
    "about": "Laborum officia est duis aute irure occaecat et. Consectetur laboris Lorem officia consequat occaecat ipsum elit. Deserunt laborum enim incididunt amet laborum incididunt Lorem occaecat. Voluptate sit qui enim consequat tempor deserunt consectetur eiusmod duis anim adipisicing labore. Nisi labore culpa velit quis mollit excepteur aliquip. Aliqua ex ullamco nulla elit mollit consequat nulla.\r\n",
    "registered": "2014-05-11T12:29:35 -02:00",
    "latitude": 57.113534,
    "longitude": 64.578715,
    "tags": [
      "consectetur",
      "esse",
      "exercitation",
      "aute",
      "exercitation",
      "ad",
      "sunt"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Harvey Leblanc"
      },
      {
        "id": 1,
        "name": "Bentley West"
      },
      {
        "id": 2,
        "name": "Christian James"
      }
    ],
    "greeting": "Hello, Ida Yang! You have 10 unread messages.",
    "favoriteFruit": "apple"
  },
  {
    "_id": "54562ff73d2ea9badbb98d3d",
    "index": 3,
    "guid": "94e6da3b-d583-45f2-bbcc-b7fd53232db4",
    "isActive": false,
    "balance": "$1,473.02",
    "picture": "http://placehold.it/32x32",
    "age": 30,
    "eyeColor": "blue",
    "name": "Ryan Hancock",
    "gender": "male",
    "company": "EVEREST",
    "email": "ryanhancock@everest.com",
    "phone": "+1 (854) 581-2378",
    "address": "415 Belmont Avenue, Blandburg, Vermont, 3005",
    "about": "Magna ut anim excepteur dolor excepteur do. Officia exercitation non eu ex in laborum exercitation id sint. Incididunt sint exercitation reprehenderit pariatur incididunt irure ut qui quis ipsum velit. Fugiat duis tempor dolore consequat esse enim cupidatat do. Culpa incididunt occaecat aliqua amet culpa deserunt do velit sunt eu ea commodo nostrud do. Eiusmod adipisicing nostrud ad cillum aliquip laborum velit aliquip deserunt enim irure quis consequat.\r\n",
    "registered": "2014-05-18T16:16:02 -02:00",
    "latitude": -54.83342,
    "longitude": 148.031257,
    "tags": [
      "elit",
      "magna",
      "velit",
      "commodo",
      "occaecat",
      "ea",
      "Lorem"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Hallie Vega"
      },
      {
        "id": 1,
        "name": "Beatrice Bray"
      },
      {
        "id": 2,
        "name": "Petty Frank"
      }
    ],
    "greeting": "Hello, Ryan Hancock! You have 4 unread messages.",
    "favoriteFruit": "banana"
  },
  {
    "_id": "54562ff7cae79af43e563cf7",
    "index": 4,
    "guid": "b542aec4-77d1-4621-bb1d-e91b6a3c9159",
    "isActive": true,
    "balance": "$3,769.69",
    "picture": "http://placehold.it/32x32",
    "age": 27,
    "eyeColor": "brown",
    "name": "Deanne Wynn",
    "gender": "female",
    "company": "EBIDCO",
    "email": "deannewynn@ebidco.com",
    "phone": "+1 (935) 588-3390",
    "address": "923 Central Avenue, Shasta, Ohio, 3828",
    "about": "Cillum enim est consequat veniam veniam aliqua id quis nostrud exercitation incididunt cillum non sit. Mollit nostrud eu proident deserunt et non ad elit elit voluptate eiusmod. Ea laborum ullamco dolore est excepteur ad dolore ipsum occaecat reprehenderit esse ullamco mollit aliquip. Deserunt elit eiusmod in mollit quis laboris amet incididunt in tempor commodo.\r\n",
    "registered": "2014-04-18T14:01:43 -02:00",
    "latitude": 30.768889,
    "longitude": -89.288416,
    "tags": [
      "ex",
      "voluptate",
      "qui",
      "non",
      "pariatur",
      "eu",
      "non"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Sanchez Mccall"
      },
      {
        "id": 1,
        "name": "Mcdonald Wheeler"
      },
      {
        "id": 2,
        "name": "Letitia Bradford"
      }
    ],
    "greeting": "Hello, Deanne Wynn! You have 4 unread messages.",
    "favoriteFruit": "apple"
  }
]
"""

