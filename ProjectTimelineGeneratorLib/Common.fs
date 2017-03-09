module Common

open System
open System.Globalization
open System.Net.Http
open System.Net.Http.Headers
open System.Xml
open System.Collections
open System.Collections.Generic
open System.Web
open System.Threading
open System.Runtime.Serialization
open System.Runtime.Serialization.Json
open System.Text
open System.IO


let comarg x = ref (box x)

let castAs<'T when 'T : null> (o:obj) = 
  match o with
  | :? 'T as res -> res
  | _ -> null
  
exception InvalidCast of string

let castAsWithException<'T> (o:obj) = 
  match o with
  | :? 'T as res -> res
  | _ -> let errorMessage = "Unable to be cast as " + typedefof<'T>.ToString()
         raise (InvalidCast(errorMessage)) 
  
let EnumeratorToEnumerable<'T when 'T : null> (src : IEnumerator) =
    seq {    
                while src.MoveNext() do 
                    yield castAs<'T>(src.Current) 
        }

let EnumeratorToEnumerableEx<'T> (src : IEnumerator) =
    seq {    
                while src.MoveNext() do 
                    yield castAsWithException<'T>(src.Current) 
        }

// used for serializing to and from Json
let toString = System.Text.Encoding.ASCII.GetString
let toBytes (x : string) = System.Text.Encoding.ASCII.GetBytes x

let serializeJson<'a> (x : 'a) =
    let ser = new DataContractJsonSerializer(typedefof<'a>)
    use stream = new MemoryStream()
    ser.WriteObject(stream, x)
    toString <| stream.ToArray()

let deserializeJson<'a> (json : string) = 
    let ser = new DataContractJsonSerializer(typedefof<'a>)
    use stream = new MemoryStream(toBytes json)
    ser.ReadObject(stream) :?> 'a


// asynchronously execute a RESTful API Get call 
let getAsync<'result> (url : string, name : string, password : string)  = async {
    use client = new HttpClient()

    client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
    client.DefaultRequestHeaders.Authorization <- new AuthenticationHeaderValue(
                        "Basic", Convert.ToBase64String(Encoding.ASCII.GetBytes(String.Format("{0}:{1}", name, password))))
    use! httpResponseMessage = client.GetAsync url |> Async.AwaitTask
    httpResponseMessage.EnsureSuccessStatusCode() |> ignore
    let! x = httpResponseMessage.Content.ReadAsStringAsync() |> Async.AwaitTask
    let qResults = deserializeJson<'result> x;
    return qResults
} 

// return a nullable DateTime by parsing an incoming string
let myDateTryParse dateStr = 
    let refDate = ref DateTime.Now
    let couldParse, refDate = DateTime.TryParse(dateStr)
    if (couldParse) then
        Nullable refDate
    else
        Nullable()

// here are the developers on the R&D team
let developerNames = ["Developer1"; "Developer2";  "Developer3"; "Developer4"; "Developer5"]

