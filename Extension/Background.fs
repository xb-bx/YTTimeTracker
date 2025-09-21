module Background
open Browser.IndexedDB
open Browser.IndexedDBExtensions
open Shared
open Fetch
open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Browser.Types
type Temp = { result :IDBDatabase }
type Opts = { autoIncrement: bool; keyPath: string }
let mutable database: IDBDatabase = null


type Config = { youtubeKey: string }

type [<AllowNullLiteral; Global>] BrowserStorage =
    abstract set: keys: obj -> Promise<unit>
    abstract get: key: obj -> Promise<'a>

[<Emit("browser.storage.local")>]
let storage: BrowserStorage = jsNative

(* [<AbstractClass>] *)
type WatchInfo = 
    val mutable videoId: string 
    val mutable channelId: string 
    val mutable watchTime: float 
    val mutable title: string 
    val mutable audioLang: string 
    val mutable timestamp: DateTime
    val mutable id: int  
type WInfo = { videoId: string; channelId: string; watchTime: float; title: string; timestamp: DateTime; audioLang: string }
let cast<'a, 'b> (v: 'b) =
    (v :> obj) :?> 'a

let objGet<'a, 'b> (dbstore: string) (key: 'a): Fable.Core.JS.Promise<'b> =
    let dbstore = (database.transaction ("watches", Readwrite)).objectStore dbstore 
    let res = dbstore.get key

    Promise.Create (fun resolve reject -> 
        res.onsuccess <- (fun (_) -> res.result |> cast |> resolve )
        res.onerror <- (fun (_) -> res.error |> cast |> reject)
    )
let objPut<'a> (dbstore: string) (obj: 'a): Fable.Core.JS.Promise<'a> =
    let dbstore = (database.transaction ("watches", Readwrite)).objectStore dbstore 
    let res = dbstore.put obj
    Promise.Create (fun resolve reject -> 
        res.onsuccess <- (fun (_) -> res.result |> cast |> resolve )
        res.onerror <- (fun (_) -> res.error |> cast |> reject)
    )
let objAdd<'a> (dbstore: string) (obj: 'a): Fable.Core.JS.Promise<'b> =
    let dbstore = (database.transaction ("watches", Readwrite)).objectStore dbstore 
    let res = dbstore.add obj
    Promise.Create (fun resolve reject -> 
        res.onsuccess <- (fun (_) -> res.result |> cast |> resolve )
        res.onerror <- (fun (_) -> res.error |> cast |> reject)
    )

let getVideoLang id apikey: Promise<string> = 
    promise {
        let! res = fetch (sprintf "https://youtube.googleapis.com/youtube/v3/videos?part=snippet%%2CcontentDetails%%2Cstatistics&id=%s&key=%s" id apikey ) []
        let! object = res.json()
        let a = object?items?at(0)?snippet?defaultAudioLanguage |> cast
        return a
    }


(* getVideo *)

let db = indexedDB.``open`` "yttracker"
db.onsuccess <- (fun ev -> 
    let t = ((ev.target :> obj) :?> Temp).result
    database <- t
    runtime.onMessage.addListener (fun (m: Message) -> 
        match m with 
        | SetApiKey key -> 
            console.log key
            storage.set ({ youtubeKey = key })
            Promise.resolve Saved
        | GetApiKey -> 
            promise {
                let! config = storage.get "youtubeKey"
                return ApiKey config.youtubeKey
            }
        | StartWatching start -> 
            promise {
                let! key = (storage.get "youtubeKey").catch(fun _ -> {youtubeKey = undefined })
                let! lang = if key.youtubeKey = undefined then Promise.resolve "" else getVideoLang start.videoId key.youtubeKey
                let! res = objAdd "watches" ({ videoId = start.videoId; channelId = start.channelId; title = start.title; watchTime = 0; timestamp = DateTime.Now; audioLang = lang })
                printfn "LANG = %s" lang
                return WatchId res
            }
        | EndWatching endd -> 
            promise {
                let! res = objGet<int, WatchInfo> "watches" endd.id
                res.watchTime <- endd.watchTime                
                let! r = objPut "watches" res
                return Saved
            }
)
)
db.onupgradeneeded <- (fun ev -> 
    let t = ((ev.target :> obj) :?> Temp).result
    t.createObjectStore ("watches", ({autoIncrement = true; keyPath = "id"} :> obj) :?> IDBCreateStoreOptions)
    database <- t
)

promise {
    let! res = storage.get "youtubeKey"
    console.log res.youtubeKey
}



