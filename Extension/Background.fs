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
let browserstorage: BrowserStorage = jsNative
[<Emit("chrome.storage.local")>]
let chromestorage: BrowserStorage = jsNative
let storage = if isChrome then chromestorage else browserstorage

type DownloadOpts = { url: string; filename: string; saveAs: bool }

[<Emit("chrome.downloads.download")>]
let chromedownload: DownloadOpts -> Promise<int> = jsNative
[<Emit("browser.downloads.download")>]
let browserdownload: DownloadOpts -> Promise<int> = jsNative
let download = if isChrome then chromedownload else browserdownload 


let objGetAll<'a, 'b> (dbstore: string): Fable.Core.JS.Promise<'b array> =
    let dbstore = (database.transaction (dbstore, Readwrite)).objectStore dbstore 
    let res = dbstore.getAll()

    Promise.Create (fun resolve reject -> 
        res.onsuccess <- (fun (_) -> res.result |> cast |> resolve )
        res.onerror <- (fun (_) -> res.error |> cast |> reject)
    )
let objGetCount<'a, 'b> (dbstore: string) start count: Fable.Core.JS.Promise<'b> =
    let dbstore = (database.transaction (dbstore, Readwrite)).objectStore dbstore 
    let range = IDBKeyRange.lowerBound(start, true)
    let res = dbstore.getAll(range, count)

    Promise.Create (fun resolve reject -> 
        res.onsuccess <- (fun (_) -> res.result |> cast |> resolve )
        res.onerror <- (fun (_) -> res.error |> cast |> reject)
    )
let objGet<'a, 'b> (dbstore: string) (key: 'a): Fable.Core.JS.Promise<'b> =
    let dbstore = (database.transaction (dbstore, Readwrite)).objectStore dbstore 
    let res = dbstore.get key

    Promise.Create (fun resolve reject -> 
        res.onsuccess <- (fun (_) -> res.result |> cast |> resolve )
        res.onerror <- (fun (_) -> res.error |> cast |> reject)
    )
let objPut<'a> (dbstore: string) (obj: 'a): Fable.Core.JS.Promise<'a> =
    let dbstore = (database.transaction (dbstore, Readwrite)).objectStore dbstore 
    let res = dbstore.put obj
    Promise.Create (fun resolve reject -> 
        res.onsuccess <- (fun (_) -> res.result |> cast |> resolve )
        res.onerror <- (fun (_) -> res.error |> cast |> reject)
    )
let objAdd<'a> (dbstore: string) (obj: 'a) : Fable.Core.JS.Promise<'b> =
    let dbstore = (database.transaction ("watches", Readwrite)).objectStore dbstore 
    let res = dbstore.add obj
    Promise.Create (fun resolve reject -> 
        res.onsuccess <- (fun (_) -> res.result |> cast |> resolve )
        res.onerror <- (fun (_) -> res.error |> cast |> reject)
    )

let getVideoLang id apikey: Promise<(string * string * string)> = 
    promise {
        let! res = fetch (sprintf "https://youtube.googleapis.com/youtube/v3/videos?part=snippet%%2CcontentDetails%%2Cstatistics&id=%s&key=%s" id apikey ) []
        let! object = res.json()
        let chan: string = emitJsExpr object "$0.items[0].snippet.channelId"
        let title: string = emitJsExpr object "$0.items[0].snippet.title"
        let a = emitJsExpr object "$0.items[0].snippet.defaultLanguage" 
        let a = 
            if a = undefined then 
                emitJsExpr object "$0.items[0].snippet.defaultAudioLanguage"
            else
                a
        let a = 
            if a = undefined then 
                "unknown"
            else
                a

        let i = a.IndexOf('-')
        let a = 
            if i <> -1 then 
                a.Substring(0, i)
            else 
                a
        return a, chan, title
    }

let toCsvRow (w: WatchInfo): string = 
    let escape (s: string) = 
        let a = s.Replace("\"", "\"\"")
        sprintf "\"%s\"" a
    sprintf "%s,%s,%s,%s,%s,%f,%i\n" (w.id.ToString()) w.channelId w.videoId (escape w.title) w.audioLang w.watchTime ((new DateTimeOffset(w.timestamp)).ToUnixTimeSeconds())
let db = indexedDB.``open`` "yttracker"
db.onerror <- (fun ev -> console.error(ev))
db.onsuccess <- (fun ev -> 
    let t = ((ev.target :> obj) :?> Temp).result
    database <- t

    addListener (fun (m: Message) -> 
        match m with 
        | GetEntries ->
            promise {
                let! res = objGetAll "watches" 
                return res |> Entries
            }
        | Import a ->
            promise { 
                let items = 
                    a 
                    |> Seq.map (fun i -> 
                        objAdd "watches" i)
                    |> Seq.toArray 
                let! _ = Promise.all items
                return Saved
            }
        | Export ->
            promise { 
                let! watches = objGetAll "watches"
                let data = watches |> Seq.filter (fun (x: WatchInfo) -> x.watchTime <> 0) |> Seq.map (toCsvRow) |> Seq.insertAt 0 "id,channelId,videoId,title,audioLang,watchTime,timestamp\n" |> Seq.toArray 
                if isChrome then 
                    let btoa: obj -> string = emitJsExpr "" "btoa"
                    let url = sprintf "data:text/csv;base64,%s" (data |> String.concat "" |> btoa)
                    let! id = download {url = url; filename = "watchdata.csv"; saveAs = true}
                    ()
                else 
                    let blob = Browser.Blob.Blob.Create (data |> cast)
                    let url = Browser.Url.URL.createObjectURL blob
                    let! id = download {url = url; filename = "watchdata.csv"; saveAs = true}
                    Browser.Url.URL.revokeObjectURL url
                return Saved
            }
        | SetApiKey key -> 
            storage.set ({ youtubeKey = key })
            Promise.resolve Saved
        | GetApiKey -> 
            promise {
                let! config = storage.get "youtubeKey"
                let res = if config = undefined || config.youtubeKey = undefined || config.youtubeKey = null then "" else config.youtubeKey
                return ApiKey res
            }
        | StartWatching start -> 
            promise {
                let! key = (storage.get "youtubeKey").catch(fun _ -> {youtubeKey = undefined })
                let! lang, chan, title = if key.youtubeKey = undefined then Promise.resolve (("", "", "")) else getVideoLang start.videoId key.youtubeKey
                let! res = objAdd "watches" ({ videoId = start.videoId; channelId = chan; title = title; watchTime = 0; timestamp = DateTime.Now; audioLang = lang; id = (Guid.NewGuid()) })
                printfn "LANG = %s" lang
                return WatchId res
            }
        | GetCurrentLang ->
            promise {
                let! resp = (sendMessageToActive (GetCurrentWatch))
                match resp with
                | Some(WatchId id) -> 
                    let! w = objGet<Guid, WatchInfo> "watches" id
                    if w <> undefined then
                        return Lang w.audioLang 
                    else 
                        console.error "not found"
                        return Saved
                | _ -> 
                    return Saved
            }
        | SetLang s -> promise {
                let! tab = getActiveTab()
                if tab.Length = 0 then
                    console.log "notabs"
                    return Saved
                else 
                    let! resp = (sendMessageToTab tab[0].id (GetCurrentWatch)).catch(fun _ -> WatchId (Guid.Empty))
                    match resp with
                    | WatchId id -> 
                        let! w = objGet<Guid, WatchInfo> "watches" id
                        if w <> undefined then
                            let res = { w with audioLang = s }
                            let! _ = objPut "watches" res
                            return Saved
                        else 
                            console.error "not found"
                            return Saved
                    | _ -> 
                        return Saved

                        
            }
        | EndWatching endd -> 
            promise {
                let! res = objGet<Guid, WatchInfo> "watches" endd.id
                if res <> undefined then
                    let res = { res with watchTime = endd.watchTime }
                    let! r = objPut "watches" res
                    ()
                else 
                    ()
                return Saved
            }
)
)
db.onupgradeneeded <- (fun ev -> 
    let t = ((ev.target :> obj) :?> Temp).result
    let watches = t.createObjectStore ("watches", ({autoIncrement = false; keyPath = "id"} :> obj) :?> IDBCreateStoreOptions)
    database <- t
)

promise {
    let! res = storage.get "youtubeKey"
    console.log res.youtubeKey
}



