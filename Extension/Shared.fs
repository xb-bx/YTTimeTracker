module Shared
open Fable.Core
open Fable.Core.JS
open Browser.Dom
open System
open Fable.Core.JsInterop

type WatchInfo = { videoId: string; channelId: string; watchTime: float; title: string; timestamp: DateTime; audioLang: string; id: Guid }
type StartWatch = { videoId: string }
type EndWatch = { id: Guid; watchTime: float; }

let cast<'a, 'b> (v: 'b) =
    (v :> obj) :?> 'a
type Message = 
    | StartWatching of StartWatch
    | EndWatching of EndWatch
    | SetApiKey of string
    | GetApiKey
    | GetEntries 
    | Export
    | Import of WatchInfo array
    | GetCurrentLang
    | GetCurrentWatch
    | SetLang of string
type Response =
    | WatchId of Guid
    | ApiKey of string
    | Entries of WatchInfo array
    | Lang of string
    | Saved
type OnMessageEvent = 
    abstract addListener: (Message -> Promise<Response>) -> unit
type OnMessageEventChrome = 
    abstract addListener: (obj -> obj -> (obj -> unit) -> bool) -> unit
type QueryParams = { active: bool }
type Tab = {id: int}
type Tabs = 
    abstract query: QueryParams -> Promise<Tab array>
    abstract sendMessage: int -> Message -> Promise<Response>
type ChromeTabs = 
    abstract query: QueryParams -> Promise<Tab array>
    abstract sendMessage: int -> obj -> Promise<Response>
type BrowserRuntime = 
    abstract sendMessage: Message -> JS.Promise<Response> 
    abstract onMessage: OnMessageEvent with get, set
type ChromeRuntime = 
    abstract sendMessage: obj -> (Response -> unit) -> unit
    abstract onMessage: OnMessageEventChrome with get, set
    abstract lastError: obj with get
[<Emit("browser.runtime")>]
let browserruntime: BrowserRuntime = jsNative 
[<Emit("browser.tabs")>]
let browsertabs: Tabs= jsNative 
[<Emit("chrome.runtime")>]
let chromeruntime: ChromeRuntime = jsNative 
[<Emit("chrome.tabs")>]
let chrometabs: ChromeTabs = jsNative 

let unionToObj (msg: 'a) = 
    {| tag = msg?tag; fields = msg?fields |}

let unionFromObj (msg: obj): 'a = 
    emitJsExpr msg "new Message($0.tag, $0.fields)"

let isChrome: bool = emitJsExpr "" "typeof chrome !== 'undefined' && typeof browser === 'undefined'" 
let sendMessage (msg: Message): JS.Promise<Response> = 
    if isChrome then
        Promise.Create(fun resolve reject -> 
            chromeruntime.sendMessage (unionToObj msg) (fun res -> // FUCK CHROME
                if chromeruntime.lastError <> null then
                    reject chromeruntime.lastError
                else 
                    resolve (unionFromObj res)
            )
        )
    else 
        browserruntime.sendMessage msg

let addListener (listener: Message -> JS.Promise<Response>) = 
    if isChrome then
        chromeruntime.onMessage.addListener (fun msg sender respond -> // FUCK CHROME
            promise { 
                let! res = listener (unionFromObj msg)
                respond (unionToObj res)
            }
            true
        )
    else 
        browserruntime.onMessage.addListener listener 
let getActiveTab () =
    if isChrome then
        chrometabs.query {active = true}
    else
        browsertabs.query {active = true}

let sendMessageToTab (tab:int) (msg: Message): JS.Promise<Response> = 
    if isChrome then
        promise { 
            let obj = (unionToObj msg)
            console.log "sending"
            console.log obj
            let! res = chrometabs.sendMessage tab obj
            let res =  unionFromObj res
            console.log "received"
            console.log res
            return res
        }
    else 
        browsertabs.sendMessage tab msg
let sendMessageToActive (msg: Message): JS.Promise<Response option> = 
    promise { 
        let! tabs = getActiveTab()
        if tabs.Length = 0 then 
            return None
        else 
            let tab = tabs[0].id
            let! res = (sendMessageToTab tab msg).``then``((fun x -> Some x), (fun x -> None))
            return res
    }
