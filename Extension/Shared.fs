module Shared
open Fable.Core
open Fable.Core.JS
open Browser.Dom
open System

type WatchInfo = { videoId: string; channelId: string; watchTime: float; title: string; timestamp: DateTime; audioLang: string; id: Guid }
type StartWatch = { videoId: string; title: string; channelId: string }
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
type Response =
    | WatchId of Guid
    | ApiKey of string
    | Entries of WatchInfo array
    | Saved
type OnMessageEvent = 
    abstract addListener: (Message -> Promise<Response>) -> unit
type BrowserRuntime = 
    abstract sendMessage: Message -> JS.Promise<Response> 
    abstract onMessage: OnMessageEvent with get, set
[<Emit("browser.runtime")>]
let runtime: BrowserRuntime = jsNative 
