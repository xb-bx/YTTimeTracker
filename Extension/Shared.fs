module Shared
open Fable.Core
open Fable.Core.JS
open Browser.Dom
open System
type StartWatch = { videoId: string; title: string; channelId: string }
type EndWatch = { id: Guid; watchTime: float; }
type Message = 
    | StartWatching of StartWatch
    | EndWatching of EndWatch
    | SetApiKey of string
    | GetApiKey
    | Export
type Response =
    | WatchId of Guid
    | ApiKey of string
    | Saved
type OnMessageEvent = 
    abstract addListener: (Message -> Promise<Response>) -> unit
type BrowserRuntime = 
    abstract sendMessage: Message -> JS.Promise<Response> 
    abstract onMessage: OnMessageEvent with get, set
[<Emit("browser.runtime")>]
let runtime: BrowserRuntime = jsNative 
