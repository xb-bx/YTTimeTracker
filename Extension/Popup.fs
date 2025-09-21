// For more information see https://aka.ms/fsharp-console-apps
module Popup 
open Fable.Core
open Fetch
open Browser.Dom
open Shared
open JsInterop

let saveBtn = document.getElementById "saveButton"
let exportBtn = document.getElementById "exportButton"
let apikeyinput = document.getElementById "apikey"
saveBtn.onclick <- (fun _ -> 
    runtime.sendMessage (SetApiKey (apikeyinput?value)) |> ignore) 
exportBtn.onclick <- (fun _ -> 
    runtime.sendMessage (Export) |> ignore) 

promise {
    let! resp = runtime.sendMessage GetApiKey
    match resp with 
    | ApiKey key -> apikeyinput.setAttribute("value", key)
}

