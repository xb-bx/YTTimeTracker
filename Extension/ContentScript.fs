module ContentScript
open Fable.Core
open Fetch
open Browser.Dom
open Shared
open Browser.Types
open System
open JsInterop

let mutable totalWatch: float = 0
let mutable watchId = Guid.Empty 
let mutable timstampStart: DateTime option = None
let mutable url = window.location.href
let mutable video = null
let mutable isplaying = false

let waitForSelector selector: JS.Promise<Browser.Types.Element> = 
    promise {
        let mutable p = null
        while p = null do 
            p <- document.querySelector selector
            if p = null then
                let! _ = Promise.sleep 100
                ()
            else ()
        return p
    }
let waitForPlayer (): JS.Promise<Browser.Types.HTMLElement> = 
    promise {
        let mutable p = null
        while p = null do 
            p <- document.getElementById "movie_player"
            if p = null then
                let! _ = Promise.sleep 1000
                ()
            else ()
        return p
    }
let isVideoPlaying video = 
    let paused = video?paused
    let ended = video?ended
    let readyState = video?readyState
    not paused && not ended && readyState > 2
let startTimer () =  
    isplaying <- isVideoPlaying video
    match timstampStart with
    | Some t -> 
        let diff = DateTime.Now - t
        totalWatch <- totalWatch + diff.TotalSeconds
        timstampStart <- None
    | None ->
        timstampStart <- Some DateTime.Now
    printfn  "total: %f" totalWatch

let getVideoId () = 
    let params: URLSearchParams = Browser.Url.URLSearchParams.Create window.location.search 
    let id = params.get "v"
    id
let saveChanges() =
    printfn "saving... %A" watchId
    sendMessage (EndWatching { id = watchId; watchTime = totalWatch})
    ()


let startNew() =
    promise {
        printfn "starting"
        url <- window.location.href
        let id = getVideoId()
        if id.IsSome then 
            let! player = waitForPlayer()
            let id = id.Value
            let! vid = sendMessage (StartWatching { videoId = id })
            match vid with | WatchId i -> watchId <- i
            video <- (player.querySelector "video") :?> Browser.Types.HTMLElement
            if isVideoPlaying video then
                timstampStart <- Some DateTime.Now
                isplaying <- true
            else ()
            printfn  "video is null %b" (video = null)
            totalWatch <- 0
        else 
            ()
    }
let urlChanged() = 
    if timstampStart.IsSome then
        startTimer()
    else 
        ()
    saveChanges()
    video <- null
    isplaying <- false
    startNew()
    ()
    

window.setInterval((fun _ -> if url <> window.location.href then promise { urlChanged() } |> ignore else () ), 100)
window.setInterval((fun _ ->
    if video <> null then (if (isVideoPlaying video) = isplaying then () else startTimer()) ), 100)
window.onbeforeunload <- fun _ -> 
    if timstampStart.IsSome then
        startTimer()
    else 
        ()
    saveChanges()

startNew()

