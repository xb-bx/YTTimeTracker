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
let startTimer () =  
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
let getChannelId () =
    promise {
        let! link = waitForSelector ".ytd-channel-name a"
        let href = link.getAttribute "href" 
        let handle = href.Substring (href.LastIndexOf "@")
        let! resp = fetch (sprintf "https://youtube.com/%s" handle) []
        let! text = resp.text()
        let search = "\"channelUrl\":\"https://www.youtube.com/channel/"
        let rest = text.Substring( (text.IndexOf(search) + search.Length))
        let id = rest.Substring(0, rest.IndexOf "\"")
        return id
    }
let saveChanges() =
    printfn "saving... %A" watchId
    runtime.sendMessage (EndWatching { id = watchId; watchTime = totalWatch})
    ()
let startNew() =
    promise {
        printfn "starting"
        url <- window.location.href
        let id = getVideoId()
        if id.IsSome then 
            let! player = waitForPlayer()
            let! channelId = getChannelId()
            let id = id.Value
            printfn "id = %s & channelid = %s" id channelId
            let! vid = runtime.sendMessage (StartWatching { videoId = id; channelId = channelId; title = ((document.querySelector "a.ytp-title-link") :?> Browser.Types.HTMLElement).innerText })
            match vid with | WatchId i -> watchId <- i
            let video = (player.querySelector "video") :?> Browser.Types.HTMLElement
            let paused = video?paused
            let ended = video?ended
            let readyState = video?readyState
            if not paused && not ended && readyState > 2 then
                timstampStart <- Some DateTime.Now
            else ()
            printfn  "video is null %b" (video = null)
            video.onplay <- (fun _ -> startTimer())
            video.onpause <- (fun _ -> startTimer())
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
    startNew()
    ()
    

window.setInterval((fun _ -> if url <> window.location.href then promise { urlChanged() } |> ignore else () ), 100)
window.onbeforeunload <- fun _ -> 
    if timstampStart.IsSome then
        startTimer()
    else 
        ()
    saveChanges()

startNew()

