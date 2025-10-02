module ContentScript
open Fable.Core
open Fetch
open Browser.Dom
open Shared
open Browser.Types
open System
open JsInterop
open Sutil
type Channel = { id: string; }
type Video = { description: string; title: string; channel: Channel; id: string }

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

type Model = { videoInformation: Video option; location: Location; videoElem: HTMLElement option; watchTime: float; playStarted: DateTime option; lang: string; watchStarted: DateTime }
type ViewMessage = 
    | UpdateVideoInfo of (Video * HTMLElement) option
    | OverwriteLang of string
    | StartTimers
    | TimerTick
    | UrlChanged
let timeout f = 
    Promise.create(
        (fun resolve _ -> 
            (JS.setTimeout 
                (fun _ -> 
                    resolve (f())
                ) 100
            ) |>ignore
        )
    )
let waitforUrlChanged old =
    promise {
        while old = window.location.href do
            do! Promise.sleep 100
        return UrlChanged
    }
let getCurrentVideoInfo prevtitle =
    promise {
        let! title = waitForSelector ("#title.ytd-watch-metadata")
        let mutable title = title?innerText
        console.log $"Title = {title}"
        while title = prevtitle || String.IsNullOrWhiteSpace(title) do
            do! Promise.sleep 20
            title <- document.querySelector("#title.ytd-watch-metadata")?innerText
            console.log $"Title = {title}"
        let links = document.querySelectorAll "ytd-app #channel-name a"
        let links: HTMLElement array = Fable.Core.JS.Array.from links 
        let link = links |> Seq.find(fun x -> x.offsetParent <> null)

        
        let href = link.getAttribute "href" 
        let handle = href.Substring (href.LastIndexOf "@")
        let! resp = fetch (sprintf "%s/%s" window.location.origin handle) []
        let! text = resp.text()
        let search = "\"channelUrl\":\"https://www.youtube.com/channel/"
        let rest = text.Substring( (text.IndexOf(search) + search.Length))
        let id = rest.Substring(0, rest.IndexOf "\"")
        let! elem = waitForSelector ("#description-inline-expander #expanded")
        elem?click()
        let! _ = Promise.sleep 50
        let desc = elem?innerText
        let params: URLSearchParams = Browser.Url.URLSearchParams.Create window.location.search 
        let vid = (params.get "v").Value
        let res = { id = vid; title = title; description = desc; channel = { id = id } }
        return res
    }
let getVideoInfo (prev, (loc: Location)) = 
    let res = 
        match loc.pathname with
        | "/watch" -> 
            let params: URLSearchParams = Browser.Url.URLSearchParams.Create window.location.search 
            match params.get "v" with
            | Some id -> 
                console.log $"getting video {id}"
                getCurrentVideoInfo prev
                |> Promise.result 
                |> Promise.bindResult (fun v -> 
                    promise {
                        let! elem = waitForPlayer()
                        let elem = elem.querySelector "video"
                        
                        return v, elem :?> HTMLElement
                    })
                |> Promise.map Result.toOption
            | _ -> None |> Promise.lift
        | _ -> None |> Promise.lift
    res |> Promise.map UpdateVideoInfo
let updateVideoInfoCmd prev loc = 
    Cmd.OfPromise.either getVideoInfo (prev, loc) id (fun _ -> UrlChanged)
let cmdOfPromise promise arg =
    Cmd.OfPromise.either promise arg id (fun _ -> failwith "shouldnt")
let waitForUnload () = 
    Promise.create (fun resolve reject ->
        window.onbeforeunload <- (fun _ -> resolve())    
    )
let saveOnUnloadCmd() = 
    let p = waitForUnload() |> Promise.map(fun _ -> UrlChanged)
    cmdOfPromise (fun _ -> p) ()

let update msg model =

    let getNewWatchAndNewPlay model =
        let (newTime, newPlay) = 
            let playing = 
                model.videoElem
                |> Option.bind (isVideoPlaying >> Some)
                |> Option.defaultValue false 
            match (playing, model.playStarted) with
            | (true, Some (t)) -> model.watchTime, model.playStarted
            | (true, None) -> model.watchTime, (Some DateTime.Now)
            | (false, Some(t)) -> 
                let diff = DateTime.Now - t
                model.watchTime + diff.TotalSeconds, None
            | (false, None) -> model.watchTime, None
        newTime, newPlay
    match msg with
    | UpdateVideoInfo (Some (video, elem)) ->
        { model with videoInformation = Some video; watchTime = 0; watchStarted = DateTime.Now; playStarted = None; videoElem = Some elem; lang = Eld.detect(video.description + "\n" + video.title )}, Cmd.batch [
            cmdOfPromise waitforUrlChanged model.location.href;
            cmdOfPromise timeout (fun _ -> TimerTick);
        ]
    | UpdateVideoInfo None ->
        { model with videoInformation = None ; playStarted = None; videoElem = None; lang = ""; watchStarted = DateTime.Now }, cmdOfPromise waitforUrlChanged model.location.href 
    | OverwriteLang lang -> { model with lang = lang }, Cmd.none
    | TimerTick ->
        let (newTime, newPlay) = getNewWatchAndNewPlay model
        {model with watchTime = newTime; playStarted = newPlay }, cmdOfPromise timeout (fun _ -> TimerTick)
    | UrlChanged ->
        let (newTime, newPlay) = getNewWatchAndNewPlay model
        console.log "maybe saving"
        match model.videoInformation with
        | Some(v) ->
            console.log "saving"
            let i = { videoId = v.id; audioLang = model.lang; watchTime = newTime; channelId = v.channel.id; title = v.title; timestamp = model.watchStarted; id = Guid.NewGuid() }
            sendMessage (Watched i) |> Promise.map(fun x -> console.log $"Save resp {x}") |> ignore
            ()
        | _ -> 
            console.log "no saving"
            ()
        { model with location = window.location }, Cmd.batch [
            updateVideoInfoCmd (model.videoInformation |> Option.bind (fun x -> x.title |> Some) |> Option.defaultValue "") model.location;
            saveOnUnloadCmd ()
        ]
    | _ -> model, Cmd.none 
let mainElement = document.createElement "div"
promise {
    let! p = waitForSelector "#container > #end"
    let view () = 
        let model, dispatch = Sutil.Store.makeElmish (fun _ -> { watchStarted = DateTime.Now; lang = ""; playStarted = None; watchTime = 0; videoInformation = None; location = window.location; videoElem = None }, Cmd.ofMsg UrlChanged) update ignore ()
        Bind.el (model |> Store.mapDistinct (fun x -> (x.videoInformation, x.lang)), (fun (vid, lang) -> 
            console.log "update"
            match vid with
            | Some(video) -> 
                Html.div [ 
                    Html.div [ 
                        prop.text $"Detected language {lang}"; prop.style "color: white;" 
                    ]; 
                    Html.input [ prop.placeholder "overwrite lang"; prop.id "ytt_overwrite_lang"];
                    Html.button [ prop.text "save"; Ev.onClick (fun _ -> dispatch (OverwriteLang (document.getElementById "ytt_overwrite_lang")?value)) ]
                ] 
            | None -> Html.none))


    Sutil.Program.mountAppend (p :?> HTMLElement, (view ()))
    console.log "here"


}
// ytd-watch-next-secondary-results-renderer #items ytd-item-section-renderer #header
(* startNew() *)

