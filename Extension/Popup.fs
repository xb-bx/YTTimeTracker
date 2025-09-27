// For more information see https://aka.ms/fsharp-console-apps
module Popup 
open Fable.Core
open Fetch
open Browser.Dom
open System
open Shared
open JsInterop
open Feliz.ViewEngine
open Elmish
open Browser.Types

type Tab =
    | Today
    | ThisWeek
    | AllTime
    | Settings
let formatSeconds (secs: float) = 
    let ts = TimeSpan.FromSeconds secs
    let hrs = ts.Days * 24 + ts.Hours
    sprintf "%ihrs %imins" hrs ts.Minutes 
let parseColumn (str: string): (string * string) =
    let unquote (s: string) = 
        let s = s.Substring(1)
        let mutable i = 0
        let mutable stop = false
        while i < s.Length && not stop do
            if s[i] = '"' && i = s.Length - 1 then ()
            else if s[i] = '"' && s[i+1] = '"' then i <- i + 1
            else if s[i] = '"' then stop <- true
            i <- i + 1
        let res = s.Substring(0, i-1)
        let this, rest = res.Replace("\"\"", "\""), (if i >= (s.Length - 1) then "" else s.Substring(i + 1))
        this,rest
    if str.StartsWith "\"" then
        unquote str
    else 
        let i = str.IndexOf(',')
        if i = -1 then
            str, ""
        else
            let s = str.Substring(0, i)
            let rest = str.Substring(i+1)
            s,rest
let parseCSV s =
    let rec p s acc =  
        match s with
        | "" -> acc
        | s -> 
            let s, rest = parseColumn s
            p rest (acc @ [s]) 
    p s []
let withClass classname elem props = 
    let found = props |> List.tryFind (fun x -> match x with | KeyValue("class", _) -> true | _ -> false)
    let classname = 
        match found with
        | Some(KeyValue("class", classes)) -> KeyValue("class", ((classes :?>string) + " " + classname))
        | _ -> KeyValue("class", classname)
    elem ([ classname ] @ props)
let block = withClass "block" Html.div
let button = withClass "button" Html.button
let panel = withClass "panel" Html.div
let panelTabs = withClass "panel-tabs" Html.div
let panelBlock = withClass "panel-block" Html.div
let input = withClass "input" Html.input

type Model = { activeTab: Tab; apikey: string; items: WatchInfo array }
type Command = 
    | Goto of Tab
    | SetApi of string
    | Export 
    | Import
    | SaveApi
let init items apikey () = 
    let tab = if window.location.pathname = "/options.html" || String.IsNullOrWhiteSpace apikey then Settings else Today
    
    { activeTab = tab; apikey = apikey; items = items; }, Cmd.none
let update msg model =
    match msg with
    | Goto t ->
        { model with activeTab = t }, Cmd.none
    | SetApi s ->
        printfn "change %s" s
        { model with apikey = s }, Cmd.none
emitJsStatement "" "var dispatchers = []"
[<Emit("dispatchers")>]
let dispatchers: (obj -> unit) array = jsNative
let mutable count = 0
let onclick handler = 
    dispatchers[count] <- handler
    count <- count + 1
    KeyValue("data-click", (sprintf "%i" (count-1)))
let ontextchange (handler: (string -> unit)) =
    dispatchers[count] <- (handler |> cast)
    count <- count + 1
    KeyValue("data-textchange", (sprintf "%i" (count-1)))

    
let globalhandler i o = 
    dispatchers[i](o)
let tab model dispatch (name: string) target = 
    Html.a [ prop.classes [ if model.activeTab = target then "is-active" else ""]; prop.text name; onclick (fun _ -> dispatch (Goto target)) ]
let StatsTab model = 
    let items = model.items
    let filtered = 
        match model.activeTab with
        | Today -> items |> Seq.filter (fun x -> DateOnly.FromDateTime(x.timestamp) = DateOnly.FromDateTime(DateTime.Today)) 
        | ThisWeek -> items |> Seq.filter (fun x -> DateOnly.FromDateTime(x.timestamp) > DateOnly.FromDateTime(( 7 |> TimeSpan.FromDays |> DateTime.Today.Subtract ))) 
        | AllTime -> items 
    if String.IsNullOrWhiteSpace model.apikey then 
        Html.div [ prop.text "SET YOUR API KEY" ] 
    else 
    Html.div [ 
        prop.style [ style.width (length.percent 100) ];
        prop.children [
            block [
                prop.text (sprintf "Total watch time: %s" (filtered |> Seq.sumBy (fun x -> x.watchTime) |> formatSeconds) );
            ];
            block [ 
                filtered 
                    |> Seq.groupBy (fun x -> x.audioLang) 
                    |> Seq.map (fun (audio, ws) -> (audio,ws |> Seq.sumBy(fun w -> w.watchTime))) 
                    |> Seq.map (fun (name, time) -> 
                        Html.p [ prop.text (sprintf "%s: %s" ( if name |> String.IsNullOrWhiteSpace then "unknown" else name)  (formatSeconds time) )]) 
                    |> prop.children 
            ]
        ]
    ]

let SettingsTab model dispatch = 
    Html.div [
        prop.style [ style.width (length.percent 100) ];
        prop.children [
            [
            input [ ontextchange (fun s -> dispatch (SetApi s)) ; prop.style [ style.width (length.percent 83); style.marginRight (length.px 5)]; prop.placeholder "API Key"; prop.value model.apikey ];
            button [
                prop.classes ["is-primary"];
                prop.text "Save";
                prop.type' "button";
                onclick (fun _ -> 
                    sendMessage (SetApiKey model.apikey) |> ignore
                )
            ]
            ] |> prop.children |> (fun x -> [x]) |> block;
            button [
                prop.classes ["is-primary"];
                prop.text "Import from CSV";
                onclick (fun _ ->
                    if window.location.pathname = "/options.html" then
                        let input = document.createElement "input";   
                        input.setAttribute("type", "file")
                        input.onchange <- (fun ev ->
                            let target = ev?target?files
                            let file = emitJsExpr "" "target[0]"
                            promise {
                                let! text = file?text()
                                let t: string = text
                                let lines = text.Split('\n')
                                let items = 
                                    lines |> Seq.tail |> Seq.filter (fun x -> String.IsNullOrWhiteSpace x |> not) |> Seq.map(fun l -> 
                                        let ls = parseCSV l
                                        printfn "%s %A" l ls
                                        let [id;channelid;videoid;title;audiolang;watchtime;timestamp] = ls
                                        let fromUnix s = 
                                            let d = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc)
                                            (d.AddSeconds ((Double.Parse(s)))).ToLocalTime()
                                        {id = Guid.Parse(id); channelId = channelid; videoId = videoid; title = title; audioLang = audiolang; watchTime = Double.Parse watchtime; timestamp = fromUnix (timestamp)}
                                    ) |> Seq.toArray 
                                printfn "%A" items
                                return! sendMessage (Message.Import items)
                            }
                            ()
                        )
                        input.click()
                    else
                        window.``open``((sprintf "%s/%s" window.location.origin "options.html"), "_blank") 
                        ()
                    ()
                )
            ] |> prop.children |> (fun x -> [x]) |> block;
            button [
                prop.className "is-primary";
                prop.text "Export as CSV";
                onclick (fun _ -> sendMessage Message.Export |> ignore)
            ] |> prop.children |> (fun x -> [x]) |> block;
        ]
    ]
let view model dispatch =
    count <- 0
    let tab = tab model dispatch
    let view = 
        panel [
            prop.children [
                panelTabs [ 
                    prop.children [
                        tab "Today" Today;
                        tab "This week" ThisWeek;
                        tab "All time" AllTime;
                        tab "Settings" Settings;
                    ]
                ];
                panelBlock [
                    prop.children [
                        match model.activeTab with
                        | Settings -> 
                            SettingsTab model dispatch 
                        | _ -> StatsTab model
                    ]
                ];
            ]
        ] |> Render.htmlView
    (document.getElementById "root").innerHTML <- view
    document.querySelectorAll("[data-click]") 
        |> Fable.Core.JS.Array.from
        |> Seq.map(fun x -> x :> HTMLElement) |> Seq.iter (fun x -> x.onmouseup <- (fun ev -> globalhandler (x.dataset["click"] |> Int32.Parse) ev ))
    document.querySelectorAll("[data-textchange]") 
        |> Fable.Core.JS.Array.from
        |> Seq.map(fun x -> x :> HTMLElement) |> Seq.iter (fun x -> x.onchange <- (fun ev -> globalhandler (x.dataset["textchange"] |> Int32.Parse) ev?target?value ))

promise { 
    let a = GetApiKey
    let b = emitJsExpr a "structuredClone($0)"
    let! items = sendMessage GetEntries 
    let! apikey = sendMessage GetApiKey
    match (items, apikey) with
    | (Entries items, ApiKey apikey) -> 
        if isChrome then // FUCK CHROME
            items |> Seq.iter (fun x -> emitJsStatement x "$0.timestamp = new Date($0.timestamp)")
        else 
            ()
        Program.mkProgram (init items apikey) update view
            |> Program.run
}

