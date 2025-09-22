// For more information see https://aka.ms/fsharp-console-apps
module Popup 
open Fable.Core
open Fetch
open Browser.Dom
open System
open Shared
open JsInterop
open Feliz
open Feliz.Bulma
[<Emit("browser.tabs.create({url: $0})")>]
let tabCreate taburl = jsNative
[<Emit("browser.runtime.getURL")>]
let getURL: string -> string = jsNative

(* let saveBtn = document.getElementById "saveButton" *)
(* let exportBtn = document.getElementById "exportButton" *)
(* let viewStatsBtn = document.getElementById "viewStatsBtn" *)
(* let apikeyinput = document.getElementById "apikey" *)
(* saveBtn.onclick <- (fun _ ->  *)
(*     runtime.sendMessage (SetApiKey (apikeyinput?value)) |> ignore)  *)
(* exportBtn.onclick <- (fun _ ->  *)
(*     runtime.sendMessage (Export) |> ignore)  *)
(* viewStatsBtn.onclick <- (fun _ ->  *)
(*     "stats.html" |> getURL |> tabCreate)  *)
(**)
(* promise { *)
(*     let! resp = runtime.sendMessage GetApiKey *)
(*     match resp with  *)
(*     | ApiKey key -> apikeyinput.setAttribute("value", key) *)
(* } *)

type Tab =
    | Today
    | ThisWeek
    | AllTime
    | Settings
let formatSeconds (secs: float) = 
    let ts = TimeSpan.FromSeconds secs
    let hrs = ts.Days * 24 + ts.Hours
    sprintf "%ihrs %imins" hrs ts.Minutes 
    
[<ReactComponent>]
let StatsTab(tab: Tab) = 
    let (items: WatchInfo array, setItems) = React.useState([||])
    React.useEffect((fun _ -> 
        promise {
            let! res = runtime.sendMessage GetEntries 
            match res with
            | Entries entries -> setItems entries
        }
        ()), [||])
    let filtered = 
        match tab with
        | Today -> items |> Seq.filter (fun x -> DateOnly.FromDateTime(x.timestamp) = DateOnly.FromDateTime(DateTime.Today)) 
        | ThisWeek -> items |> Seq.filter (fun x -> DateOnly.FromDateTime(x.timestamp) > DateOnly.FromDateTime(( 7 |> TimeSpan.FromDays |> DateTime.Today.Subtract ))) 
        | AllTime -> items 
    Html.div [ 
        prop.style [ style.width (length.percent 100) ];
        prop.children [
            Bulma.block [
                prop.text (sprintf "Total watch time: %s" (filtered |> Seq.sumBy (fun x -> x.watchTime) |> formatSeconds) );
            ];
            Bulma.block [ 
                filtered 
                    |> Seq.groupBy (fun x -> x.audioLang) 
                    |> Seq.map (fun (audio, ws) -> (audio,ws |> Seq.sumBy(fun w -> w.watchTime))) 
                    |> Seq.map (fun (name, time) -> 
                        Bulma.text.p [ prop.text (sprintf "%s: %s" ( if name |> String.IsNullOrWhiteSpace then "unknown" else name)  (formatSeconds time) )]) 
                    |> prop.children 
            ]
        ]
    ]

    (* Bulma.text.p [ prop.text (sprintf "Total watch time: %s" (((filterd |> Seq.sumBy (fun x -> x.watchTime) |> TimeSpan.FromSeconds)).ToString()) ) ] *)
[<ReactComponent>]
let SettingsTab() = 
    let (apiKey, setApiKey) = React.useState("")
    React.useEffect((fun _ -> 
            promise {
                let! res = runtime.sendMessage GetApiKey
                match res with
                | ApiKey s -> setApiKey s
                ()
            }
            ()
        ), [||])
    Html.div [
        prop.style [ style.width (length.percent 100) ];
        prop.children [
            [
            Bulma.input.text [ prop.onTextChange setApiKey; prop.style [ style.width (length.percent 83); style.marginRight (length.px 5)]; prop.placeholder "API Key"; prop.valueOrDefault apiKey ];
            Bulma.button.button [
                prop.className "is-primary";
                prop.text "Save";
                prop.onClick (fun _ -> runtime.sendMessage (SetApiKey apiKey) |> ignore)
                
            ]] |> Bulma.block;
            Bulma.button.button [
                prop.className "is-primary";
                prop.text "Export as CSV";
                prop.onClick (fun _ -> runtime.sendMessage Export |> ignore)
            ] |> Bulma.block;
        ]
    ]
[<ReactComponent>]
let TabLink isActive (header: string) tab setTab =
    Html.a [ prop.classes [ if isActive() then "is-active" else ""]; prop.text header; prop.onClick (fun _ -> setTab tab) ]
[<ReactComponent>]
let Popup() = 
    let (activeTab, setTab) = React.useState(Today)
    Bulma.panel [
        Bulma.panelTabs [
            TabLink (fun _ -> activeTab = Today) "Today" Today setTab;
            TabLink (fun _ -> activeTab = ThisWeek) "This week" ThisWeek setTab;
            TabLink (fun _ -> activeTab = AllTime) "All time" AllTime setTab;
            TabLink (fun _ -> activeTab = Settings) "Settings" Settings setTab;
        ];
        Bulma.panelBlock.div [
            match activeTab with
            | Settings -> SettingsTab()
            | _ -> StatsTab(activeTab)
        ];
    ]

let root = ReactDOM.createRoot(document.getElementById "root")
root.render(Popup())
