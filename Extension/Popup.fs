// For more information see https://aka.ms/fsharp-console-apps
module Popup 
open Fable.Core
open Fetch
open Browser.Dom
open System
open Shared
open JsInterop
open Browser.Types
open Sutil
open Bulma

type Tab =
    | Today
    | ThisWeek
    | ThisMonth
    | ThisYear
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

type Model = { activeTab: Tab; items: WatchInfo array; }
type Command = 
    | Goto of Tab
    | Export 
    | Import
let init items () = 
    let tab = if window.location.pathname = "/options.html" then Settings else Today
    { activeTab = tab; items = items; }, Cmd.none
let update msg model =
    match msg with
    | Goto t ->
        { model with activeTab = t }, Cmd.none
    
let dynamic<'a> (data: ResizeArray<(Model -> obj) * (HTMLElement -> obj -> HTMLElement) * obj>) model (selector: Model -> 'a) (elemcreate: Model -> HTMLElement) =
    let wrapped m (old: HTMLElement) model = 
        let res: HTMLElement = m model
        let dynid = if old <> null then old.dataset["dynid"] else data.Count.ToString()
        res.dataset["dynid"] <- dynid
        if old <> null then
            let result = res
            old.parentElement.replaceChild(result, old) 
            ()
        else
            ()
        res
    let res = wrapped elemcreate null model
    let s = selector |> cast
    data.Add (s, (wrapped elemcreate) |> cast, s model)
    res
let tab model dispatch (name: string) target = 
    Bind.el ((model), (fun model -> Html.a [ prop.className (if model.activeTab = target then "is-active" else ""); prop.text name; Ev.onClick (fun _ -> dispatch (Goto target)) ]))
let startofweek (d: DateTime) =
    let diff = (7 + (int(d.DayOfWeek) - int(DayOfWeek.Monday))) % 7
    d.AddDays(float(-1 * diff)).Date
let startofmonth (d: DateTime) =
    let d = d.Date
    new DateTime(d.Year, d.Month, 1)
let startofyear (d: DateTime) =
    let d = d.Date
    new DateTime(d.Year, 1, 1)
let averageOrZero s =
    if (Seq.isEmpty s) then 0.0
    else s |> Seq.average
    
let StatsTab model dispatch = 
    let average data =
        data |> Seq.groupBy (fun x -> x.timestamp.Date) |> Seq.map (fun (d, items) -> items |> Seq.sumBy (fun x -> x.watchTime)) |> averageOrZero
    let items = (model |> Store.get).items
    let filtered = 
        match (model |> Store.get).activeTab with
        | Today -> items |> Seq.filter (fun x -> DateOnly.FromDateTime(x.timestamp) = DateOnly.FromDateTime(DateTime.Today)) 
        | ThisWeek -> items |> Seq.filter (fun x -> DateOnly.FromDateTime(x.timestamp) >= DateOnly.FromDateTime(DateTime.Now |> startofweek)) 
        | ThisMonth -> items |> Seq.filter (fun x -> DateOnly.FromDateTime(x.timestamp) >= DateOnly.FromDateTime(DateTime.Now |> startofmonth)) 
        | ThisYear -> items |> Seq.filter (fun x -> DateOnly.FromDateTime(x.timestamp) >= DateOnly.FromDateTime(DateTime.Now |> startofyear)) 
        | AllTime -> items 
    Html.div [ 
        prop.style "width: 100%";
        bulma.block [
            prop.text (sprintf "Total watch time: %s" (filtered |> Seq.sumBy (fun x -> x.watchTime) |> formatSeconds) );
        ];
        bulma.block [
            prop.text (sprintf "Per day: %s" (filtered |> average |> formatSeconds) );
        ];
        bulma.block  
            (filtered 
                |> Seq.groupBy (fun x -> x.audioLang) 
                |> Seq.map (fun (audio, ws) -> (audio,ws |> Seq.sumBy(fun w -> w.watchTime), ws |> average)) 
                |> Seq.map (fun (name, time, avgTime) -> 
                    Html.p [ prop.text (sprintf "%s: %s/per day %s" ( if name |> String.IsNullOrWhiteSpace then "unknown" else name)  (formatSeconds time) (formatSeconds avgTime) )]) 
                |> List.ofSeq)
        
    ]

let SettingsTab model dispatch = 
    Html.div [
        prop.style "width: 100%";
            bulma.block [ bulma.button.button [
                prop.className "is-primary control";
                prop.text "Import from CSV";
                Ev.onClick (fun _ ->
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
                                            (d.AddSeconds ((Double.Parse(s))))
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
            ]];
            bulma.block [ bulma.button.button [
                prop.className "is-primary";
                prop.text "Export as CSV";
                Ev.onClick (fun _ -> sendMessage Message.Export |> ignore)
            ]]

    ]
let view items =
    let model, dispatch = Store.makeElmish (init items ) update ignore ()
    let tab = tab model dispatch 
    let mmodel = model
    let view = 
        Bulma.bulma.panel [
            bulma.panelTabs [ 
                tab "Today" Today;
                tab "This week" ThisWeek;
                tab "This month" ThisMonth;
                tab "This year" ThisYear;
                tab "All time" AllTime;
                tab "Settings" Settings;
            ];
            Bind.el((model), (fun model -> 
                bulma.panelTabs [
                    match model.activeTab with
                    | Settings -> 
                        SettingsTab model dispatch 
                    | _ -> StatsTab mmodel dispatch

                ]));
        ]
    view

promise { 
    let! items = sendMessage GetEntries 
    match items with
    | Entries items -> 
        if isChrome then // FUCK CHROME
            items |> Seq.iter (fun x -> emitJsStatement x "$0.timestamp = new Date($0.timestamp)")
        else 
            ()
        view items |> Program.mount
}

