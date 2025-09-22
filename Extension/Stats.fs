// For more information see https://aka.ms/fsharp-console-apps
module Stats
open Fable.Core
open Fetch
open Browser.Dom
open Shared
open JsInterop

open Feliz
open Feliz.Bulma

[<ReactComponent>]
let Counter () = 
    let r: WatchInfo array = [||]
    let (items, setItems) = React.useState(r)
    let (filter, setFilter) = React.useState("")
    React.useEffect((fun _ -> 
        promise { 
            let! entries = runtime.sendMessage(GetEntries) 
            console.log "loading"
            match entries with 
            | Entries entries -> 
                setItems(entries)
        }
        ()), [||] )
    let (filteredItems, setFiltered) = React.useState(items)
    React.useEffect((fun _ -> 
            let fnstr = sprintf "(item) => %s" filter
            let fn: WatchInfo -> bool = emitJsExpr null "eval(fnstr)"
            items |> Seq.filter fn |> Seq.toArray |> setFiltered
        ), [| filter |] |> cast )
    Bulma.panel [
        Bulma.input.text [ prop.placeholder "Filter"; prop.onTextChange setFilter] |> Bulma.panelBlock.div;
        Bulma.table [
            prop.children (filteredItems |> Seq.map (fun x -> Bulma.text.p x.title) |> Seq.toList )
        ] |> Bulma.panelBlock.div
    ]

open Browser.Dom

let root = ReactDOM.createRoot(document.getElementById "root")
root.render(Counter())

