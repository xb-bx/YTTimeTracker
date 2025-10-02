module Eld
open Fable.Core.JsInterop
let detect (text: string): string = emitJsExpr text "window.eld.detect($0).language"
