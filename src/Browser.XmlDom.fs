namespace Browser.Types
open System
open Fable.Core

type DOMParserStatic =
    [<Emit("new $0()")>] abstract Create: unit -> DOMParser
    [<Emit("new $0($1...)")>] abstract Create: options: Options list -> DOMParser

and XMLSerializerStatic =
    [<Emit("new $0()")>] abstract Create: unit -> XMLSerializer

and DOMParser =
    abstract parseFromString: xmlsource: string * ?mimeType: string -> Browser.Types.Document

and XMLSerializer =
    abstract serializeToString: node: Browser.Types.Node -> string

and Options =
    | Locator of obj
    | ErrorHandler of Func<string, obj, obj>

namespace Browser
open Fable.Core
open Browser.Types

module XMLDom =
    let [<Global>] DOMParser: DOMParserStatic = jsNative
    let [<Global>] XMLSerializer: XMLSerializerStatic = jsNative
