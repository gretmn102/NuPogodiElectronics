module NuPogodiElectronics.App
open Browser.Dom
open Browser.Types
open Browser.XMLDom

let assetsUrl : string =
    Fable.Core.JsInterop.importDefault "../assets/assets.svg"

let canvas : HTMLDivElement = document.createElement("div") :?> HTMLDivElement

let appNode = document.querySelector "#app"
appNode.setAttribute("style", "height: 100%;")
appNode.appendChild canvas
|> ignore

canvas.innerText <- "Loading..."
canvas.setAttribute("style", "height: 100%;")

let loadSvg () =
    Fetch.fetch assetsUrl []
    |> Promise.bind (fun x ->
        x.text()
    )
    |> Promise.map (fun rawSvg ->
        let parser = DOMParser.Create ()
        let doc = parser.parseFromString(rawSvg, "image/svg+xml")
        let svg = doc.documentElement :?> SVGElement
        svg.setAttribute("width", "100%")
        svg.setAttribute("height", "100%")
        svg
        |> Ok
    )
    |> Promise.catch (fun x ->
        Error (sprintf "error load %s\n%s" assetsUrl x.Message)
    )

loadSvg ()
|> Promise.map (fun x ->
    match x with
    | Ok svg ->
        canvas.removeChild canvas.firstChild |> ignore
        canvas.appendChild svg
        |> ignore

        let assetsManager = AssetsManager.create svg

        Game.start assetsManager
    | Error msg ->
        canvas.innerText <- msg
)
|> ignore
