module NuPogodiElectronics.App
open Browser.Dom

let canvas = document.createElement("div")
canvas.innerText <- "Hello world"

let appNode = document.querySelector "#app"
appNode.appendChild canvas
|> ignore
