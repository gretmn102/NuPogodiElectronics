module NuPogodiElectronics.Joystick
open Browser.Dom

let mutable isRight = false
let mutable isLeft = false
let mutable isUp = false
let mutable isDown = false

document.onkeydown <- fun x ->
    match x.code with
    | "KeyD" | "ArrowRight" ->
        isRight <- true
    | "KeyA" | "ArrowLeft" ->
        isLeft <- true
    | "KeyW" | "ArrowUp" ->
        isUp <- true
    | "KeyS" | "ArrowDown" ->
        isDown <- true
    | _ ->
        ()

document.onkeyup <- fun x ->
    match x.code with
    | "KeyD" | "ArrowRight" ->
        isRight <- false
    | "KeyA" | "ArrowLeft" ->
        isLeft <- false
    | "KeyW" | "ArrowUp" ->
        isUp <- false
    | "KeyS" | "ArrowDown" ->
        isDown <- false
    | _ ->
        ()
