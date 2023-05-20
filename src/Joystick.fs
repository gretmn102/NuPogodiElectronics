module NuPogodiElectronics.Joystick
open Browser.Dom

let mutable isRight = false
let mutable isLeft = false
let mutable isUp = false
let mutable isDown = false

document.onkeydown <- fun x ->
    match x.code with
    | "KeyD" ->
        isRight <- true
    | "KeyA" ->
        isLeft <- true
    | "KeyW" ->
        isUp <- true
    | "KeyS" ->
        isDown <- true
    | _ ->
        ()

document.onkeyup <- fun x ->
    match x.code with
    | "KeyD" ->
        isRight <- false
    | "KeyA" ->
        isLeft <- false
    | "KeyW" ->
        isUp <- false
    | "KeyS" ->
        isDown <- false
    | _ ->
        ()
