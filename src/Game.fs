module NuPogodiElectronics.Game
open Browser

let update (dt: float) (assetsManager: AssetsManager) (state: State) =
    state
    |> PlayerInputSystem.update
    |> PhysicsSystem.update dt
    |> GraphicsSystem.update assetsManager

let start (assetsManager: AssetsManager) =
    let rec loop (previousTimeStamp, assetsManager, state) =
        window.requestAnimationFrame (fun timeStamp ->
            let dt = timeStamp - previousTimeStamp
            let assetsManager, state = update dt assetsManager state
            loop (timeStamp, assetsManager, state)
        )
        |> ignore

    Ui.initKeyButtons assetsManager

    let initState = State.create ()

    loop (0.0, assetsManager, initState)
