module NuPogodiElectronics.Game
open Browser

let update (dt: float) (assetsManager: AssetsManager) (state: State) =
    match state.Status with
    | GameStatus.Playing ->
        state
        |> PlayerInputSystem.update
        |> PhysicsSystem.update dt
        |> GraphicsSystem.update assetsManager

    | GameStatus.GameOver ->
        if Ui.isStartGameAButtonPressed then
            let state =
                { state with
                    Status = GameStatus.HasNotStartedYet
                }
            assetsManager, state
        else
            state
            |> PhysicsSystem.updateBunny dt
            |> GraphicsSystem.update assetsManager

    | GameStatus.HasNotStartedYet ->
        if Ui.isStartGameAButtonPressed then
            let state =
                let state = State.create ()
                { state with
                    Status = GameStatus.Playing
                }
            assetsManager, state
        else
            assetsManager, state

let start (assetsManager: AssetsManager) =
    let rec loop (previousTimeStamp, assetsManager, state) =
        window.requestAnimationFrame (fun timeStamp ->
            let dt = timeStamp - previousTimeStamp
            let assetsManager, state = update dt assetsManager state
            loop (timeStamp, assetsManager, state)
        )
        |> ignore

    Ui.init assetsManager

    let initState = State.create ()

    loop (0.0, assetsManager, initState)
