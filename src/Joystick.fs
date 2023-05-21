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

let initUiButtons (assetsManager: AssetsManager) =
    let bind assetId binding =
        assetsManager
        |> AssetsManager.iterById assetId (
            Asset.iter (fun sprite ->
                let svgElement = sprite.SvgElement
                svgElement.onmousedown <- fun x ->
                    binding true

                svgElement.onmouseup <- fun x ->
                    binding false

                svgElement.onmouseout <- fun x ->
                    binding false

                svgElement.ontouchstart <- fun x ->
                    binding true

                svgElement.ontouchend <- fun x ->
                    binding false
            )
        )

    bind AssetLabels.leftTopButton (fun isPress ->
        isLeft <- isPress
        isUp <- isPress
    )

    bind AssetLabels.leftBottomButton (fun isPress ->
        isLeft <- isPress
        isDown <- isPress
    )

    bind AssetLabels.rightTopButton (fun isPress ->
        isRight <- isPress
        isUp <- isPress
    )

    bind AssetLabels.rightBottomButton (fun isPress ->
        isRight <- isPress
        isDown <- isPress
    )
