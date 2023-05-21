module NuPogodiElectronics.Ui

let mutable isStartGameAButtonPressed = false

let bind assetId binding assetsManager =
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

let initKeyButtons (assetsManager: AssetsManager) =
    let bind assetId binding = bind assetId binding assetsManager

    bind AssetLabels.leftTopButton (fun isPress ->
        Joystick.isLeft <- isPress
        Joystick.isUp <- isPress
    )

    bind AssetLabels.leftBottomButton (fun isPress ->
        Joystick.isLeft <- isPress
        Joystick.isDown <- isPress
    )

    bind AssetLabels.rightTopButton (fun isPress ->
        Joystick.isRight <- isPress
        Joystick.isUp <- isPress
    )

    bind AssetLabels.rightBottomButton (fun isPress ->
        Joystick.isRight <- isPress
        Joystick.isDown <- isPress
    )

let initStartGameAButton (assetsManager: AssetsManager) =
    assetsManager
    |> bind AssetLabels.gameAButton (fun isPressed ->
        isStartGameAButtonPressed <- isPressed
    )

let init (assetsManager: AssetsManager) =
    assetsManager
    |> initKeyButtons

    assetsManager
    |> initStartGameAButton
