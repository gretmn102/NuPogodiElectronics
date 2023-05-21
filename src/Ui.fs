module NuPogodiElectronics.Ui

let initKeyButtons (assetsManager: AssetsManager) =
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
