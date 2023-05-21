namespace NuPogodiElectronics

module PlayerInputSystem =
    let update (state: State) =
        let wolf = state.Wolf

        let wolf =
            if Joystick.isLeft then
                { wolf with
                    BodyPos = WolfBodyPos.Left
                }
            else
                wolf

        let wolf =
            if Joystick.isRight then
                { wolf with
                    BodyPos = WolfBodyPos.Right
                }
            else
                wolf

        let wolf =
            if Joystick.isUp then
                { wolf with
                    HandPos = WolfHandPos.Top
                }
            else
                wolf

        let wolf =
            if Joystick.isDown then
                { wolf with
                    HandPos = WolfHandPos.Bottom
                }
            else
                wolf

        { state with
            Wolf = wolf
        }

module PhysicsSystem =
    let r = System.Random()

    let update (dt: float) (state: State) =
        if state.TimeAcc > state.Cooldown then
            let state =
                state.Eggs
                |> Map.fold
                    (fun state id egg ->
                        let newPos = egg.Pos + 1
                        if newPos > Egg.maxPos then
                            let brokenEggPos =
                                match egg.Gutter, state.Wolf.BodyPos, state.Wolf.HandPos with
                                | EggGutter.LeftTop, WolfBodyPos.Left, WolfHandPos.Top ->
                                    None
                                | EggGutter.LeftTop, _, _ ->
                                    Some BrokenEggPos.Left
                                | EggGutter.LeftBottom, WolfBodyPos.Left, WolfHandPos.Bottom ->
                                    None
                                | EggGutter.LeftBottom, _, _ ->
                                    Some BrokenEggPos.Left
                                | EggGutter.RightTop, WolfBodyPos.Right, WolfHandPos.Top ->
                                    None
                                | EggGutter.RightTop, _, _ ->
                                    Some BrokenEggPos.Right
                                | EggGutter.RightBottom, WolfBodyPos.Right, WolfHandPos.Bottom ->
                                    None
                                | EggGutter.RightBottom, _, _ ->
                                    Some BrokenEggPos.Right
                                | x -> failwithf "not found EggGutter.%A" x

                            let state =
                                { state with
                                    BrokenEggPos = brokenEggPos
                                }

                            let state =
                                match brokenEggPos with
                                | None ->
                                    { state with
                                        CatchedEggsCount =
                                            state.CatchedEggsCount + 1
                                    }
                                | Some _ ->
                                    { state with
                                        BrokenEggsCount =
                                            state.BrokenEggsCount + 1
                                    }
                            state
                        else
                            { state with
                                Eggs =
                                    let egg =
                                        { egg with
                                            Pos = newPos
                                        }
                                    Map.add id egg state.Eggs
                            }
                    )
                    { state with
                        Eggs = Map.empty
                        BrokenEggPos = None
                    }

            let state =
                { state with
                    Eggs =
                        let newEgg =
                            Egg.create EggGutter.all.[r.Next(0, 4)]

                        Map.add newEgg.Id newEgg state.Eggs
                }

            { state with
                TimeAcc = 0
            }
        else
            { state with
                TimeAcc = dt + state.TimeAcc
            }

module GraphicsSystem =
    let update (assetsManager: AssetsManager) (state: State) =
        let assetsManager = AssetsManager.hideAll assetsManager

        let visible assetLabel assetsManager =
            AssetsManager.update assetLabel (Asset.map Sprite.visible) assetsManager

        let assetsManager =
            state.Eggs
            |> Map.fold
                (fun (assetsManager: AssetsManager) id egg ->
                    let eggAssets =
                        match egg.Gutter with
                        | EggGutter.LeftTop ->
                            AssetLabels.leftTopEggs
                        | EggGutter.LeftBottom ->
                            AssetLabels.leftBottomEggs
                        | EggGutter.RightTop ->
                            AssetLabels.rightTopEggs
                        | EggGutter.RightBottom ->
                            AssetLabels.rightBottomEggs
                        | x -> failwithf "not found `EggGutter.%A`" x

                    assetsManager
                    |> AssetsManager.update eggAssets (
                        Asset.updateGroup
                            (string egg.Pos)
                            (Asset.map Sprite.visible)
                    )
                )
                assetsManager

        let assetsManager =
            match state.BrokenEggPos with
            | Some x ->
                let assetLabel =
                    match x with
                    | BrokenEggPos.Left ->
                        AssetLabels.leftBrokenEgg
                    | BrokenEggPos.Right ->
                        AssetLabels.rightBrokenEgg
                    | x -> failwithf "expected `BrokenEggPos.Left` or `BrokenEggPos.Right` but `%A`" x

                visible assetLabel assetsManager
            | None ->
                assetsManager

        let assetsManager =
            let assetLabel =
                match state.Wolf.BodyPos with
                | WolfBodyPos.Left ->
                    AssetLabels.leftWolf
                | WolfBodyPos.Right ->
                    AssetLabels.rightWolf
                | x -> failwithf "expected `WolfPos.Left` or `WolfPos.Right` but `%A`" x

            visible assetLabel assetsManager

        let assetsManager =
            let assetLabel =
                let wolf = state.Wolf
                match wolf.BodyPos, wolf.HandPos with
                | WolfBodyPos.Left, WolfHandPos.Top ->
                    AssetLabels.leftWolfTopHand
                | WolfBodyPos.Left, WolfHandPos.Bottom ->
                    AssetLabels.leftWolfBottomHand
                | WolfBodyPos.Right, WolfHandPos.Top ->
                    AssetLabels.rightWolfTopHand
                | WolfBodyPos.Right, WolfHandPos.Bottom ->
                    AssetLabels.rightWolfBottomHand
                | x -> failwithf "expected `WolfPos.Left` or `WolfPos.Right` but `%A`" x

            visible assetLabel assetsManager

        let assetsManager =
            let draw assetId displayNumber assetsManager =
                assetsManager
                |> AssetsManager.update assetId (fun asset ->
                    (state.CatchedEggsCount / (pown 10 (displayNumber - 1))) % 10
                    |> SegmentDisplay.ofDigit
                    |> SegmentDisplay.toAssetNames
                    |> Array.fold
                        (fun st assetId ->
                            st
                            |> Asset.updateGroup assetId (Asset.map Sprite.visible)
                        )
                        asset
                )
            assetsManager
            |> draw AssetLabels.digit1 1
            |> draw AssetLabels.digit2 2
            |> draw AssetLabels.digit3 3
            |> draw AssetLabels.digit4 4

        let assetsManager =
            let draw assetId assetsManager =
                assetsManager
                |> AssetsManager.update assetId (
                    Asset.map Sprite.visible
                )

            let assetsIds =
                [|
                    AssetLabels.brokenEgg1
                    AssetLabels.brokenEgg2
                    AssetLabels.brokenEgg3
                |]

            let rec f assetsManager i length =
                if i < length then
                    let assetId = assetsIds.[i]
                    let acc = draw assetId assetsManager
                    f acc (i + 1) length
                else
                    assetsManager
            f assetsManager 0 (state.BrokenEggsCount % (assetsIds.Length + 1))

        assetsManager, state
