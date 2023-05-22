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

    let updateEggs (dt: float) (state: State) =
        let updateEgg (state: State) (egg: Egg) =
            let newPos = egg.Pos + 1
            if newPos > Egg.maxPos then
                let brokenEggPos =
                    let wolf = state.Wolf
                    match egg.Gutter, wolf.BodyPos, wolf.HandPos with
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
                        BrokenEgg = brokenEggPos
                    }

                let state =
                    match brokenEggPos with
                    | Some pos ->
                        let dir =
                            match pos with
                            | BrokenEggPos.Left ->
                                HatchedChickDirection.Left
                            | BrokenEggPos.Right ->
                                HatchedChickDirection.Right
                            | pos ->
                                failwithf "can't convert %A" pos

                        match state.Bunny.Status with
                        | BunnyStatus.Active ->
                            { state with
                                HatchedChick =
                                    Some (HatchedChick.create dir)
                            }
                        | _ ->
                            state
                    | None ->
                        state

                let state =
                    match brokenEggPos with
                    | None ->
                        { state with
                            CatchedEggsCount =
                                state.CatchedEggsCount + 1
                        }
                    | Some _ ->
                        let brokenEggValue =
                            match state.Bunny.Status with
                            | BunnyStatus.Active ->
                                BrokenEggIcon.Half (HalfBrokenEggIcon.create ())
                            | _ ->
                                BrokenEggIcon.Full

                        let brokenEggsBar = BrokenEggsBar.add brokenEggValue state.BrokenEggsBar
                        let state =
                            { state with
                                BrokenEggsBar =
                                    brokenEggsBar
                            }

                        if brokenEggsBar.Length < 3 then
                            state
                        else
                            { state with
                                Status = GameStatus.GameOver
                            }
                            |> State.mapBunny (fun bunny ->
                                bunny
                                |> Bunny.disableChangesStatus
                                |> Bunny.visible
                                |> Bunny.startRingingBell
                            )
                state
            else
                state
                |> State.mapEggsContainer (fun state ->
                    { state with
                        Eggs =
                            let egg =
                                { egg with
                                    Pos = newPos
                                }
                            Map.add egg.Id egg state.Eggs
                    }
                )

        let eggsContainer = state.EggsContainer
        if eggsContainer.TimeAcc > eggsContainer.Cooldown then
            let state =
                let initState =
                    { state with
                        BrokenEgg = None
                    }
                    |> State.mapEggsContainer (fun state ->
                        { state with
                            Eggs = Map.empty
                        }
                    )

                eggsContainer.Eggs
                |> Map.fold
                    (fun state _ egg ->
                        updateEgg state egg
                    )
                    initState

            state
            |> State.mapEggsContainer (fun state ->
                { state with
                    Eggs =
                        let newEgg =
                            Egg.create EggGutter.all.[r.Next(0, EggGutter.all.Length)]
                        Map.add newEgg.Id newEgg state.Eggs
                    TimeAcc = 0
                }
            )
        else
            state
            |> State.mapEggsContainer (fun state ->
                { state with
                    TimeAcc = dt + state.TimeAcc
                }
            )

    let updateBunny (dt: float) (state: State) =
        let bunny = state.Bunny

        let updateChangesStatus state =
            let f statusChangeTimeLeft newStatus =
                let timeLeft = bunny.StatusChangeTimeLeft
                if bunny.StatusChangeTimeLeft > 0 then
                    state
                    |> State.mapBunny (fun state ->
                        { state with
                            StatusChangeTimeLeft = timeLeft - dt
                        }
                    )
                else
                    state
                    |> State.mapBunny (fun state ->
                        { state with
                            StatusChangeTimeLeft = statusChangeTimeLeft
                            Status = newStatus
                        }
                    )

            if bunny.ChangesStatusDisabled then
                state
            else
                match bunny.Status with
                | BunnyStatus.Ready ->
                    f bunny.ActiveTime BunnyStatus.Active
                | BunnyStatus.Active ->
                    f bunny.CooldownTime BunnyStatus.Cooldown
                | BunnyStatus.Cooldown ->
                    f bunny.AutoActivateTime BunnyStatus.Ready

        let updateBellRing (state: State) =
            let bunny = state.Bunny
            match bunny.BellingRing with
            | None ->
                state
            | Some timer ->
                let bunny =
                    if Timer.isElapsed timer then
                        let newHandPos =
                            match bunny.HandPos with
                            | Some x ->
                                BunnyHandPos.switch x
                            | None ->
                                BunnyHandPos.Top
                        { bunny with
                            HandPos = Some newHandPos
                            BellingRing =
                                Timer.reset timer
                                |> Some
                        }
                    else
                        { bunny with
                            BellingRing =
                                Timer.update dt timer
                                |> Some
                        }
                { state with
                    Bunny = bunny
                }

        state
        |> updateChangesStatus
        |> updateBellRing

    let updateHatchedChick (dt: float) (state: State) =
        let hatchedChick = state.HatchedChick

        match hatchedChick with
        | Some hatchedChick ->
            let pos = hatchedChick.Pos + hatchedChick.Speed * dt
            if pos < HatchedChick.posesCount then
                state
                |> State.mapHatchedChick (fun hatchedChick ->
                    { hatchedChick with
                        Pos = pos
                    }
                )
            else
                { state with
                    HatchedChick = None
                }
        | None ->
            state

    let updateBrokenEggsBar (dt: float) (state: State) =
        state
        |> State.mapBrokenEggsBar (BrokenEggsBar.update dt)

    let update (dt: float) (state: State) =
        state
        |> updateEggs dt
        |> updateBunny dt
        |> updateHatchedChick dt
        |> updateBrokenEggsBar dt

module GraphicsSystem =
    let visible assetLabel assetsManager =
        AssetsManager.update assetLabel (Asset.map Sprite.visible) assetsManager

    let updateEggsContainer (assetsManager: AssetsManager) (state: State) =
        state.EggsContainer.Eggs
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

    let updateBrokenEgg (assetsManager: AssetsManager) (state: State) =
        match state.BrokenEgg with
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

    let updateWolf (assetsManager: AssetsManager) (state: State) =
        let updateBody assetsManager =
            let assetLabel =
                match state.Wolf.BodyPos with
                | WolfBodyPos.Left ->
                    AssetLabels.leftWolf
                | WolfBodyPos.Right ->
                    AssetLabels.rightWolf
                | x -> failwithf "expected `WolfPos.Left` or `WolfPos.Right` but `%A`" x

            visible assetLabel assetsManager

        let updateHands assetsManager =
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

        assetsManager
        |> updateBody
        |> updateHands

    let updateCachedEggsCount (assetsManager: AssetsManager) (state: State) =
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

    let updateBrokenEggsBar assetsManager state =
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

        state.BrokenEggsBar
        |> BrokenEggsBar.foldBack
            (fun (i, assetsManager) brokenEggIcon ->
                if i < assetsIds.Length then
                    let assetsManager =
                        match brokenEggIcon with
                        | BrokenEggIcon.Full ->
                            let assetId = assetsIds.[i]
                            draw assetId assetsManager
                        | BrokenEggIcon.Half x ->
                            if HalfBrokenEggIcon.isHidden x then
                                assetsManager
                            else
                                let assetId = assetsIds.[i]
                                draw assetId assetsManager
                    (i + 1, assetsManager)
                else
                    (i + 1, assetsManager)
            )
            (0, assetsManager)
        |> snd

    let updateBunny (assetsManager: AssetsManager) (state: State) =
        let bunny = state.Bunny
        let assetsManager =
            match bunny.Status with
            | BunnyStatus.Active ->
                visible AssetLabels.bunny assetsManager
            | _ ->
                assetsManager

        match bunny.HandPos with
        | None ->
            assetsManager
        | Some bunnyHandPos ->
            let assetId =
                match bunnyHandPos with
                | BunnyHandPos.Top ->
                    AssetLabels.bunnyTopHandWithBell
                | BunnyHandPos.Bottom ->
                    AssetLabels.bunnyBottomHandWithBell
                | x ->
                    failwithf "%A not implemented yet!" x

            visible assetId assetsManager

    let updateHatchedChick (assetsManager: AssetsManager) (state: State) =
        match state.HatchedChick with
        | None ->
            assetsManager
        | Some hatchedChick ->
            let assetGroupId =
                match hatchedChick.Direction with
                | HatchedChickDirection.Left ->
                    AssetLabels.leftChickens
                | HatchedChickDirection.Right ->
                    AssetLabels.rightChickens
                | dir ->
                    failwithf "not found asset group ID for \"%A\" direction" dir

            assetsManager
            |> AssetsManager.update assetGroupId (fun asset ->
                let id = string (int hatchedChick.Pos)
                asset
                |> Asset.updateGroup id (Asset.map Sprite.visible)
            )

    let update (assetsManager: AssetsManager) (state: State) =
        let assetsManager = AssetsManager.hideAll assetsManager

        let f fn assetsManager =
            fn assetsManager state

        assetsManager
        |> f updateEggsContainer
        |> f updateBrokenEgg
        |> f updateWolf
        |> f updateCachedEggsCount
        |> f updateBrokenEggsBar
        |> f updateBunny
        |> f updateHatchedChick
        |> fun assetsManager ->
            assetsManager, state
