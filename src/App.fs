module NuPogodiElectronics.App
open Browser.Dom
open Browser.Types
open Browser.XMLDom

type EggId = System.Guid
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EggId =
    let create () =
        System.Guid.NewGuid()

let maxPos = 5

type Egg =
    {
        Id: EggId
        /// [1..5]
        Pos: int
        AssetId: string
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Egg =
    let create assetId =
        {
            Id = EggId.create ()
            Pos = 1
            AssetId = assetId
        }

    let move (egg: Egg) =
        { egg with
            Pos = egg.Pos + 1
        }

type WolfBodyPos =
    | Left = 0
    | Right = 1

type WolfHandPos =
    | Top = 0
    | Bottom = 1

type Wolf =
    {
        BodyPos: WolfBodyPos
        HandPos: WolfHandPos
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Wolf =
    let create () =
        {
            BodyPos = WolfBodyPos.Left
            HandPos = WolfHandPos.Top
        }

type State =
    {
        Eggs: Map<EggId, Egg>
        Wolf: Wolf
        Cooldown: float
        TimeAcc: float
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module State =
    let create () =
        {
            Eggs = Map.empty
            Cooldown = 1000.0
            TimeAcc = 0.0
            Wolf = Wolf.create ()
        }

type Sprite =
    {
        SvgElement: SVGElement
        IsHidden: bool
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Sprite =
    let create svg =
        {
            SvgElement = svg
            IsHidden =
                match svg.getAttribute("visibility") with
                | null -> false
                | "hidden" -> true
                | _ -> false
        }

    let hide (sprite: Sprite) =
        if sprite.IsHidden then
            sprite
        else
            sprite.SvgElement.setAttribute("visibility", "hidden")
            { sprite with IsHidden = true }

    let visible (sprite: Sprite) =
        if sprite.IsHidden then
            sprite.SvgElement.setAttribute("visibility", "")
            { sprite with IsHidden = false }
        else
            sprite

[<RequireQualifiedAccess>]
type Asset =
    | Group of Map<string, Asset>
    | Node of Sprite
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Asset =
    let iter fn (asset: Asset) =
        let rec loop = function
            | Asset.Group m ->
                m
                |> Map.iter (fun _ asset ->
                    loop asset
                )
            | Asset.Node x ->
                fn x
        loop asset

    let map fn (asset: Asset) =
        let rec loop = function
            | Asset.Group m ->
                m
                |> Map.map (fun _ asset ->
                    loop asset
                )
                |> Asset.Group
            | Asset.Node x ->
                Asset.Node (fn x)
        loop asset

module AssetLabels =
    let rec rightTopEggs = nameof rightTopEggs
    let rec rightBottomEggs = nameof rightBottomEggs
    let rec leftTopEggs = nameof leftTopEggs
    let rec leftBottomEggs = nameof leftBottomEggs
    let rec leftWolf = nameof leftWolf
    let rec rightWolf = nameof rightWolf
    let rec leftWolfTopHand = nameof leftWolfTopHand
    let rec leftWolfBottomHand = nameof leftWolfBottomHand
    let rec rightWolfTopHand = nameof rightWolfTopHand
    let rec rightWolfBottomHand = nameof rightWolfBottomHand

type AssetsManager =
    {
        Container: Map<string, Asset>
        Root: SVGElement
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module AssetsManager =
    let create (root: SVGElement) : AssetsManager =
        let findByLabel label (root: Element) =
            let createPattern name =
                $"[*|label=\"{name}\"]"
            root.querySelector (createPattern label) :?> SVGElement
            |> Option.ofObj
            |> Option.defaultWith (fun x ->
                failwithf "not found %s" label
            )

        let loadEggs groupName =
            let group = findByLabel groupName root
            let group =
                [1..5]
                |> List.map (fun i ->
                    let id = string i
                    findByLabel id group
                    |> Sprite.create
                    |> fun sprite -> id, Asset.Node sprite
                )
                |> Map.ofList
            groupName, Asset.Group group

        let load name =
            let x = findByLabel name root
            name, Asset.Node (Sprite.create x)

        {
            Root = root
            Container =
                [
                    loadEggs AssetLabels.rightTopEggs
                    loadEggs AssetLabels.rightBottomEggs
                    loadEggs AssetLabels.leftTopEggs
                    loadEggs AssetLabels.leftBottomEggs
                    load AssetLabels.leftWolf
                    load AssetLabels.rightWolf
                    load AssetLabels.leftWolfTopHand
                    load AssetLabels.leftWolfBottomHand
                    load AssetLabels.rightWolfTopHand
                    load AssetLabels.rightWolfBottomHand
                ]
                |> Map.ofList
        }

    let hideAll (assetsManager: AssetsManager) =
        { assetsManager with
            Container =
                assetsManager.Container
                |> Map.map (fun _ ->
                    Asset.map Sprite.hide
                )
        }

    let tryFind id (assetsManager: AssetsManager) =
        Map.tryFind id assetsManager.Container

    let add id x (assetsManager: AssetsManager) =
        { assetsManager with
            Container =
                Map.add id x assetsManager.Container
        }

let updatePlayerInput (state: State) =
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

let updateGraphics (assetsManager: AssetsManager) (state: State) =
    let assetsManager = AssetsManager.hideAll assetsManager

    let assetsManager =
        state.Eggs
        |> Map.fold
            (fun (assetsManager: AssetsManager) id egg ->
                match AssetsManager.tryFind egg.AssetId assetsManager with
                | Some (Asset.Group group) ->
                    let posId = string egg.Pos
                    let sprite =
                        match Map.find posId group with
                        | Asset.Node sprite ->
                            Sprite.visible sprite
                            |> Asset.Node
                        | x ->
                            failwithf "expected `Asset.Node sprite` but `%A`" x
                    let group =
                        Map.add posId sprite group
                        |> Asset.Group

                    AssetsManager.add egg.AssetId group assetsManager
                | x ->
                    failwithf "expected `Some (Asset.Group group)` but `%A`" x
            )
            assetsManager

    let assetsManager =
        let assetLabel =
            match state.Wolf.BodyPos with
            | WolfBodyPos.Left ->
                AssetLabels.leftWolf
            | WolfBodyPos.Right ->
                AssetLabels.rightWolf
            | x -> failwithf "expected `WolfPos.Left` or `WolfPos.Right` but `%A`" x

        match AssetsManager.tryFind assetLabel assetsManager with
        | Some (Asset.Node x) ->
            let x = Asset.Node (Sprite.visible x)
            AssetsManager.add assetLabel x assetsManager
        | x ->
            failwithf "expected `Some (Asset.Node x)` but `%A`" x

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

        match AssetsManager.tryFind assetLabel assetsManager with
        | Some (Asset.Node x) ->
            let x = Asset.Node (Sprite.visible x)
            AssetsManager.add assetLabel x assetsManager
        | x ->
            failwithf "expected `Some (Asset.Node x)` but `%A`" x

    assetsManager, state

let r = System.Random()

let updatePhysics (dt: float) (state: State) =
    if state.TimeAcc > state.Cooldown then
        let state =
            { state with
                Eggs =
                    state.Eggs
                    |> Map.fold
                        (fun st id egg ->
                            let newPos = egg.Pos + 1
                            if newPos > maxPos then
                                st
                            else
                                let egg =
                                    { egg with
                                        Pos = newPos
                                    }
                                Map.add id egg st
                        )
                        Map.empty
            }

        let state =
            { state with
                Eggs =
                    let eggAssets = [|
                        AssetLabels.rightTopEggs
                        AssetLabels.rightBottomEggs
                        AssetLabels.leftTopEggs
                        AssetLabels.leftBottomEggs
                    |]

                    let newEgg =
                        Egg.create eggAssets.[r.Next(0, 4)]

                    Map.add newEgg.Id newEgg state.Eggs
            }

        { state with
            TimeAcc = 0
        }
    else
        { state with
            TimeAcc = dt + state.TimeAcc
        }

let update (dt: float) (assetsManager: AssetsManager) (state: State) =
    state
    |> updatePlayerInput
    |> updatePhysics dt
    |> updateGraphics assetsManager

let startGame (assetsManager: AssetsManager) =
    let rec loop (previousTimeStamp, assetsManager, state) =
        window.requestAnimationFrame (fun timeStamp ->
            let dt = timeStamp - previousTimeStamp
            let assetsManager, state = update dt assetsManager state
            loop (timeStamp, assetsManager, state)
        )
        |> ignore

    let initState = State.create ()

    loop (0.0, assetsManager, initState)

let canvas : HTMLDivElement = document.createElement("div") :?> HTMLDivElement

let appNode = document.querySelector "#app"
appNode.appendChild canvas
|> ignore

canvas.innerText <- "Loading..."

let loadSvg () =
    let url = "/assets/assets.svg"
    Fetch.fetch url []
    |> Promise.bind (fun x ->
        x.text()
    )
    |> Promise.map (fun rawSvg ->
        let parser = DOMParser.Create ()
        let doc = parser.parseFromString(rawSvg, "image/svg+xml")
        doc.documentElement :?> SVGElement
        |> Ok
    )
    |> Promise.catch (fun x ->
        Error (sprintf "error load %s\n%s" url x.Message)
    )

loadSvg ()
|> Promise.map (fun x ->
    match x with
    | Ok svg ->
        canvas.removeChild canvas.firstChild |> ignore
        canvas.appendChild svg
        |> ignore

        let assetsManager = AssetsManager.create svg

        startGame assetsManager
    | Error msg ->
        canvas.innerText <- msg
)
|> ignore
