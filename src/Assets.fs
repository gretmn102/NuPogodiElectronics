namespace NuPogodiElectronics
open Browser.Types

type Sprite =
    {
        Id: string
        SvgElement: SVGElement
        IsHidden: bool
        IsVisibleLock: bool
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Sprite =
    let create isVisibleLock (svg: SVGElement) =
        {
            Id = svg.getAttribute("inkscape:label")
            SvgElement = svg
            IsHidden =
                match svg.getAttribute("visibility") with
                | null -> false
                | "hidden" -> true
                | _ -> false
            IsVisibleLock = isVisibleLock
        }

    let hide (sprite: Sprite) =
        if sprite.IsVisibleLock then
            sprite
        elif sprite.IsHidden then
            sprite
        else
            sprite.SvgElement.setAttribute("visibility", "hidden")
            { sprite with IsHidden = true }

    let visible (sprite: Sprite) =
        if sprite.IsVisibleLock then
            sprite
        elif sprite.IsHidden then
            sprite.SvgElement.setAttribute("visibility", "")
            { sprite with IsHidden = false }
        else
            sprite

type SegmentDisplay =
    | None = 0
    | Top = 1
    | LeftTop = 2
    | RightTop = 4
    | Middle = 8
    | LeftBottom = 16
    | RightBottom = 32
    | Bottom = 64
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module SegmentDisplay =
    let all =
        System.Enum.GetValues(typeof<SegmentDisplay>) :?> SegmentDisplay array

    let allExceptNone =
            all
            |> Array.filter (fun x -> x <> SegmentDisplay.None)

    let ofDigit n =
        match n with
        | 0 ->
            SegmentDisplay.Top
            ||| SegmentDisplay.LeftTop
            ||| SegmentDisplay.RightTop
            ||| SegmentDisplay.LeftBottom
            ||| SegmentDisplay.RightBottom
            ||| SegmentDisplay.Bottom
        | 1 ->
            SegmentDisplay.RightTop
            ||| SegmentDisplay.RightBottom
        | 2 ->
            SegmentDisplay.Top
            ||| SegmentDisplay.RightTop
            ||| SegmentDisplay.Middle
            ||| SegmentDisplay.LeftBottom
            ||| SegmentDisplay.Bottom
        | 3 ->
            SegmentDisplay.Top
            ||| SegmentDisplay.RightTop
            ||| SegmentDisplay.Middle
            ||| SegmentDisplay.RightBottom
            ||| SegmentDisplay.Bottom
        | 4 ->
            SegmentDisplay.LeftTop
            ||| SegmentDisplay.RightTop
            ||| SegmentDisplay.Middle
            ||| SegmentDisplay.RightBottom
        | 5 ->
            SegmentDisplay.Top
            ||| SegmentDisplay.LeftTop
            ||| SegmentDisplay.Middle
            ||| SegmentDisplay.RightBottom
            ||| SegmentDisplay.Bottom
        | 6 ->
            SegmentDisplay.Top
            ||| SegmentDisplay.LeftTop
            ||| SegmentDisplay.Middle
            ||| SegmentDisplay.LeftBottom
            ||| SegmentDisplay.RightBottom
            ||| SegmentDisplay.Bottom
        | 7 ->
            SegmentDisplay.Top
            ||| SegmentDisplay.RightTop
            ||| SegmentDisplay.RightBottom
        | 8 ->
            SegmentDisplay.Top
            ||| SegmentDisplay.LeftTop
            ||| SegmentDisplay.RightTop
            ||| SegmentDisplay.Middle
            ||| SegmentDisplay.LeftBottom
            ||| SegmentDisplay.RightBottom
            ||| SegmentDisplay.Bottom
        | 9 ->
            SegmentDisplay.Top
            ||| SegmentDisplay.LeftTop
            ||| SegmentDisplay.RightTop
            ||| SegmentDisplay.Middle
            ||| SegmentDisplay.RightBottom
            ||| SegmentDisplay.Bottom
        | _ ->
            SegmentDisplay.None

    let toAssetName (sd: SegmentDisplay) =
        match sd with
        | SegmentDisplay.Top -> "top"
        | SegmentDisplay.LeftTop -> "leftTop"
        | SegmentDisplay.RightTop -> "rightTop"
        | SegmentDisplay.Middle -> "middle"
        | SegmentDisplay.LeftBottom -> "leftBottom"
        | SegmentDisplay.RightBottom -> "rightBottom"
        | SegmentDisplay.Bottom -> "bottom"
        | x ->
            failwithf "not found asset name for %A" x

    let toAssetNames (sd: SegmentDisplay) =
        let test t =
            sd &&& t = t

        allExceptNone
        |> Array.choose (fun x ->
            if test x then
                Some (toAssetName x)
            else
                None
        )

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

    let updateGroup assetName updating = function
        | Asset.Group group ->
            let asset =
                Map.find assetName group
                |> updating

            Map.add assetName asset group
            |> Asset.Group
        | x ->
            failwithf "expected group in \"%s\" asset id but `%A`" assetName x

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
    let rec rightBrokenEgg = nameof rightBrokenEgg
    let rec leftBrokenEgg = nameof leftBrokenEgg
    let rec digit1 = nameof digit1
    let rec digit2 = nameof digit2
    let rec digit3 = nameof digit3
    let rec digit4 = nameof digit4
    let rec leftTopButton = nameof leftTopButton
    let rec rightTopButton = nameof rightTopButton
    let rec leftBottomButton = nameof leftBottomButton
    let rec rightBottomButton = nameof rightBottomButton

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
                    |> Sprite.create false
                    |> fun sprite -> id, Asset.Node sprite
                )
                |> Map.ofList
            groupName, Asset.Group group

        let load name =
            let x = findByLabel name root
            name, Asset.Node (Sprite.create false x)

        let loadVisibleLock name =
            let x = findByLabel name root
            name, Asset.Node (Sprite.create true x)

        let loadSegmentDisplay groupName =
            let group = findByLabel groupName root
            let group =
                SegmentDisplay.allExceptNone
                |> Array.map (fun x ->
                    let id = SegmentDisplay.toAssetName x
                    findByLabel id group
                    |> Sprite.create false
                    |> fun sprite -> id, Asset.Node sprite
                )
                |> Map.ofArray
            groupName, Asset.Group group

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
                    load AssetLabels.leftBrokenEgg
                    load AssetLabels.rightBrokenEgg
                    loadSegmentDisplay AssetLabels.digit1
                    loadSegmentDisplay AssetLabels.digit2
                    loadSegmentDisplay AssetLabels.digit3
                    loadSegmentDisplay AssetLabels.digit4
                    loadVisibleLock AssetLabels.leftTopButton
                    loadVisibleLock AssetLabels.rightTopButton
                    loadVisibleLock AssetLabels.leftBottomButton
                    loadVisibleLock AssetLabels.rightBottomButton
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

    let update assetLabel updating assetsManager =
        match tryFind assetLabel assetsManager with
        | Some asset ->
            let asset = updating asset
            add assetLabel asset assetsManager
        | x ->
            failwithf "expected `Some _` but `%A`" x

    let iterById assetId updating assetsManager =
        match tryFind assetId assetsManager with
        | Some asset ->
            updating asset
        | x ->
            failwithf "expected `Some _` but `%A`" x
