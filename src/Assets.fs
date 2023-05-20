namespace NuPogodiElectronics
open Browser.Types

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
    let rec rightBrokenEgg = nameof rightBrokenEgg
    let rec leftBrokenEgg = nameof leftBrokenEgg

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
                    load AssetLabels.leftBrokenEgg
                    load AssetLabels.rightBrokenEgg
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
