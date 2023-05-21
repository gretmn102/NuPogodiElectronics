namespace NuPogodiElectronics
type EggId = System.Guid
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EggId =
    let create () =
        System.Guid.NewGuid()

type EggGutter =
    | LeftTop = 0
    | LeftBottom = 1
    | RightTop = 2
    | RightBottom = 3
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EggGutter =
    let all =
        // System.Enum.GetValues<EggGutter>() // error in Fable 4.1.3
        System.Enum.GetValues(typeof<EggGutter>) :?> EggGutter array

type Egg =
    {
        Id: EggId
        /// [1..5]
        Pos: int
        Gutter: EggGutter
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Egg =
    let maxPos = 5

    let create assetId =
        {
            Id = EggId.create ()
            Pos = 1
            Gutter = assetId
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

type EggsContainer =
    {
        Eggs: Map<EggId, Egg>
        Cooldown: float
        TimeAcc: float
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EggsContainer =
    let create () =
        {
            Eggs = Map.empty
            Cooldown = 1000.0
            TimeAcc = 0.0
        }

[<RequireQualifiedAccess;Struct>]
type BunnyStatus =
    | Active
    | Cooldown
    | Ready

type Bunny =
    {
        Status: BunnyStatus
        CooldownTime: float
        ActiveTime: float
        AutoActivateTime: float

        StatusChangeTimeLeft: float
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Bunny =
    let create () : Bunny =
        {
            Status = BunnyStatus.Ready
            CooldownTime = 1.0 * 1000.0
            ActiveTime = 3.0 * 1000.0
            AutoActivateTime = 10.0 * 1000.0

            StatusChangeTimeLeft = 0.0
        }

type HatchedChickDirection =
    | Left = 0
    | Right = 1

type HatchedChick =
    {
        Direction: HatchedChickDirection
        Speed: float
        Pos: float
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HatchedChick =
    let posesCount = 4

    let create corner =
        {
            Direction = corner
            Speed = 0.002
            Pos = 0.0
        }

type HalfBrokenEggIcon =
    {
        Speed: float
        /// <1 — hidden, >=1 — visible
        Visible: float
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HalfBrokenEggIcon =
    let create () =
        {
            Speed = 0.002
            Visible = 1.0
        }

    let isHidden (icon: HalfBrokenEggIcon) =
        icon.Visible < 1.0

    let update (dt: float) (icon: HalfBrokenEggIcon) =
        { icon with
            Visible = (icon.Speed * dt + icon.Visible) % 2.0
        }

[<RequireQualifiedAccess>]
type BrokenEggIcon =
    | Full
    | Half of HalfBrokenEggIcon

type BrokenEggsBar =
    {
        List: BrokenEggIcon list
        Length: float
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module BrokenEggsBar =
    let empty : BrokenEggsBar =
        {
            List = []
            Length = 0.0
        }

    let add (icon: BrokenEggIcon) (brokenEggsBar: BrokenEggsBar) =
        { brokenEggsBar with
            List =
                match brokenEggsBar.List with
                | (BrokenEggIcon.Half _ as curr)::xs ->
                    match icon with
                    | BrokenEggIcon.Full ->
                        curr::icon::xs
                    | BrokenEggIcon.Half(_) ->
                        BrokenEggIcon.Full::xs
                | _ ->
                    icon :: brokenEggsBar.List
            Length =
                let value =
                    match icon with
                    | BrokenEggIcon.Full -> 1.0
                    | BrokenEggIcon.Half _ -> 0.5
                value + brokenEggsBar.Length
        }

    let fold folder state (brokenEggsBar: BrokenEggsBar) =
        List.fold folder state brokenEggsBar.List

    let foldBack folder state (brokenEggsBar: BrokenEggsBar) =
        List.foldBack (fun st x -> folder x st) brokenEggsBar.List state

    let update (dt: float) (brokenEggBar: BrokenEggsBar) =
        match brokenEggBar.List with
        | (BrokenEggIcon.Half half)::xs ->
            let x =
                HalfBrokenEggIcon.update dt half
                |> BrokenEggIcon.Half

            { brokenEggBar with
                List =
                    x::xs
            }

        | _ ->
            brokenEggBar
