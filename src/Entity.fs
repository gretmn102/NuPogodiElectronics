namespace NuPogodiElectronics
type Timer =
    {
        Interval: float
        TimeLeft: float
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Timer =
    let create interval =
        {
            Interval = interval
            TimeLeft = interval
        }

    let isElapsed (timerComponent: Timer) =
        not (timerComponent.TimeLeft > 0)

    let reset (timerComponent: Timer) =
        { timerComponent with
            TimeLeft = timerComponent.Interval
        }

    let update (dt: float) (timerComponent: Timer) =
        { timerComponent with
            TimeLeft = timerComponent.TimeLeft - dt
        }

type EggId = System.Guid
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EggId =
    let create () =
        System.Guid.NewGuid()

type EggGutterCorner =
    | LeftTop = 0
    | LeftBottom = 1
    | RightTop = 2
    | RightBottom = 3
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EggGutterCorner =
    let all =
        // System.Enum.GetValues<EggGutter>() // error in Fable 4.1.3
        System.Enum.GetValues(typeof<EggGutterCorner>) :?> EggGutterCorner array

[<RequireQualifiedAccess>]
type EggGutterStatus =
    | HasNotStartedYet of Timer
    | Started

type EggGutter =
    {
        Status: EggGutterStatus
        Corner: EggGutterCorner
        EggSlots: bool []
        MovementTimer: Timer
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EggGutter =
    let create corner startAt interval eggsCount =
        {
            Status =
                Timer.create startAt
                |> EggGutterStatus.HasNotStartedYet
            EggSlots = Array.zeroCreate eggsCount
            Corner = corner
            MovementTimer = Timer.create interval
        }

    let isLast ({ EggSlots = eggSlots }: EggGutter) =
        eggSlots.[eggSlots.Length - 1]

    let move (isCreateNewEgg: bool) (eggGutter: EggGutter) : EggGutter =
        let eggSlots = eggGutter.EggSlots
        let eggSlotsLength = eggSlots.Length
        let newEggSlots = Array.zeroCreate eggSlotsLength

        newEggSlots.[0] <- isCreateNewEgg

        for i = 0 to eggSlotsLength - 2 do
            newEggSlots.[i + 1] <- eggSlots.[i]

        let eggGutter =
            { eggGutter with
                EggSlots = newEggSlots
            }

        eggGutter

    /// `isMoved * EggGutter`
    let update (dt: float) isCreateNewEgg (eggGutter: EggGutter) : bool * EggGutter =
        let update () =
            let timer = Timer.update dt eggGutter.MovementTimer
            let isMoved = Timer.isElapsed timer
            let eggGutter =
                if isMoved then
                    { eggGutter with
                        MovementTimer = Timer.reset timer
                    }
                    |> move isCreateNewEgg
                else
                    { eggGutter with
                        MovementTimer = timer
                    }
            isMoved, eggGutter

        match eggGutter.Status with
        | EggGutterStatus.Started ->
            update ()
        | EggGutterStatus.HasNotStartedYet timer ->
            let timer = Timer.update dt timer

            let newStatus =
                if Timer.isElapsed timer then
                    EggGutterStatus.Started
                else
                    EggGutterStatus.HasNotStartedYet timer

            let eggGutter =
                { eggGutter with
                    Status = newStatus
                }
            false, eggGutter

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

module ArrayExt =
    let insertAt idx newElement (xs: _ []) =
        xs
        |> Array.mapi (fun i current ->
            if idx = i then newElement
            else current
        )

type EggsContainer =
    {
        Eggs: EggGutter []
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EggsContainer =
    let eggsCount = 5

    let create () =
        let interval = 1000.0
        {
            Eggs =
                EggGutterCorner.all
                |> Array.mapi (fun i corner ->
                    let startAt = float (i + 1) * (interval / 4.0)
                    EggGutter.create corner startAt interval eggsCount
                )
        }

    let setEggGutter idx (eggGutter: EggGutter) (eggGuttersContainer: EggsContainer) =
        {
            Eggs = ArrayExt.insertAt idx eggGutter eggGuttersContainer.Eggs
        }

[<RequireQualifiedAccess;Struct>]
type BunnyStatus =
    | Active
    | Cooldown
    | Ready

type BunnyHandPos =
    | Top = 0
    | Bottom = 1
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module BunnyHandPos =
    let switch = function
        | BunnyHandPos.Top ->
            BunnyHandPos.Bottom
        | BunnyHandPos.Bottom ->
            BunnyHandPos.Top
        | x ->
            failwithf "%A not implemented yet!" x

type Bunny =
    {
        Status: BunnyStatus
        CooldownTime: float
        ActiveTime: float
        AutoActivateTime: float
        ChangesStatusDisabled: bool

        StatusChangeTimeLeft: float

        HandPos: BunnyHandPos option
        BellingRing: Timer option
        BellingRingInterval: float
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
            ChangesStatusDisabled = false

            StatusChangeTimeLeft = 0.0

            HandPos = None
            BellingRing = None
            BellingRingInterval = 100.0
        }

    let visible (bunny: Bunny) =
        { bunny with
            Status = BunnyStatus.Active
            StatusChangeTimeLeft = bunny.ActiveTime
        }

    let disableChangesStatus (bunny: Bunny) =
        { bunny with
            ChangesStatusDisabled = true
        }

    let startRingingBell (bunny: Bunny) =
        { bunny with
            BellingRing =
                Timer.create bunny.BellingRingInterval
                |> Some
        }

    let isRingingBell (bunny: Bunny) =
        Option.isSome bunny.BellingRing

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
