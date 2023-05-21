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
