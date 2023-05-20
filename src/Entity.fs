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
        System.Enum.GetValues(typeof<EggGutter>) :?> EggGutter array // System.Collections.Generic.

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
