namespace NuPogodiElectronics
type BrokenEggPos =
    | Left = 0
    | Right = 1

[<Struct;RequireQualifiedAccess>]
type GameStatus =
    | HasNotStartedYet
    | Playing
    | GameOver

type State =
    {
        Wolf: Wolf
        EggsContainer: EggsContainer
        BrokenEgg: BrokenEggPos option
        HatchedChick: HatchedChick option
        CatchedEggsCount: int
        BrokenEggsCount: int
        Status: GameStatus
        Bunny: Bunny
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module State =
    let create () =
        {
            EggsContainer = EggsContainer.create ()
            Wolf = Wolf.create ()
            BrokenEgg = None
            HatchedChick = None
            CatchedEggsCount = 0
            BrokenEggsCount = 0
            Status = GameStatus.HasNotStartedYet
            Bunny = Bunny.create ()
        }

    let mapEggsContainer mapping (state: State) =
        { state with
            EggsContainer = mapping state.EggsContainer
        }

    let mapBunny mapping (state: State) =
        { state with
            Bunny = mapping state.Bunny
        }

    let mapHatchedChick mapping (state: State) =
        { state with
            HatchedChick = Option.map mapping state.HatchedChick
        }
