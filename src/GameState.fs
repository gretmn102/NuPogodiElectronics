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
        Eggs: Map<EggId, Egg>
        Wolf: Wolf
        Cooldown: float
        TimeAcc: float
        BrokenEggPos: BrokenEggPos option
        CatchedEggsCount: int
        BrokenEggsCount: int
        Status: GameStatus
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
            BrokenEggPos = None
            CatchedEggsCount = 0
            BrokenEggsCount = 0
            Status = GameStatus.HasNotStartedYet
        }
