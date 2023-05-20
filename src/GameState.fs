namespace NuPogodiElectronics
type BrokenEggPos =
    | Left = 0
    | Right = 1

type State =
    {
        Eggs: Map<EggId, Egg>
        Wolf: Wolf
        Cooldown: float
        TimeAcc: float
        BrokenEggPos: BrokenEggPos option
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
        }
