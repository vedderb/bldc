module Generated.StaticExample1Spec where
import qualified Data.Map as M
import SExpGen
import Test.Hspec as H

env :: Environment
env = M.fromList [ (Sym "apa", (ILit TInt28 5)),
                       (Sym "apa1", (ILit TInt28 (-10))),
                       (Sym "bepa", (ILit TUInt32 100)),
                       (Sym "cepa", (Symbol TSymbol (Sym "kurt-russel"))),
                       (Sym "depa", (FLit TFloat 3.14)),
                       (Sym "eepa", (FLit TDouble 6.28)),
                       (Sym "fepa", listToCons [i28 1, i32 2, u28 3, u32 4]),
                       (Sym "gepa", str "Hello World")
                     ]

spec :: H.Spec
spec = specify "StaticExample1Spec" $ (decEnv (encEnv env)) == env
