import Crucible
import SiftTests.Core
import SiftTests.Primitives
import SiftTests.Combinators
import SiftTests.Char
import SiftTests.Text

open Crucible

def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
