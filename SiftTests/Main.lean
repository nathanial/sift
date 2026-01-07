import Crucible
import SiftTests.Core
import SiftTests.Primitives
import SiftTests.Combinators
import SiftTests.Char
import SiftTests.Text
import SiftTests.Utf8

open Crucible

def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
