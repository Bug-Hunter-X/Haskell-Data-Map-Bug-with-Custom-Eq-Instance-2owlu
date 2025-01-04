# Haskell Data.Map Bug with Custom Eq Instance

This repository demonstrates a subtle bug in Haskell code involving the use of `Data.Map` with a custom `Eq` instance that doesn't fully align with the intended key semantics.  Lazy evaluation plays a significant role in manifesting this error.

## The Problem
The bug occurs when a custom equality (`Eq`) instance for the keys of a `Data.Map` is defined in a way that doesn't maintain strict uniqueness under all circumstances.  This may lead to unexpected overwriting of entries or incorrect lookups.

## How to Reproduce
1. Clone this repository.
2. Compile and run `bug.hs` using a Haskell compiler (like GHC).
3. Observe the unexpected output.

## Solution
The solution is to carefully define the `Eq` instance to ensure that it perfectly reflects the intended semantic equality for the keys of the map.  Often this means making it stricter or handling edge cases more robustly.   Examine `bugSolution.hs` to see the recommended fix.