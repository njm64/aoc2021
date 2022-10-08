#!/bin/sh
ghc -prof -fprof-auto *.hs
./main +RTS -p
