module Tests.AllTests where

import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)

import Tests.CellTest
import Tests.TapeTest

all = suite "All Tests"
  [ Tests.CellTest.tests
  , Tests.PuzzleTest.tests
  , Tests.TapeTest.tests
  ]

main =
  runDisplay all
