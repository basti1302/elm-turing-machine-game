module Tests.AllTests where

import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)

import Tests.CellTest as CellTest
import Tests.PuzzleTest as PuzzleTest
import Tests.TapeTest as TapeTest

all = suite "All Tests"
  [ CellTest.tests
  , PuzzleTest.tests
  , TapeTest.tests
  ]

main =
  runDisplay all
