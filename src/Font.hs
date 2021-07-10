module Font
  ( font
  ) where

import qualified Data.Map as Map

fontList =
  [ ( ' '
    , "\
        \    ....\n\
        \    ....\n\
        \    ....\n\
        \    ....\n\
        \    ....\n\
        \    ....\n\
        \    ....\n\
        \    ....\n\
        \")
  , ( ','
    , "\
         \   ..\n\
         \   ..\n\
         \   ..\n\
         \   ..\n\
         \   ..\n\
         \   .O\n\
         \   .O\n\
         \   O.\n\
        \")
  , ( '.'
    , "\
         \   ..\n\
         \   ..\n\
         \   ..\n\
         \   ..\n\
         \   ..\n\
         \   ..\n\
         \   OO\n\
         \   OO\n\
        \")
  , ( '0'
    , "\
         \   .OOO.\n\
         \   O...O\n\
         \   O...O\n\
         \   O.O.O\n\
         \   O...O\n\
         \   O...O\n\
         \   .OOO.\n\
        \")
  , ( '1'
    , "\
         \   OO\n\
         \   .O\n\
         \   .O\n\
         \   .O\n\
         \   .O\n\
         \   .O\n\
         \   .O\n\
        \")
  , ( '2'
    , "\
         \   .OO.\n\
         \   O..O\n\
         \   ...O\n\
         \   ..O.\n\
         \   .O..\n\
         \   O...\n\
         \   OOOO\n\
        \")
  , ( '3'
    , "\
         \   .OO.\n\
         \   O..O\n\
         \   ...O\n\
         \   ..O.\n\
         \   ...O\n\
         \   O..O\n\
         \   .OO.\n\
        \")
  , ( '4'
    , "\
         \   .O.O\n\
         \   .O.O\n\
         \   O..O\n\
         \   O..O\n\
         \   OOOO\n\
         \   ...O\n\
         \   ...O\n\
        \")
  , ( '5'
    , "\
         \   OOOO\n\
         \   O...\n\
         \   O...\n\
         \   OOO.\n\
         \   ...O\n\
         \   O..O\n\
         \   .OO.\n\
        \")
  , ( '6'
    , "\
         \   .OO.\n\
         \   O..O\n\
         \   O...\n\
         \   OOO.\n\
         \   O..O\n\
         \   O..O\n\
         \   .OO.\n\
        \")
  , ( '7'
    , "\
         \   OOOO\n\
         \   ...O\n\
         \   ...O\n\
         \   ..O.\n\
         \   .O..\n\
         \   .O..\n\
         \   O...\n\
        \")
  , ( '8'
    , "\
         \   .OO.\n\
         \   O..O\n\
         \   O..O\n\
         \   .OO.\n\
         \   O..O\n\
         \   O..O\n\
         \   .OO.\n\
        \")
  , ( '9'
    , "\
         \   .OO.\n\
         \   O..O\n\
         \   O..O\n\
         \   .OOO\n\
         \   ...O\n\
         \   O..O\n\
         \   .OO.\n\
        \")
  , ( ','
    , "\
         \   ..\n\
         \   ..\n\
         \   ..\n\
         \   OO\n\
         \   OO\n\
         \   ..\n\
         \   OO\n\
         \   OO\n\
        \")
  , ( '?'
    , "\
         \   .OOO.\n\
         \   O...O\n\
         \   ....O\n\
         \   ...O.\n\
         \   ..O..\n\
         \   ..O..\n\
         \   .....\n\
         \   ..O..\n\
        \")
  , ( 'A'
    , "\
         \   .OOOO.\n\
         \   O....O\n\
         \   O....O\n\
         \   OOOOOO\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
        \")
  , ( 'B'
    , "\
         \   OOOOO.\n\
         \   O....O\n\
         \   O....O\n\
         \   OOOOO.\n\
         \   O....O\n\
         \   O....O\n\
         \   OOOOO.\n\
        \")
  , ( 'C'
    , "\
         \   .OOOO.\n\
         \   O....O\n\
         \   O.....\n\
         \   O.....\n\
         \   O.....\n\
         \   O....O\n\
         \   .OOOO.\n\
        \")
  , ( 'D'
    , "\
         \   OOOOO.\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   OOOOO.\n\
        \")
  , ( 'E'
    , "\
         \   OOOOOO\n\
         \   O.....\n\
         \   O.....\n\
         \   OOOOO.\n\
         \   O.....\n\
         \   O.....\n\
         \   OOOOOO\n\
        \")
  , ( 'F'
    , "\
         \   OOOOOO\n\
         \   O.....\n\
         \   O.....\n\
         \   OOOOO.\n\
         \   O.....\n\
         \   O.....\n\
         \   O.....\n\
        \")
  , ( 'G'
    , "\
         \   .OOOO.\n\
         \   O....O\n\
         \   O.....\n\
         \   O..OOO\n\
         \   O....O\n\
         \   O....O\n\
         \   .OOOO.\n\
        \")
  , ( 'H'
    , "\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   OOOOOO\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
        \")
  , ( 'I'
    , "\
         \   OOO\n\
         \   .O.\n\
         \   .O.\n\
         \   .O.\n\
         \   .O.\n\
         \   .O.\n\
         \   OOO\n\
        \")
  , ( 'J'
    , "\
         \   ....O\n\
         \   ....O\n\
         \   ....O\n\
         \   ....O\n\
         \   ....O\n\
         \   O...O\n\
         \   .OOO.\n\
        \")
  , ( 'K'
    , "\
         \   O...O\n\
         \   O...O\n\
         \   O..O.\n\
         \   OOO..\n\
         \   O..O.\n\
         \   O...O\n\
         \   O...O\n\
        \")
  , ( 'L'
    , "\
         \   O....\n\
         \   O....\n\
         \   O....\n\
         \   O....\n\
         \   O....\n\
         \   O....\n\
         \   OOOOO\n\
        \")
  , ( 'M'
    , "\
         \   O.....O\n\
         \   O.....O\n\
         \   OO...OO\n\
         \   OO...OO\n\
         \   O.O.O.O\n\
         \   O..O..O\n\
         \   O..O..O\n\
        \")
  , ( 'N'
    , "\
         \   O....O\n\
         \   OO...O\n\
         \   OO...O\n\
         \   O.OO.O\n\
         \   O...OO\n\
         \   O...OO\n\
         \   O....O\n\
        \")
  , ( 'O'
    , "\
         \   .OOOO.\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   .OOOO.\n\
        \")
  , ( 'P'
    , "\
         \   OOOOO.\n\
         \   O....O\n\
         \   O....O\n\
         \   OOOOO.\n\
         \   O.....\n\
         \   O.....\n\
         \   O.....\n\
        \")
  , ( 'Q'
    , "\
         \   .OOOO..\n\
         \   O....O.\n\
         \   O....O.\n\
         \   O....O.\n\
         \   O..O.O.\n\
         \   O. .OO.\n\
         \   .OOOO.O\n\
        \")
  , ( 'R'
    , "\
         \   OOOO.\n\
         \   O...O\n\
         \   O...O\n\
         \   OOOO.\n\
         \   O..O.\n\
         \   O...O\n\
         \   O...O\n\
        \")
  , ( 'S'
    , "\
         \   .OOO.\n\
         \   O...O\n\
         \   O....\n\
         \   .OOO.\n\
         \   ....O\n\
         \   O...O\n\
         \   .OOO.\n\
        \")
  , ( 'T'
    , "\
         \   OOOOO\n\
         \   ..O..\n\
         \   ..O..\n\
         \   ..O..\n\
         \   ..O..\n\
         \   ..O..\n\
         \   ..O..\n\
        \")
  , ( 'U'
    , "\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   O....O\n\
         \   .OOOO.\n\
        \")
  , ( 'V'
    , "\
         \   O...O\n\
         \   O...O\n\
         \   O...O\n\
         \   .O.O.\n\
         \   .O.O.\n\
         \   ..O..\n\
         \   ..O..\n\
        \")
  , ( 'W'
    , "\
         \   O.....O\n\
         \   O.....O\n\
         \   .O...O.\n\
         \   .O.O.O.\n\
         \   .O.O.O.\n\
         \   ..O.O..\n\
         \   ..O.O..\n\
        \")
  , ( 'X'
    , "\
         \   O...O\n\
         \   O...O\n\
         \   .O.O.\n\
         \   ..O..\n\
         \   .O.O.\n\
         \   O...O\n\
         \   O...O\n\
        \")
  , ( 'Y'
    , "\
         \   O...O\n\
         \   O...O\n\
         \   .O.O.\n\
         \   .O.O.\n\
         \   ..O..\n\
         \   ..O..\n\
         \   ..O..\n\
        \")
  , ( 'Z'
    , "\
         \   OOOOO\n\
         \   ....O\n\
         \   ...O.\n\
         \   ..O..\n\
         \   .O...\n\
         \   O....\n\
         \   OOOOO\n\
        \")
  ]

font = Map.fromList fontList
