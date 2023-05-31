{-# OPTIONS_GHC -Wall #-}

-- https://stackoverflow.com/questions/58777439/haskell-data-yaml-utf-8-decoding

{- Alternately, try this:

λ> :m + Data.ByteString.UTF8 
Prelude Data.ByteString.UTF8
λ> t2  = Data.ByteString.UTF8.fromString "α"
Prelude Data.ByteString.UTF8
λ> t2
"\206\177"
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- read a file, produce a haskell representation as ByteString by concatenation
-- of many lines

-- base --------------------------------

import Control.Monad       ( forM_, return )
import Data.Function       ( ($) )
import Data.String         ( String )
import System.Environment  ( getArgs )
import System.IO           ( FilePath, IO, putStrLn )
import Text.Show           ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import Data.ByteString  ( ByteString, getContents, readFile, split )

-- lens --------------------------------

import System.FilePath.Lens  ( basename )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Monoid   ( ю )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

bs2hs ∷ String → ByteString → [String]
bs2hs name bs =   [ [fmt|%s ∷ ByteString|] name
                  ]
                ⊕ case split 10 bs of
                    []      → [ [fmt|%s = empty|] name ]
                    (b:bss) → ю [ [ [fmt|%s = intercalate "\\n"|] name
                                  , "  [ " ⊕ show b ]
                                , (("  , " ⊕) ∘ show) ⊳ bss
                                , [ "  ]" ]
                                ]

bs2hsFile ∷ FilePath → IO [String]
bs2hsFile fn = do bs ← readFile fn
                  return $ bs2hs (fn ⊣ basename) bs

putStrLns ∷ [String] → IO ()
putStrLns ss = forM_ ss putStrLn

main ∷ IO ()
main = do
  getArgs ≫ \x → case x of
                   []  → do bs ← getContents
                            forM_ (bs2hs "stdin" bs) putStrLn
                   fns → forM_ fns (\ fn → bs2hsFile fn ≫ putStrLns)
