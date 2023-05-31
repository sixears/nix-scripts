{-# OPTIONS_GHC -fprint-potential-instances -Wall #-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

import Prelude  ( Int, error )

-- base --------------------------------

import Control.Concurrent      ( threadDelay )
import Control.Monad           ( (>>=), (>>), forM_, return, when )
import Control.Monad.IO.Class  ( liftIO )
import Data.Bool               ( Bool( False ), not )
import Data.Char               ( isAlphaNum, isDigit, isSpace )
import Data.Function           ( ($), const )
import Data.Functor            ( (<$>) )
import Data.List               ( and, drop, dropWhile, dropWhileEnd, filter
                               , length, take, takeWhile )
import Data.Maybe              ( isJust )
import Data.Ord                ( (>) )
import Data.String             ( String, lines )
import Numeric.Natural         ( Natural )
import System.IO               ( FilePath, IO, putStrLn )
import Text.Read               ( readMaybe )
import Text.Show               ( show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡), (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- directory ---------------------------

import System.Directory  ( getCurrentDirectory, makeAbsolute
                         , withCurrentDirectory )

-- process -----------------------------

import System.Process  ( callProcess )

-- shake -------------------------------

import Development.Shake      ( Action, CmdOption( Cwd )
                              , ShakeOptions( shakeFiles, shakeProgress
                                            , shakeThreads )
                              , Stderr( Stderr ), Stdout( Stdout )
                              , (%>)
                              , cmd, cmd_, getDirectoryFiles, need, newResource
                              , phony, progressDisplay, progressProgram
                              , progressTitlebar, putQuiet, removeFiles
                              , removeFilesAfter, shakeArgs, shakeOptions
                              , withResource, withTempDirWithin
                              , writeFileChanged
                              )
import Development.Shake.FilePath
                              ( (</>), (-<.>)
                              , replaceDirectory, takeDirectory, takeFileName )

-- unix --------------------------------

import System.Posix.Files  ( createLink, createSymbolicLink, fileExist )

--------------------------------------------------------------------------------

----------------------------------------
--              commands              --
----------------------------------------

cdparanoia ∷ FilePath
cdparanoia = "__cdparanoia__/bin/cdparanoia"

flac ∷ FilePath
flac = "__flac__/bin/flac"

minfo ∷ FilePath
minfo = "__minfo__/bin/minfo"

lsCmd ∷ FilePath
lsCmd = "__coreutils__/bin/ls"

musictag ∷ FilePath
musictag = "__music-tag__/bin/music-tag"

----------------------------------------

inTempDir ∷ FilePath → [FilePath] → [FilePath] → (FilePath → Action α) → Action α
inTempDir d ls os f =
  let llink t l = do absL ← liftIO $ makeAbsolute l
                     putQuiet $ "LINK: " ⊕ absL ⊕ " -> " ⊕ (replaceDirectory l t)
                     liftIO $ createSymbolicLink absL (replaceDirectory l t)
   in do need ls
         cwd ← liftIO $ getCurrentDirectory
         withTempDirWithin d $ \ t → do
           forM_ ls $ llink t
           liftIO $ callProcess lsCmd ["-lL", t ]
           a ← f t
           forM_ os $ \o → do liftIO $ createLink (replaceDirectory o t) (cwd </> o)
           return a

inLocalTempDir ∷ [FilePath] → [FilePath] → (FilePath → Action α) → Action α
inLocalTempDir = inTempDir "."

inLocalTempDir_ ∷ [FilePath] → [FilePath] → (FilePath → Action ()) → Action ()
inLocalTempDir_ = inLocalTempDir

inTargetTempDir ∷ [FilePath] → FilePath → (FilePath → Action α) → Action α
inTargetTempDir ls o = inTempDir (takeDirectory o) ls [o]

inTargetTempDir_ ∷ [FilePath] → FilePath → (FilePath → Action ()) → Action ()
inTargetTempDir_ = inTargetTempDir

{-
wavTrim ∷ Double → FilePath → FilePath → FilePath → Action ()
wavTrim threshold dir infn outfn =
  let args = [ "silence", "1", "0.1", show threshold ⊕ "%"
             , "reverse", "silence", "1", "0.1", show threshold ⊕ "%"
             , "reverse"
             ]
   in cmd_ (Cwd dir) "${pkgs.sox}/bin/sox" ([ infn, outfn ] ⊕ args)
-}

strip ∷ String → String
strip = dropWhileEnd isSpace ∘ dropWhile isSpace

disc_dir ∷ FilePath → FilePath
disc_dir d = let b = takeFileName d
              in if and [ length b > 5
                        , take 5 b ≡ "Disc "
                        , isJust (readMaybe @Int $ takeWhile (≢ ' ') $ drop 5 b)
                        ]
                 then takeDirectory d
                 else d

info_yaml ∷ FilePath → FilePath
info_yaml f = disc_dir (takeDirectory f) </> "info.yaml"

artwork_jpg ∷ FilePath → FilePath
artwork_jpg f = disc_dir (takeDirectory f) </> "artwork.jpg"

main ∷ IO ()
main = do
  pProgram <- progressProgram

  let pDisplay = progressDisplay 1 (\ s → progressTitlebar s >> pProgram s)
      opts = shakeOptions { shakeFiles = "/tmp/martyn/_shake"
                          , shakeThreads = 0
                          , shakeProgress = pDisplay
                          }
  shakeArgs opts $ do
--    want ["mp3s"]
    cdrom ← newResource "cdrom" 1

    "info.y" %> \ out → do
      (Stderr err) ← cmd cdparanoia "-Q"
      let d1 ""    = False
          d1 (c:_) = isDigit c
          trackInfo = filter d1 $ strip <$> lines err
      (Stdout yaml) ← cmd minfo "write" [ show $ length trackInfo ]
      writeFileChanged out yaml

    "//*.mp3" %> \ out → do
      let flc  = out -<.> "flac"
          info = info_yaml   out
          art  = artwork_jpg out
--      inLocalTempDir_ [art,info,flc] [out] $ \ t →
--        cmd_ (Cwd t) ("${mp3mk}/bin/mp3mk" ∷ String) [takeFileName flc]
      cmd_ ("__mp3mk__/bin/mp3mk" ∷ String) [flc]

    "*.flac" %> \ out → do
      fex ← liftIO $ fileExist out
      when (not fex) $ do
        let (t1 : t2 : t3 : _) = out
        if isDigit t1 ∧ isDigit t2 ∧ not (isAlphaNum t3) then
          do let wav  = "track" ⊕ [t1,t2] ⊕ ".cdda.wav"
                 info = info_yaml   out
                 art  = artwork_jpg out
             need [wav,info,art]
             inTargetTempDir_ [wav,info,art] out $ \ t → do
               cmd_ (Cwd t) flac "--best" [wav] "-o" [out]
               cmd_ (Cwd t) musictag "-v" [out]
             liftIO $ removeFiles "." [wav]
        else
          error $ "filename not parsed: '" ⊕ out ⊕ "'"

    -- can't get this to work reliably, it keeps re-ripping, and then failing
    -- (to make links in inLocalTempDir_, even though the files blatantly exist)
    "XXXtrack*.cdda.wav" %> \ out → do
      inLocalTempDir_ [] [out] $ \ t → do
        let track   = take 7 out
            trackid = drop 5 track
        withResource cdrom 1 $ liftIO $ withCurrentDirectory t $ do
          callProcess cdparanoia ["--batch", trackid]
          -- wait for up to a second for the file to appear
          forM_ [1∷Natural ..10] $ const $ fileExist out >>= \ fex → when (not fex) (putStrLn ("waiting for '" ⊕ out ⊕ "'") >> threadDelay 100)
          liftIO $ fileExist out >>= \ fex → when (not fex) (error $ "not found: '" ⊕ out ⊕ "'")

    -- Even at 0.01%, it still feels a little aggressive; e.g., try the fade out
    -- at the end of The Pineapple Thief's "In Exile" (2016 Your Wilderness, #1).

    -- XXX try shntool trim/split
    {-
    "track*.trim.wav" %> \ out → do
      -- https://thejoe.it/en/2017/11/03/eliminare-il-silenzio-allinizio-e-alla-fine-di-un-file-audio-con-sox/
      -- https://digitalcardboard.com/blog/2009/08/25/the-sox-of-silence/comment-page-2/
      let cdda = take 7 out ⊕ ".cdda.wav"
      need [cdda]
      inLocalTempDir_ [cdda] [out] $ \ t → wavTrim 0.01 t cdda out
    -}

    phony "mp3s" $ do
      ss ← getDirectoryFiles "." ["*.flac"]
      cwd ← liftIO $ getCurrentDirectory
      need [ cwd </> t | s ← ss, let t = s -<.> "mp3"  ]

    phony "rip" $ do
      cwd ← liftIO $ getCurrentDirectory
      need [ disc_dir cwd </> "info.yaml" ]
--      (Stdout tcount_) ← cmd minfo "track-count"
--      let tcount = read tcount_
--          wavs = (printf "track%02d.cdda.wav") <$> [1∷Natural .. tcount]
--      inLocalTempDir [] wavs $ \ t →
--        liftIO $ withCurrentDirectory t (callProcess cdparanoia ["--batch"])
      (Stdout flacs) ← cmd minfo "flac-list"
      (Stdout mp3s)  ← cmd minfo "mp3-list"
      need (lines flacs ⊕ lines mp3s)

    phony "clean" $ do
      removeFilesAfter "." ["*.wav","extra-dir-*","*~"]
