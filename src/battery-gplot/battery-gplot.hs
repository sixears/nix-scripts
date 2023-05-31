{-# OPTIONS_GHC -W -Wall -fhelpful-errors #-}

-- nix-env -i gnuplot
-- ./battery-gplot -o /tmp/out testdata/data0 -O > /tmp/gscript
-- ln -s /tmp/out data1
-- evince multiple_plots.eps


{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

{-# LANGUAGE PatternSynonyms     #-}

import Debug.Trace  ( traceShow )

import Prelude  ( (-), (/), div, floor, fromIntegral, log )
import MockIO ( DoMock )
import System.IO               ( IO, hIsEOF )
import MockIO.IOClass       ( HasIOClass, IOClass( IORead ) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode        ( (⊕) )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Options.Applicative       ( Parser )
import qualified  Data.Text.IO  as  TextIO

-- base --------------------------------

import Control.Monad   ( return, when )
import Data.Bool       ( Bool( False, True ) )
import Data.Either     ( Either( Left, Right ) )
import Data.Eq         ( Eq )
import Data.Function   ( ($) )
import Data.Maybe      ( Maybe( Just, Nothing ), fromMaybe )
import Data.List       ( reverse, zip, zip3 )
import Data.Ord        ( Ord, (>) )
import GHC.Num         ( (+) )
import GHC.Stack       ( HasCallStack )
import System.IO       ( Handle )
import Text.Read       ( readEither )
import Text.Show       ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode          ( (≡) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- fpath -------------------------------

import FPath.AbsDir           ( AbsDir, absdir )
import FPath.AppendableFPath  ( (⫻) )
import FPath.AbsFile          ( AbsFile, absfile )
import FPath.File             ( File( FileA ) )
import FPath.Parseable        ( readM )
import FPath.RelFile          ( relfile )

-- index -------------------------------

import Index  ( (!) )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational ) )

-- mockio ------------------------------

import MockIO      ( HasDoMock )
import MockIO.Log  ( MockIOClass, mkIOL )

-- mockio-plus -------------------------

import  MockIO.File  ( fileFoldLinesUTF8
                     , openFileReadUTF8, openFileWriteUTF8 )

-- monaderror-io -----------------------

import MonadError.IO        ( ioThrow )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO        ( MonadIO, liftIO )
import MonadIO.File   ( devnull, fileFoldLinesH )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Bool         ( 𝔹 )
import Data.MoreUnicode.Maybe        ( 𝕄 )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Text         ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- natural -----------------------------

import Natural  ( length )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, flag, help, long, metavar
                                    , option, short, value )

-- safe --------------------------------

import Safe  ( headMay, lastMay, tailSafe )

-- stdmain -----------------------------

import StdMain               ( LogTIO, stdMainSimple )
import StdMain.OptionsTools  ( fileToAbsNoOverwrite, fileToAbsUE )
import StdMain.UsageError    ( AsUsageError, UsageIOError )

-- strings -----------------------------

import Data.Strings ( strPadLeft )

-- text --------------------------------

import Data.Text     ( intercalate, pack, unlines, unpack, words )
import Data.Text.IO  ( hPutStrLn, putStrLn )

-- tfmt --------------------------------

import Text.Fmt   ( fmt )

--------------------------------------------------------------------------------

pattern 𝕱 ∷ 𝔹
pattern 𝕱 = False
pattern 𝕿 ∷ 𝔹
pattern 𝕿 = True
{-# COMPLETE 𝕿, 𝕱 #-}

pattern 𝕵 ∷ α → 𝕄 α
pattern 𝕵 a ← Just a
        where 𝕵 a = Just a
pattern 𝕹 ∷ 𝕄 α
pattern 𝕹 = Nothing
{-# COMPLETE 𝕵, 𝕹 #-}

gscript ∷ [(ℕ,ℕ)] → 𝕋
gscript segments =
  let
    ranges = [ [fmt|# range %d - %d|] start end | (start,end) ← segments ]
    -- the -1 is due to the use of 0-based numbering for the segments
    width = floor $ 1 + (log (fromIntegral $ length ranges - 1) / log 10)
    lpad = strPadLeft '0' width ∘ show
  in
    unlines $ ю
    [
      [ "# -- Battery Performance Gnuplot -----------------------------------------"
      , ""
      , "set terminal postscript eps enhanced color solid colortext 9;"
      , "set title 'battery performance';"
      , ""
      , "# X-Axis (Date/Time) -----------------------------------------------------"
      , ""
      , "# x-axis formatting --------------------"
      , ""
      , "set xlabel \"date-time\""
      , "set xdata time"
      , "set timefmt \"%s\""
      , "axis_gap = 25"
      , ""
      , "# x-axis ranges ------------------------"
      , ""
      ]
    , ranges
    , [ "" ]
    , [ [fmt|segment%s_start = %d; segment%s_end = %d; segment%s_length = %d|]
          (lpad i) st (lpad i) ed (lpad i) (ed-st)
        | (i,(st,ed)) ← zip [0..] segments ]
    , [ [fmt|segment_%s_%s_gap = %d|]
          (lpad $ i-1) (lpad i) (next-last)
        | (i,(_,last),(next,_)) ← zip3 [1..] segments (tailSafe segments) ]
    , [ "a0 = 1611933475; a0_ = 1611933549"
      , "a1 = 1611956790; a1_ = 1611957269"
      , "f(x) = (x <= a0_) ? x : (x < a1) ? NaN : (x - (a1-a0_) + axis_gap)"
      , "g(x) = (x <= a0_) ? x : (x < a0_+axis_gap) ? NaN : (x + (a1-a0_) - axis_gap)"
      , "# g(x) = (x <= segment0_end) ? x : (x < segment0_end+axis_gap) ? NaN : (x < (segment1_length+segment0_end) + axis_gap) ? (x + segment_0_1_gap - axis_gap) : (x < (segment1_length+segment0_end) + 2*axis_gap) ? NaN : (x + (segment_0_1_gap+segment_1_2_gap) - 2*axis_gap)"
      ]

    , [ "# x + " ⊕ intercalate "+" [ [fmt|segment_%s_%s_gap|] (lpad $ j-1) (lpad j) | j ← [1..i] ] ⊕ [fmt| - %d*axis_gap|] i | i ← [1..length segments-1] ]
    , [ "# g(x) = (x <= segment0_end) ? x : "
      , "#          (x < segment0_end+axis_gap) ? NaN : "
      , "#        (x < (segment1_length+segment0_end) + axis_gap) ? (x + segment_0_1_gap - axis_gap) : "
      , "#          (x < (segment1_length+segment0_end) + 2*axis_gap) ? NaN : (x + (segment_0_1_gap+segment_1_2_gap) - 2*axis_gap)"

      , "set xrange [1611933475:1611957269] noextend"
      , "set nonlinear x via f(x) inverse g(x)"
      , ""
      , "# Creation of the broken axis marks (this should be automated)"
      , "  set arrow 500 from 1611933549, graph 0 to 1611956790, graph 0 nohead lt 1611956790 lw 4 lc bgnd front"
      , "  set arrow 501 from 1611933549, graph 0 length graph  .01 angle 75 nohead lw 2 front"
      , "  set arrow 502 from 1611933549, graph 0 length graph -.01 angle 75 nohead lw 2 front"
      , "  set arrow 503 from 1611956790, graph 0 length graph  .01 angle 75 nohead lw 2 front"
      , "  set arrow 504 from 1611956790, graph 0 length graph -.01 angle 75 nohead lw 2 front"
      , ""
      , "# Y-Axes -----------------------------------------------------------------"
      , ""
      , "set ylabel \"potential difference (V)\"; set ytics nomirror;"
      , "set y2label \"energy (Wh) / power (W)\"; set y2tics nomirror;"
      , ""
      , "# Output -----------------------------------------------------------------"
      , ""
      , "set output 'multiple_plots.eps';"
      , ""
      , "plot 'data1' using 1:3 w l  title 'energy (Wh)'  axis x1y2,\\"
      , "     ''           using 1:4 w l title 'p.d. (V)'     axis x1y1,\\"
      , "     ''           using 1:6 w l title 'min p.d. (V)' axis x1y1,\\"
      , "     ''           using 1:5 w l title 'power(W)' axis x1y2"
      , ""
      , "# -- that's all, folks! --------------------------------------------------"
      ]
    ]
------------------------------------------------------------

stdoutA ∷ AbsFile
stdoutA = [absfile|/dev/stdout|]
stdoutF ∷ File
stdoutF = FileA stdoutA

------------------------------------------------------------

data OptOverwrite = NoOptOverwrite | DoOptOverwrite
  deriving (Eq,Show)

------------------------------------------------------------

data Options = Options { input_          ∷ File
                       , data_output_    ∷ File
                       , gscript_output_ ∷ File
                       , overwrite_      ∷ OptOverwrite
                       }

bat_dir ∷ AbsDir
bat_dir = [absdir|/sys/class/power_supply/BAT0/|]

bat_uevent ∷ AbsFile
bat_uevent = bat_dir ⫻ [relfile|uevent|]

{- | Parse option settings from the user-supplied cmdline. -}
parseOpts ∷ Parser Options
parseOpts =
  let
    input_help  = help $ ю [ "Read input from this file.  File should be"
                           , "tab-delimited columns, being [ timestamp (seconds"
                           , "since the epoch); ignored (typically a"
                           , "human-readable timestamp), energy (Wh), P.D. (V),"
                           , "power (W), min P.D. (V) (should be a constant;"
                           , "the minimum p.d. rating for this battery) ].  "
                           , "These values can be read from "
                           , toString bat_uevent
                           ]
    output_help = help $ ю [ "Data output is written here." ]
  in
    Options ⊳ argument readM (metavar "DATA-FILE" ⊕ input_help)
            ⊵ option readM (short 'o' ⊕ long "output" ⊕ long "data-output"
                                      ⊕ value stdoutF ⊕ output_help)
            ⊵ option readM (short 'g' ⊕ long "gscript-output" ⊕ value stdoutF)
            ⊵ flag NoOptOverwrite DoOptOverwrite (short 'O' ⊕ long "overwrite")

{- | Like `Options`, but the values have been checked for sanity (which
     may involve IO, and so cannot be done as part of options parsing). -}
data CheckOpts = CheckOpts { _input          ∷ AbsFile
                           , _data_output    ∷ AbsFile
                           , _gscript_output ∷ AbsFile
                           }

input ∷ Lens' CheckOpts AbsFile
input = lens _input (\ o f → o { _input = f })

data_output ∷ Lens' CheckOpts AbsFile
data_output = lens _data_output (\ o f → o { _data_output = f })

gscript_output ∷ Lens' CheckOpts AbsFile
gscript_output = lens _gscript_output (\ o f → o { _gscript_output = f })

{- | Check the user options (e.g., output files are writable but not overwriting
     extant files ('cept character special files, named pipes, sockets), return
     a sanitized set of options.
 -}

checkOpts ∷ (MonadIO μ,
             AsIOError ε, AsUsageError ε, Printable ε,
             MonadError ε μ, HasCallStack,
             Default ω, HasIOClass ω, HasDoMock ω,
             MonadLog (Log ω) μ) ⇒
            Options → μ CheckOpts

checkOpts opts = do
  let check_output = if DoOptOverwrite ≡ overwrite_ opts
                     then fileToAbsUE
                     else fileToAbsNoOverwrite
  data_output_abs    ← check_output (data_output_ opts)
  input_abs          ← fileToAbsUE (input_ opts)
  gscript_output_abs ← check_output (gscript_output_ opts)
  return $ CheckOpts input_abs data_output_abs gscript_output_abs

------------------------------------------------------------

{- | Parse a line of data.  The line is expected to lead with a ℕ, being a
     timestamp in seconds since the epoch.
     Blank lines are ignored.
     In case of a parse failure, an error is thrown into IO.
     All other lines are copied to the output FH.
     Lines are grouped into segments; any gap of more than `segment_size`
     seconds causes the start of a new segment.
-}

segment_size ∷ ℕ
segment_size = 300 -- length of a segment, in seconds.

{- | Read a single line of data; which is expected to begin with a timestamp
     (seconds since the epoch).  Return the timestamp, and all the words of the
     line (including the timestamp).

     A blank line gives a `Nothing` return.
     A line that fails to parse will cause an `IOException` thrown into `IO`.
-}
__readDataLine__ ∷ MonadIO μ ⇒ 𝕋 → μ (𝕄 (ℕ, [𝕋]))
__readDataLine__ t = do
  let ws = words t
  case readEither @ℕ ∘ unpack ⊳ 0 ! ws of
    -- blank line
    𝕹 → return 𝕹
    -- failed to parse leading time
    𝕵 (Left e)  → ioThrow e
    -- successfully parsed a ℕ from the first word
    𝕵 (Right w) → return $ 𝕵 (w,ws)
      -- place a blank line for any gap > 5mins, to cause a gap in the x-axis
      -- (this is used to suggest a lid-closing event)


data Accumulator = Accumulator ℕ       -- ^ latest timestamp seen
                               ℕ       -- ^ beginning of the current segment
                               [(ℕ,ℕ)] -- ^ list of prior segments
  deriving Show

{- | All segments recorded in the accumulator, including the current one;
     in ascending timestamp order. -}
segments ∷ Accumulator → [(ℕ,ℕ)]
segments (Accumulator latest segstart segments) =
  reverse $ (segstart,latest) : segments

{- | Handler for each data line.  Reads the line, and writes it to `h`.
     Prepends with a blank line if the timestamp gap is greater than
     `segment_size`.  Ignores blank lines.
     Accumulator is updated with a list of contiguous segments (where a segment
     is a list of points, each of which is fewer than `segment_size` seconds
     later than the prior point in that segment.
 -}

__handleDataLine__ ∷ Handle         -- ^ file handle to write to
                   → Accumulator    -- ^ latest timestamp, read from beginning of
                                    --   prior line
                   → 𝕋              -- ^ line of text to parse
                   → IO Accumulator -- ^ timestamp found
__handleDataLine__ h acc@(Accumulator latest start priors) t = do
  ln ← __readDataLine__ t
  case ln of
    𝕹        → return acc
    𝕵 (ts,_) → do
      let gap = ts - latest
      (s, ps) ← case (gap > segment_size, latest, start) of
                  -- contiguous segment, no update to segment start/priors
                  (𝕱, _, _) → return (start,priors)
                  -- discontiguous segment, output a blank line, reset start
                  -- and add the last segment to priors
                  (𝕿, l, s) → do
                    hPutStrLn h ""
                    return (ts, (s,l) : priors)

      -- just copy the input to the output, but return the timestamp for
      -- the next iteration.
      hPutStrLn h t
      return $ Accumulator ts s ps -- beginning priors

readFirstLine ∷ MonadIO μ ⇒ Handle → μ (𝕄 (ℕ, [𝕋], 𝕋))
readFirstLine h = do
  eof ← liftIO $ hIsEOF h
  case eof of
    𝕿 → return 𝕹
    𝕱 → liftIO $ do
      t ← TextIO.hGetLine h
      __readDataLine__ t ≫ \ case
          𝕵 (w,ws) → return (𝕵 (w,ws,t))
          𝕹   → readFirstLine h

--------------------------------------------------------------------------------

main ∷ MonadIO μ ⇒ μ ()
main = liftIO $
  stdMainSimple "generate battery gnuplot" parseOpts main'

main' ∷ HasCallStack ⇒ DoMock → Options → (LogTIO MockIOClass UsageIOError) ()
main' mck opts = do
  copts ← checkOpts opts

  d_fh ← openFileWriteUTF8 𝕹 (𝕵 0o644) devnull (copts ⊣ data_output) mck
  i_fh ← openFileReadUTF8 devnull (copts ⊣ input) mck
  -- mock/log this
  fl ← readFirstLine i_fh
--  acc  ← fileFoldLinesUTF8 def (__handleDataLine__ d_fh) (return def) (copts ⊣ input) mck
  -- and mock/log this, too
  mkIOL Informational IORead ("hello" ∷ 𝕋) () (return ()) mck
  -- check firstline is not nothing
--  acc  ← liftIO $ fileFoldLinesH def (__handleDataLine__ d_fh) i_fh
  case fl of
    𝕵 (st,_,t) → do
      liftIO $ hPutStrLn d_fh t
      acc ← liftIO $ fileFoldLinesH (Accumulator st st []) (__handleDataLine__ d_fh) i_fh
      let acc' = segments acc
      liftIO $ putStrLn $ gscript acc'

-- that's all, folks! ----------------------------------------------------------
