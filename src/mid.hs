{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

import Base1

-- base --------------------------------

import Data.Char      ( isDigit )
import Data.Function  ( flip )
import Data.List      ( concatMap, sortOn )
import Data.Maybe     ( catMaybes )
import Text.Read      ( Read, readEither )

-- containers --------------------------

import qualified Data.Map.Strict  as  Map

-- data-textual ------------------------

import qualified Data.Textual

-- duration ----------------------------

import Duration  ( Duration )

-- env-plus ----------------------------

import Env.Types  ( ә, ӭ )

-- fpath -------------------------------

import FPath.AbsFile           ( AbsFile, absfile )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( File )
import FPath.Parseable         ( readM )

-- fstat -------------------------------

import FStat  ( FStat, size )

-- lens --------------------------------

import Control.Lens.At  ( At, Index, IxValue, at )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, Severity( Informational ) )

-- log-plus ----------------------------

import Log  ( Log )

-- mockio-log --------------------------

import MockIO.MockIOClass  ( MockIOClass )
import MockIO.DoMock  ( DoMock( NoMock ) )

-- mockio-plus -------------------------

import MockIO.FStat    ( stat )
import MockIO.Process  ( ꙩ )

-- mtl ---------------------------------

import Control.Monad.Reader  ( runReaderT )
import Control.Monad.Trans   ( lift )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( throwUserError )

-- monadio-plus ------------------------

import MonadIO                        ( say, warn )
import MonadIO.Base                   ( getArgs )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.FPath                  ( pResolve )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, flag, help, long, metavar, option
                                    , short, switch, strArgument, value )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReader, parseNE, textualOption )

-- stdmain -----------------------------

import StdMain  ( stdMainNoDR )
import StdMain.UsageError  ( UsageFPProcIOError )

-- text --------------------------------

import Data.Text  ( breakOn, drop, isPrefixOf, pack, takeWhile, unpack )

-- textual-plus ------------------------

import TextualPlus  ( TextualPlus ( textual' ), parseTextual )

--------------------------------------------------------------------------------

data Mode = ModeRaw | ModeParsed

data Options = Options { _mode   ∷ Mode
                       , _inputs ∷ NonEmpty File
                       }

----------------------------------------

inputs ∷ Lens' Options (NonEmpty File)
inputs = lens _inputs (\ o is → o { _inputs = is })

----------------------------------------

mode ∷ Lens' Options Mode
mode = lens _mode (\ o is → o { _mode = is })

----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ flag ModeParsed ModeRaw (long "raw" ⊕ help "output all ID_ tags")
          ⊵ parseNE (argument readM (metavar "FILENAME"))

------------------------------------------------------------

-- FIXME

mplayer = [absfile|/nix/store/y2j061fqy2zvn3na1rvv5vy31a7v1z27-mplayer-unstable-2022-02-03/bin/mplayer|]

instance TextualPlus Duration where
  textual' = Data.Textual.textual

(⫤) ∷ At δ ⇒ δ → Index δ → 𝕄 (IxValue δ)
x ⫤ y = x ⊣ at y

myMain ∷ ∀ ε .
         (HasCallStack, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
          AsProcExitError ε, Printable ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain opts = flip runReaderT NoMock $ do
  ins ∷ NonEmpty AbsFile ← sequence $ pResolve ⊳ opts ⊣ inputs
  xs ∷ NonEmpty (𝕄 𝕋) ← forM ins $ \ input → do
    let args = [ "-vo", "null", "-ao", "null", "-frames", "0", "-identify"
               , toText input ]
        parseLine l = if "ID_" `isPrefixOf` l
                      then case breakOn "=" l of
                             (_,"") → []
                             (k,v)  → [(k,drop 1 v)]
                      else []
        printRaw (k,v) = say  $ [fmtT|%t\t%t|] k v

    -- this will error out in case of non-zero exit, so no need to
    -- bind the exit value to a var
    (_,stdout∷[𝕋]) ← ꙩ (mplayer,args, [ӭ $ ә"HOME"])

    let m_identifiers = Map.fromList $ concatMap parseLine stdout

    let printParsed ∷ 𝕄 FStat → Map.Map 𝕋 𝕋 → 𝔼 𝕋 (Duration,ℕ,ℕ,Word64)
        printParsed st identifiers = do
          let readEitherT ∷ Read α ⇒ 𝕊 → 𝕋 → 𝔼 𝕋 α
              readEitherT typ s =
                case readEither (unpack s) of
                  𝕽 x → 𝕽 $ x
                  𝕷 e → 𝕷 $ ([fmt|failed to parse %t as %s: %s|] s typ e)
              get ∷ 𝕋 → (𝕋 → 𝔼 𝕋 α) → 𝔼 𝕋 α
              get name f = maybe (𝕷 $ [fmt|no %t found|] name)
                               f (identifiers ⫤ name)

--          len ← maybe (𝕷 "no length found") (parseTextual ∘ (⊕"s")) ((takeWhile isDigit) ⊳ ("ID_LENGTH" `Map.lookup` identifiers))
          len ← get "ID_LENGTH" (parseTextual ∘ (⊕"s"))
          width ← get "ID_VIDEO_WIDTH" (readEitherT "ℕ")
          height ← get "ID_VIDEO_HEIGHT" (readEitherT "ℕ")
          sz ← maybe (𝕷 "empty stat") (𝕽 ∘ size) st
          return (len,width,height,sz)

    case opts ⊣ mode of
      ModeRaw → do forM_ (sortOn fst $ Map.toList m_identifiers) printRaw ⪼ return 𝕹
      ModeParsed → do
        st ← stat Informational 𝕹 input NoMock
        case printParsed st m_identifiers of
          𝕽 (len,width,height,sz) → do say ([fmtT|%T\t%,10d\t%10t\t%8T|] input sz ([fmt|%dx%d|] width height)  len) ⪼ return 𝕹
                                  -- say $ [fmtT|%T\t%,10d\t%8T|] input sz len
                                  -- return 𝕹
          𝕷 e       → return $ 𝕵 ([fmtT|%T: %t|] input e)

  case catMaybes $ toList xs of
    [] → return 0
    xs' → forM_ xs' (\ x → warn x) ⪼ return 10

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "FIX ME"
      my_main = myMain @UsageFPProcIOError
  getArgs ≫ (\ args → stdMainNoDR progDesc parseOptions my_main args)
