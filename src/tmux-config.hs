{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

import Base1

import Debug.Trace  ( traceShow )
import Prelude  ( error, round )

-- base --------------------------------

import Control.Exception   ( Handler( Handler ), SomeException
                           , catches, displayException )
import Data.Char           ( isAlphaNum, isAscii, isControl )
import Data.List           ( intercalate, reverse, sortOn, take
                           , takeWhile, zip )
import Data.Maybe          ( catMaybes )
import Data.Tuple          ( uncurry )
import System.Timeout      ( timeout )
import Text.Read           ( readEither )

-- base-unicode-symbols ----------------

import Prelude.Unicode  ( (≠) )

-- bytestring --------------------------

import Data.ByteString.Lazy  qualified as LBS

-- domainnames -------------------------

import DomainNames.Hostname  ( hostlocal )

-- duration ----------------------------

import Duration  ( Duration( SECS ), asMicroseconds )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask )

-- fpath -------------------------------

import FPath.Parseable  qualified

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.Error.FPathError  ( AsFPathError, FPathError )
import FPath.Parseable         ( Parseable, parseDir )
import FPath.RelFile           ( RelFile )

-- http-client -------------------------

import Network.HTTP.Client          ( httpLbs, newManager, parseRequest
                                    , responseBody )
import Network.HTTP.Client.Internal ( HttpException( HttpExceptionRequest
                                                   , InvalidUrlException ) )
import Network.HTTP.Client.TLS      ( tlsManagerSettings )

-- ip4 ---------------------------------

import IP4  ( IP4 )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity( Informational ) )

-- log-plus ----------------------------

import Log  ( Log )

-- mockio-cmds-inetutils ---------------

import MockIO.Cmds.InetUtils.Hostname  ( hostname )

-- mockio-log --------------------------

import MockIO.Log          ( logit )
import MockIO.MockIOClass  ( MockIOClass )
import MockIO.DoMock       ( DoMock( NoMock ), HasDoMock )

-- mockio-plus -------------------------

import MockIO.Process            ( ꙩ )
import MockIO.Process.MLCmdSpec  ( MLCmdSpec, ToMLCmdSpec )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( throwUserError )

-- monadio-plus ------------------------

import MonadIO                        ( say )
import MonadIO.Base                   ( getArgs )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.Process.CmdSpec        ( cwd )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊩) )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, ReaderT, runReaderT )

-- natural -----------------------------

import Natural.Length  ( ỻ )

-- network-info ------------------------

import Network.Info  qualified as  NI
import Network.Info  ( getNetworkInterfaces )

-- optparse-applicative ----------------

import Options.Applicative        ( argument, eitherReader, metavar )
import Options.Applicative.Types  ( Parser, ReadM )

-- parsec-plus -------------------------

import ParsecPlus  ( AsParseError, Parsecable, parsec )

-- pcre --------------------------------

import PCRE          ( PCRE, compRE )
import PCRE.Error    ( AsREParseError, PCREScriptError )
import PCRE.GroupID  ( GroupID( GIDName ) )
import PCRE.REMatch  ( (≃) )

-- stdmain -----------------------------

import StdMain                       ( stdMainNoDR )
import StdMain.ProcOutputParseError  ( AsProcOutputParseError, ScriptError
                                     , throwAsProcOutputParseError )

-- tasty -------------------------------

import Test.Tasty                              ( TestTree
                                               , localOption, testGroup )
import Test.Tasty.Ingredients.ConsoleReporter  ( UseColor( Never ) )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  qualified as  T

import Data.Text.Encoding  ( decodeUtf8 )

-- text-printer ------------------------

import qualified Text.Printer  as P

--------------------------------------------------------------------------------

ð ∷ Default α => α
ð = def

{-| for testing mock/log commands

    e.g., `runCmd @ScriptError $ hostname Informational`
-}
runCmd ∷ ∀ ε α μ . (MonadIO μ, MonadMask μ) =>
         ExceptT ε (LoggingT (Log MockIOClass) (ReaderT DoMock μ)) α → μ (𝔼 ε α)
runCmd = flip runReaderT NoMock ∘ logit -- @ScriptError

tmux_path ∷ AbsFile
tmux_path = [absfile|/home/martyn/.nix-profiles/default/bin/tmux|]

git_path ∷ AbsFile
git_path = [absfile|/run/current-system/sw/bin/git|]

git ∷ ∀ ε δ μ . (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                 AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
                 AsIOError ε, Printable ε, MonadError ε μ,
                 MonadLog (Log MockIOClass) μ) ⇒
      AbsDir → [𝕋] → μ 𝕋
git d args = snd ⊳ ꙩ (git_path,args,mlCmdSpecSetCWD @𝕋 d)

gits ∷ ∀ ε δ μ . (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                 AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
                 AsIOError ε, Printable ε, MonadError ε μ,
                 MonadLog (Log MockIOClass) μ) ⇒
      AbsDir → [𝕋] → μ [𝕋]
gits d args = snd ⊳ ꙩ (git_path,args,mlCmdSpecSetCWD @[𝕋] d)

------------------------------------------------------------

data Options = Options { -- _mode   ∷ Mode
                         -- , _inputs ∷ NonEmpty File
                         _dir ∷ AbsDir
                       }

dir ∷ Lens' Options AbsDir
dir = lens _dir (\ o d → o { _dir = d })

----------------------------------------

-- add this to FPath.Parseable
readMDir ∷ Parseable χ ⇒ ReadM χ
readMDir = eitherReader (first toString ∘ parseDir @_ @FPathError)

parseOptions ∷ Parser Options
parseOptions = -- pure Options
  Options ⊳ argument readMDir (metavar "DIR")
{-
  Options ⊳ ( flag (ModeParsed Human) ModeRaw
                   (long "raw" ⊕ help "output all ID_ tags")
            ∤ flag (ModeParsed Human) (ModeParsed Tabs)
                   (long "tabs" ⊕ help "output tab-delimited"))
          ⊵ parseNE (argument readM (metavar "FILENAME"))
-}

------------------------------------------------------------

data SavedDefault   deriving Show
data UnsavedDefault deriving Show
-- a Format is basically a newtype around Text, but used with
-- SavedDefault or UnsavedDefault as a phantom type
newtype Format α = Format { unFormat ∷ 𝕋 }

instance Printable (Format α) where
  print = P.text ∘ unFormat

class ToFormat α where
  toFormat :: α → Format UnsavedDefault

instance ToFormat () where
  toFormat () = Format ""

instance ToFormat 𝕋 where
  toFormat = Format

instance ToFormat α => ToFormat [α] where
  toFormat as = Format $ ю [ unFormat $ toFormat a | a ← as ]

saveDefault ∷ ToFormat α => α → Format SavedDefault
saveDefault f = Format $ [fmt|#[push-default]%T#[pop-default]|] (toFormat f)

------------------------------------------------------------

data StyleVariable = StatusLeftStyle
                   | StatusRightStyle
                   | StyleText 𝕋
                   | WindowStatusStyle
                   | WindowStatusActivityStyle
                   | WindowStatusBellStyle
                   | WindowStatusCurrentStyle
                   | WindowStatusLastStyle
  deriving Show

instance Printable StyleVariable where
  print StatusLeftStyle           = P.text "status-left-style"
  print StatusRightStyle          = P.text "status-right-style"
  print (StyleText t)             = P.text t
  print WindowStatusStyle         = P.text "window-status-style"
  print WindowStatusActivityStyle = P.text "window-status-activity-style"
  print WindowStatusBellStyle     = P.text "window-status-bell-style"
  print WindowStatusCurrentStyle  = P.text "window-status-current-style"
  print WindowStatusLastStyle     = P.text "window-status-last-style"

instance ToFormat StyleVariable where
  toFormat o = Format $ [fmt|%T|] o

------------------------------------------------------------

data StyleExpr = DefaultStyle | StyleExp StyleVariable  deriving Show

instance ToFormat StyleExpr where
  toFormat DefaultStyle = Format "default"
  toFormat (StyleExp se) = toFormat se

------------------------------------------------------------

{-| A user-option, which should begin with a '@'.  In a better world,
    we would check that at construction time.  We could use quasi-quoting,
    but that requires a separate file due to staging restrictions. -}
newtype UserVariable = UserVariable 𝕋

instance Show UserVariable where
  show (UserVariable t) = "UserVariable: '" ◇ T.unpack t ◇ "'"

instance Printable UserVariable where
  print (UserVariable t) = P.text t

instance ToFormat UserVariable where
  toFormat o = Format $ [fmt|#{%T}|] o

userVariable ∷ 𝕋 → UserVariable
userVariable   (T.uncons → 𝓝)          = error "userVariable: empty text"
userVariable t@(T.uncons → 𝓙 ('@', _)) = UserVariable t
userVariable t                         = error $ "userVariable: '" ◇ T.unpack t ◇ "'"

------------------------------------------------------------

data FormatVariable = StatusLeft | StatusRight | WindowStatusCurrentFormat
                    | WindowStatusFormat
  deriving Show

instance Printable FormatVariable where
  print StatusLeft                = "status-left"
  print StatusRight               = "status-right"
  print WindowStatusFormat        = "window-status-format"
  print WindowStatusCurrentFormat = "window-status-current-format"

instance ToFormat FormatVariable where
  toFormat o = Format $ [fmt|#{%T}|] o

------------------------------------------------------------

data BooleanVariable = WindowEndFlag | WindowLastFlag | WindowActivityFlag
                     | WindowBellFlag | WindowSilenceFlag
  deriving Show

instance Printable BooleanVariable where
  print WindowEndFlag      = "window_end_flag"
  print WindowBellFlag     = "window_bell_flag"
  print WindowLastFlag     = "window_last_flag"
  print WindowActivityFlag = "window_activity_flag"
  print WindowSilenceFlag  = "window_silence_flag"

instance ToFormat BooleanVariable where
  toFormat bv = Format $ [fmt|#{%T}|] bv

------------------------------------------------------------

data Variable = BoolVar   BooleanVariable
              | FormatVar FormatVariable
              | StyleVar  StyleVariable
              | UserVar   UserVariable
              | StringVar StringVariable
  deriving Show

instance Printable Variable where
  print (BoolVar   bv) = print bv
  print (FormatVar fv) = print fv
  print (StringVar sv) = print sv
  print (StyleVar  sv) = print sv
  print (UserVar   sv) = print sv

instance ToFormat Variable where
  toFormat (BoolVar   bv) = Format $ [fmt|#{%T}|] bv
  toFormat (FormatVar fv) = Format $ [fmt|toFormat FormatVar %w|] fv
  toFormat (StringVar sv) = Format $ [fmt|toFormat StringVar %w|] sv
  toFormat (StyleVar  sv) = Format $ [fmt|toFormat StyleVar %w|] sv
  toFormat (UserVar   uv) = Format $ [fmt|toFormat UserVar %w|] uv

------------------------------------------------------------

data NatExpr = NatLit ℕ  deriving Show

instance Printable NatExpr where
  print (NatLit n) = P.text $ [fmt|%d|] n

instance ToFormat NatExpr where
  toFormat nx = Format $ "NatExpr: [" ◇ T.pack (show nx) ◇ "]"

------------------------------------------------------------

data StringExpr = SVar StringVariable | StyExp StyleExpr
                | StrTxt 𝕋
  deriving Show

instance ToFormat StringExpr where
  toFormat (StrTxt t)  = Format t
  toFormat (StyExp sx) = toFormat sx
  toFormat sx = Format $ "StringExpr: [" ◇ T.pack (show sx) ◇ "]"

------------------------------------------------------------

data BoolExpr = BVar BooleanVariable | And BoolExpr BoolExpr
              | Or BoolExpr BoolExpr | StrNotEq StringExpr StringExpr
  deriving Show

instance Printable BoolExpr where
  print (BVar bo) = print bo
  print x         = P.string $ show x

qualify ∷ BoolExpr → Format UnsavedDefault
qualify (BVar bo) = Format $ [fmt|#{%T}|] bo
qualify bx        = toFormat bx

instance ToFormat BoolExpr where
  toFormat (BVar bo) = Format $ [fmt|%T|] bo
  toFormat (And x y) =
    -- testing shows that &&: doesn't work with raw var names,
    -- we always need a #{..} form
    Format $ [fmt|#{&&:%T,%T}|] (qualify x) (qualify y)
  toFormat (Or x y) =
    Format $ [fmt|#{||:%T,%T}|] (qualify x) (qualify y)
  toFormat (StrNotEq x y) = Format $ [fmt|#{!=:%T,%T}|] (toFormat x) (toFormat y)

----------------------------------------

strNotEq ∷ (ToFormat α, ToFormat β) => α → β → BoolExpr
strNotEq a b = let toT ∷ ToFormat γ => γ → StringExpr
                   toT = StrTxt ∘ toText ∘ toFormat
               in  StrNotEq (toT a) (toT b)

------------------------------------------------------------

data StringVariable = WindowStatusSeparator | WindowName  deriving Show


instance Printable StringVariable where
  print WindowStatusSeparator  = "window-status-separator"
  print WindowName             = "window_name"

instance ToFormat StringVariable where
  toFormat v = Format $ [fmt|#{%T}|] v

------------------------------------------------------------

{-| Tmux "options" that evaluate to an integer value -}
data IntVariable = StatusLeftLength | StatusRightLength | WindowIndex
  deriving Show

instance Printable IntVariable where
  print StatusLeftLength  = "status-left-length"
  print StatusRightLength = "status-right-length"
  print WindowIndex       = "window_index"

instance ToFormat IntVariable where
  toFormat io = Format $ [fmt|#{%T}|] io

------------------------------------------------------------

newtype Option α = Option α
  deriving (Printable, Show, ToFormat)

------------------------------------------------------------

data AlignVariable = {-| Set the position of the window list in the status line:
                         left, centre or right. centre puts the window list in
                         the relative centre of the available free space;
                         absolute-centre uses the centre of the entire
                         horizontal space. -}
                     StatusJustify deriving Show

instance Printable AlignVariable where
  print StatusJustify = "status-justify"

------------------------------------------------------------

data Alignment = AlignLeft | AlignRight | AlignCentre | AlignOpt AlignVariable
  deriving Show

instance Printable Alignment where
  print AlignLeft     = "align=left"
  print AlignCentre   = "align=centre"
  print AlignRight    = "align=right"
  print (AlignOpt ao) = P.text $ [fmt|align=#{%T}|] ao

------------------------------------------------------------

data RangeStyle = {-| When a mouse event occurs in the range=left or range=right
                      range, the  ‘StatusLeft’  and  ‘StatusRight’ key bindings
                      are triggered. -}
                  RangeLeft | RangeRight
                | RangeNone
                  {-| range=window|X is the range for a window.  This triggers
                      the ‘Status’ mouse key with the target window given by the
                      ‘X’ argument.  ‘X’ is a window index in the current
                      session. The mouse_status_range format variable will be
                      set to ‘window’. -}
                | RangeWindow IntVariable
  deriving Show

instance Printable RangeStyle where
  print RangeLeft        = "range=left"
  print RangeRight       = "range=right"
  print RangeNone        = "norange"
  print (RangeWindow io) = P.text $ [fmt|range=window|%T|] (toFormat io)

------------------------------------------------------------

data ListStyle = {-| list=on marks the start of the list -}
                 ListOn
                 {-| list=focus is the part of the  list  that should be kept in
                     focus if the entire list won't fit in the available space
                     (typically the current window); -}
               | ListFocus
                 {-| list=left-marker and list=right-marker mark the text to be
                     used to mark that text has been trimmed from the left or
                     right of the list if there is not enough space.-}
               | ListLeftMarker 𝕋 | ListRightMarker 𝕋
               | ListNone
  deriving Show

instance Printable ListStyle where
  print ListOn              = "list=on"
  print ListFocus           = "list=focus"
  print (ListLeftMarker _)  = "list=left-marker"
  print (ListRightMarker _) = "list=right-marker"
  print ListNone            = "nolist"

listPayload ∷ ListStyle → 𝕄 𝕋
listPayload ListOn              = 𝓝
listPayload ListFocus           = 𝓝
listPayload (ListLeftMarker  t) = 𝓙 t
listPayload (ListRightMarker t) = 𝓙 t
listPayload ListNone            = 𝓝

------------------------------------------------------------

-- tmux' colour nummbers, run tmux-colours to see them all
newtype Colour8 = Colour8 Word8  deriving  (Eq,Show)

instance Printable Colour8 where print (Colour8 c) = P.text $ [fmt|%d|] c

data StyleDefault = StyleDefault | NoStyleDefault  deriving Show

data Style α = Style { _styleDefault ∷ StyleDefault
                     , _alignStyle   ∷ 𝕄 Alignment
                     , _rangeStyle   ∷ 𝕄 RangeStyle
                     , _listStyle    ∷ 𝕄 ListStyle
                     , _fg           ∷ 𝕄 Colour8
                     , _bg           ∷ 𝕄 Colour8
                     , _stylePayload ∷ 𝕄 α
                     }
  deriving Show

alignStyle :: Lens' (Style α) (𝕄 Alignment)
alignStyle = lens _alignStyle (\ s a → s { _alignStyle = a })

rangeStyle :: Lens' (Style α) (𝕄 RangeStyle)
rangeStyle = lens _rangeStyle (\ s a → s { _rangeStyle = a })

styleDefault :: Lens' (Style α) StyleDefault
styleDefault = lens _styleDefault (\ s a → s { _styleDefault = a })

listStyle :: Lens' (Style α) (𝕄 ListStyle)
listStyle = lens _listStyle (\ s a → s { _listStyle = a })

fg ∷ Lens' (Style α) (𝕄 Colour8)
fg = lens _fg (\ s c → s { _fg = c })

bg ∷ Lens' (Style α) (𝕄 Colour8)
bg = lens _bg (\ s c → s { _bg = c })

stylePayload ∷ Lens (Style α) (Style β) (𝕄 α) (𝕄 β)
stylePayload = lens _stylePayload (\ s a → s { _stylePayload = a })
stylePayload_ ∷ Lens' (Style α) (𝕄 α)
stylePayload_ = lens _stylePayload (\ s a → s { _stylePayload = a })

style :: α → Style α
style y = Style NoStyleDefault 𝓝 𝓝 𝓝 𝓝 𝓝 (𝓙 y)

instance Default (Style α) where
  def = Style NoStyleDefault 𝓝 𝓝 𝓝 𝓝 𝓝 𝓝

instance Show α => Printable (Style α) where print s = P.string (show s)

instance ToFormat α => ToFormat (Style α) where
  toFormat s =
    let pieces = [ case s ⊣ styleDefault of
                     StyleDefault   → 𝓙 "default"
                     NoStyleDefault → 𝓝
                 , [fmt|%T|] ⊳ (s ⊣ rangeStyle)
                 , [fmt|%T|] ⊳ (s ⊣ listStyle)
                 , [fmt|%T|] ⊳ (s ⊣ alignStyle)
                 , [fmt|fg=colour%T|] ⊳ (s ⊣ fg)
                 , [fmt|bg=colour%T|] ⊳ (s ⊣ bg)
                 , toText ∘ toFormat ⊳ (s ⊣ stylePayload_)
                 ]
        payload = "" ⧐ (s ⊣ listStyle ≫ listPayload)
    in  Format $ [fmt|#[%t]%t|] (T.intercalate " " $ catMaybes pieces) payload

-- I originally chose infixl (weakly); but that causes tmf $ x & y ≈ z to not
-- parse.  Using infixr fixes that!
infixr 0 ≈
(≈) ∷ Style α → β → Style β
a ≈ b = a & stylePayload ⊩ b

ꝏ ∷ Style ()
ꝏ = def

------------------------------------------------------------

data LenSpec = FixedLen ℤ | OptLen IntVariable
  deriving Show

instance Printable LenSpec where
  print (FixedLen l) = P.text $ [fmt|=%d|]   l
  print (OptLen   o) = P.text $ [fmt|=/#{%T}|] o

--------------------

data WithStrftime = WithStrftime | WithoutStrftime deriving Show

--------------------

instance Printable WithStrftime where
  print WithStrftime    = "T"
  print WithoutStrftime = "E"

------------------------------------------------------------

class IsVariable α

instance IsVariable BooleanVariable
instance IsVariable FormatVariable
instance IsVariable StringVariable
instance IsVariable StyleVariable
instance IsVariable UserVariable

------------------------------------------------------------

{- A format specifier is a #{…} group -}
data FormatSpecifier α = IsVariable α => BareVariable α
                       | ExpandTwice WithStrftime (FormatSpecifier α)
                       | MaxLen LenSpec (FormatSpecifier α)

--------------------

instance Show α => Show (FormatSpecifier α) where
  show (BareVariable v)     = [fmt|IsVariable %w|] v
  show (ExpandTwice wsf v) = [fmt|ExpandTwice %w %w|] wsf v
  show (MaxLen ls v)       = [fmt|MaxLen %w %w|] ls v

------------------------------------------------------------

stackRank ∷ FormatSpecifier α → Word8
stackRank (ExpandTwice _ _) = 2
stackRank (MaxLen      _ _) = 1
stackRank _                 = 0

----------------------------------------

innerFormatSpecifier :: FormatSpecifier α → 𝕄 (FormatSpecifier α)
innerFormatSpecifier (BareVariable   _)      = 𝓝
innerFormatSpecifier (MaxLen        _  fs)  = 𝓙 fs
innerFormatSpecifier (ExpandTwice   _  fs)  = 𝓙 fs

--------------------

instance (Show α, ToFormat α, Printable α) => Printable (FormatSpecifier α) where
  print (BareVariable  t)           = print t
  print (ExpandTwice w_strftime _) = P.text $ [fmt|%T|] w_strftime
  print (MaxLen      len_spec   _) = P.text $ [fmt|%T|] len_spec

--------------------

toStackedFormat ∷ (Printable α, ToFormat α, Show α) =>
                  [FormatSpecifier α] → FormatSpecifier α → Format β
toStackedFormat stack ofs =
  case innerFormatSpecifier ofs of
    𝓙 ifs → toStackedFormat (ofs:stack) ifs
    _     → case toText ⊳ reverse (sortOn stackRank stack) of
              []   → Format $ [fmt|#{%T}|] ofs
              stck → Format $ [fmt|#{%t:%T}|] (T.intercalate ";" stck) ofs

instance (Show α, ToFormat α, Printable α) => ToFormat (FormatSpecifier α) where
  toFormat ofs            = toStackedFormat [] ofs

_E ∷ FormatSpecifier α → FormatSpecifier α
_E = ExpandTwice WithoutStrftime
_e ∷ IsVariable α => α → FormatSpecifier α
_e = _E ∘ BareVariable
_T ∷ FormatSpecifier α → FormatSpecifier α
_T = ExpandTwice WithStrftime
_t ∷ IsVariable α => α → FormatSpecifier α
_t = _T ∘ BareVariable

------------------------------------------------------------

data TMuxFormatTyped α = (Show α, ToFormat α, IsVariable α) => TMFV α
                       | (Show α, ToFormat α) => TMFY (Style α)
                       | (Show α, ToFormat α, Printable α) =>
                           TMFS (FormatSpecifier α)
                       | Show α => TMFF (Format α)
                       | -- conditional
                         TMFC BoolExpr (𝕄 (TMuxFormatTyped α))
                                       (𝕄 (TMuxFormatTyped α))
                       | TMFN NatExpr

instance Show (TMuxFormatTyped α) where
  show (TMFV v)          = [fmt|TMFV: %w|] v
  show (TMFY y)          = [fmt|TMFY: %w|] y
  show (TMFS s)          = [fmt|TMFS: %w|] s
  show (TMFN n)          = [fmt|TMFN: %w|] n
  show (TMFF (Format f)) = [fmt|TMFF: %w|] f
  show (TMFC p t e)      = [fmt|TMFC: %w %w %w|] p t e

instance ToFormat (TMuxFormatTyped α) where
  toFormat t = Format $ toText t

instance Printable (TMuxFormatTyped α) where
  print (TMFV v) = P.text ∘ unFormat $ toFormat v
  print (TMFN v) = P.text ∘ unFormat $ toFormat v
  print (TMFY y) = P.text ∘ unFormat $ toFormat y
  print (TMFS s) = P.text ∘ unFormat $ toFormat s
  print (TMFF f) = P.text ∘ unFormat $ f
  print (TMFC if_ then_ else_) =
    let def_empty = \ case 𝓝   → ""
                           𝓙 x → toText $ toFormat x
    in  P.text $ [fmt|#{?%T,%t,%t}|] (toFormat if_) (def_empty then_)
                                                    (def_empty else_)

------------------------------------------------------------

data TMuxFormat = ∀ α . TMFT (TMuxFormatTyped α)
                | TMFB BoolExpr
                | TMFZ NatExpr
                | TMFL [TMuxFormat]
                | {-| ‘S:’, ‘W:’, ‘P:’ or ‘L:’ will loop over each session,
                      window, pane or client  and  insert the format once for
                      each.  For windows and panes, two comma-separated formats
                      may be given: the second is used for the current window or
                      active pane. -}
                  TMF_W TMuxFormat (𝕄 TMuxFormat) -- W: (for each window)
                | TMF_P TMuxFormat (𝕄 TMuxFormat) -- P: (for each pane)
                | TMF_S TMuxFormat                -- S: (for each session)
                | TMF_L TMuxFormat                -- L: (for each client)

--------------------

instance Show TMuxFormat where
  show (TMFT  t)    = "TMFT: "  ◇ show t
  show (TMFZ  t)    = "TMFZ: "  ◇ show t
  show (TMFB  t)    = "TMFB: "  ◇ show t
  show (TMFL  ts)   = "TMFL: [" ◇ intercalate ", " (show ⊳ ts) ◇ "]"
  show (TMF_W x y)  = "TMF_W: " ◇ show x ◇ " " ◇ show y
  show (TMF_P x y)  = "TMF_P: " ◇ show x ◇ " " ◇ show y
  show (TMF_S x)    = "TMF_S: " ◇ show x
  show (TMF_L x)    = "TMF_L: " ◇ show x

--------------------

instance ToFormat TMuxFormat where
  toFormat t = Format $ toText t

instance Printable TMuxFormat where
  print (TMFT t) = print t
  print (TMFZ z) = print z
  print (TMFB b) = P.text ∘ unFormat $ toFormat b
  print (TMFL l) = P.text ∘ ю $ toText ⊳ l
  print (TMF_W w 𝓝)      = P.text $ [fmt|#{W:%T}|] (toText w)
  print (TMF_W w (𝓙 w')) = P.text $ [fmt|#{W:%T,%T}|] (toText w) (toText w')
  print (TMF_P p 𝓝)      = P.text $ [fmt|#{P:%T}|] (toText p)
  print (TMF_P p (𝓙 p')) = P.text $ [fmt|#{P:%T,%T}|] (toText p) (toText p')
  print (TMF_S s)        = P.text $ [fmt|#{S:%T}|] (toText s)
  print (TMF_L l)        = P.text $ [fmt|#{L:%T}|] (toText l)

forEachWindow ∷ (TMuxFormatable α, TMuxFormatable β) => α → 𝕄 β → TMuxFormat
forEachWindow w w' = TMF_W (tmf w) (tmf ⊳ w')

class TMuxFormatable α where
  tmf ∷ α → TMuxFormat

instance (Show α, ToFormat α) => TMuxFormatable (Style α) where
  tmf = TMFT ∘ TMFY

instance TMuxFormatable (TMuxFormatTyped α) where
  tmf = TMFT

class TMuxFormatTypedable α where
  type TMuxFormatTypedableType α
  tmft ∷ α → TMuxFormatTyped (TMuxFormatTypedableType α)

instance (Show α, ToFormat α) => TMuxFormatTypedable (Style α) where
  type TMuxFormatTypedableType (Style α) = α
  tmft = TMFY

instance (Show α, Printable α, ToFormat α) =>
         TMuxFormatTypedable (FormatSpecifier α) where
  type TMuxFormatTypedableType (FormatSpecifier α) = α
  tmft = TMFS

instance TMuxFormatTypedable StringVariable where
  type TMuxFormatTypedableType StringVariable = StringVariable
  tmft = TMFV

instance TMuxFormatTypedable (TMuxFormatTyped α) where
  type TMuxFormatTypedableType (TMuxFormatTyped α) = α
  tmft = id

conditional ∷ (TMuxFormatTypedable α) =>
              BoolExpr → Maybe α → Maybe α
            → TMuxFormatTyped (TMuxFormatTypedableType α)
conditional if_ then_ else_ = TMFC if_ (tmft ⊳ then_) (tmft ⊳ else_)

{- This fails with:

tmux-config.hs:525:17: error: [GHC-25897]
    • Couldn't match type ‘k’ with ‘*’
      Expected: Format @{k} α → TMuxFormatTyped α0
        Actual: Format @{*} α0 → TMuxFormatTyped α0
      ‘k’ is a rigid type variable bound by
        the instance declaration
        at tmux-config.hs:524:10-34
    • In the second argument of ‘(∘)’, namely ‘TMFF’
      In the expression: TMFT ∘ TMFF
      In an equation for ‘tmf’: tmf = TMFT ∘ TMFF
    • Relevant bindings include
        tmf :: Format α → TMuxFormat (bound at tmux-config.hs:525:4)

instance TMuxFormatable (Format α) where
   tmf = TMFT ∘ TMFF
-}

instance TMuxFormatable (Format UnsavedDefault) where
   tmf = TMFT ∘ TMFF

instance TMuxFormatable (Format SavedDefault) where
   tmf = TMFT ∘ TMFF

instance (Show α, ToFormat α, Printable α) =>
         TMuxFormatable (FormatSpecifier α) where
  tmf = TMFT ∘ TMFS

instance TMuxFormatable [TMuxFormat] where
  tmf = TMFL

instance TMuxFormatable BoolExpr where
  tmf = TMFB

--------------------

tmfv ∷ (Show α, ToFormat α, IsVariable α) => α → TMuxFormat
tmfv = TMFT ∘ TMFV

{- requires UndecidableInstances -}
-- instance (ToFormat α, IsVariable α) => TMuxFormatable α where tmf = tmfv
instance TMuxFormatable FormatVariable where
  tmf = tmfv
instance TMuxFormatable UserVariable where
  tmf = tmfv
instance TMuxFormatable NatExpr where
  tmf = TMFZ

------------------------------------------------------------
--             miscellaneous building blocks              --
------------------------------------------------------------

alignLeft ∷ Style α → Style α
alignLeft = alignStyle ⊩ AlignLeft

alignRight ∷ Style α → Style α
alignRight = alignStyle ⊩ AlignRight

--------------------

rangeLeft ∷ Style α → Style α
rangeLeft = rangeStyle ⊩ RangeLeft

rangeRight ∷ Style α → Style α
rangeRight = rangeStyle ⊩ RangeRight

rangeWinIY ∷ Style α → Style α
rangeWinIY = rangeStyle ⊩ RangeWindow WindowIndex

rangeNone ∷ Style α → Style α
rangeNone = rangeStyle ⊩ RangeNone

--------------------

styleDef ∷ Style α → Style α
styleDef = styleDefault ⊢ StyleDefault

--------------------

listOn ∷ Style α → Style α
listOn = listStyle ⊩ ListOn

listFocus ∷ Style α → Style α
listFocus = listStyle ⊩ ListFocus

noList ∷ Style α → Style α
noList = listStyle ⊩ ListNone

--------------------

{-| align the list; if either end gets trimmed, mark it as such -}
listAlignMark ∷ Alignment → 𝕋 → 𝕋 → TMuxFormat
listAlignMark align l r = tmf [ tmf $ ꝏ & listOn & alignStyle ⊩ align
                              , tmf $ ꝏ & listStyle ⊩ ListLeftMarker l
                              , tmf $ ꝏ & listStyle ⊩ ListRightMarker r
                              , tmf $ ꝏ & listOn
                              ]

{-| window-current-status-style, if that's not the default;
    else window-status-style -}
windowCurrentStatusOrStyle ∷ TMuxFormatTyped StyleVariable
windowCurrentStatusOrStyle =
  conditional (strNotEq (_e WindowStatusCurrentStyle) DefaultStyle)
              (𝓙 $ _e WindowStatusCurrentStyle)
              (𝓙 $ _e WindowStatusStyle)

{-| if   windows-last-flag ∧ (window-status-last-style != default)
    then window-status-last-style
    else nothing-}
windowStatusLastStyle ∷ TMuxFormatTyped StyleVariable
windowStatusLastStyle =
  conditional (And (BVar WindowLastFlag)
                   (strNotEq (_e WindowStatusLastStyle) DefaultStyle))
              (𝓙 $ _e WindowStatusLastStyle) 𝓝

{-| if ⋀ ( window-has-bell
         , window-status-bell-style != default )
    then window-status-bell-style
    else (if ⋀ ( (window-has-activity ∨ silence)
                 , window-status-activity-style != default )
          then window-status-activity-style
          else nothing)
 -}
showWindowBellOrActivity ∷ TMuxFormatTyped StyleVariable
showWindowBellOrActivity =
  let {- if ⋀ ( (window-has-activity ∨ silence)
              , window-status-activity-style != default )
         then window-status-activity-style
         else nothing
       -}
      show_window_activity ∷ TMuxFormatTyped StyleVariable
      show_window_activity =
        conditional (And (Or (BVar WindowActivityFlag)
                             (BVar WindowSilenceFlag))
                         (strNotEq (_e WindowStatusActivityStyle) DefaultStyle)
                    )
                    (𝓙 $ _e WindowStatusActivityStyle) 𝓝
  in  conditional (And (BVar WindowBellFlag)
                       (strNotEq (_e WindowStatusBellStyle) DefaultStyle))
                  (𝓙 ∘ tmft $ _e WindowStatusBellStyle)
                  (𝓙 ∘ tmft $ show_window_activity)

{-| WindowSeparator, unless this is the last window -}
windowSeparator ∷ TMuxFormatTyped StringVariable
windowSeparator = conditional (BVar WindowEndFlag) 𝓝 (𝓙 WindowStatusSeparator)

----------------------------------------

{-| format `v` to a max length of `l` -}
maxLen ∷ IsVariable ν => IntVariable → ν → FormatSpecifier ν
maxLen l v = _T $ MaxLen (OptLen l) (BareVariable v)

----------------------------------------

data Current = NotCurrent | IsCurrent

windowFormat ∷ Current → [TMuxFormat]
windowFormat current =
  let (current_list_focus,current_status_style,status_format,
       list_on) =
        case current of
          NotCurrent → ( id
                       , tmf $ _e WindowStatusStyle
                       , WindowStatusFormat
                       , id)
          IsCurrent  → ( listFocus
                       , tmf windowCurrentStatusOrStyle
                       , WindowStatusCurrentFormat
                       , listOn)
  in  [ tmf $ ð & rangeWinIY & current_list_focus
                    ≈ [ current_status_style
                      , tmf windowStatusLastStyle
                      , tmf showWindowBellOrActivity
                      ]
      , tmf $ saveDefault (_t status_format)
      , tmf $ ꝏ & rangeNone & styleDef & list_on
      , tmf $ windowSeparator
      ]

{-| config for a tmux status bar that shows status-left & status-right (but
    nothing inbetween -}
lrBar ∷ [TMuxFormat]
lrBar = [ tmf $ style(_e StatusLeftStyle) & rangeLeft & alignLeft
        , tmf $ saveDefault (maxLen StatusLeftLength StatusLeft)
        , tmf $ ꝏ & rangeNone & styleDef
        , tmf $ style(_e StatusRightStyle) & rangeRight & alignRight & noList
        , tmf $ saveDefault (maxLen StatusRightLength StatusRight)
        ]

-- main ------------------------------------------------------------------------

data OptionScope = OptionScopeGlobal

newtype OptionFlags = OptionFlags { _optionScope ∷ OptionScope }

instance Printable OptionFlags where
  print _ = "-g"

newtype OptionName = OptionName 𝕋 deriving Printable

optionName ∷ 𝕋 → OptionName
optionName t =
  if T.filter (\ c → isAlphaNum c ∨ c ∈ ("-_"∷𝕋)) t == t
  then OptionName t
  else error $ "illegal option name: '" ◇ T.unpack t ◇ "'"

------------------------------------------------------------

class ToTextss α where
  toTextss ∷ α → [[𝕋]]

data TMuxConfig = SetOption OptionName OptionFlags TMuxFormat
                | SetOptionL OptionName OptionFlags [TMuxFormat]

instance ToTextss TMuxConfig where
  toTextss (SetOption opt_name opt_flags tmux_format) =
    [[ "set-option", toText opt_flags, toText opt_name, [fmt|%q|] tmux_format ]]
  toTextss (SetOptionL opt_name opt_flags formats) =
    let fmtF (i,t) =
          ["set-option",toText opt_flags, [fmt|%T[%d]|] opt_name i, [fmt|%q|] t]
    in  fmtF ⊳ zip [(0∷ℕ)..] formats

instance ToTextss [TMuxConfig] where
  toTextss cs = ю (toTextss ⊳ cs)

------------------------------------------------------------

-- status-{left,right}
-- /nix/store/8v78vjs9qwl51z4c6lafakx2fhkp90qk-tmuxplugin-powerline-3.0.0/share/tmux-plugins/powerline/powerline.sh left
{-
λ> :!tmux display-message -p "#{status-left}"
#[fg=colour234,bg=colour148] #S:#I.#P #[default]#[fg=colour148,bg=colour90]#(/nix/store/8v78vjs9qwl51z4c6lafakx2fhkp90qk-tmuxplugin-powerline-3.0.0/share/tmux-plugins/powerline/powerline.sh left)
*Main MockIO.Cmds.InetUtils.Paths Control.Lens Data.List Safe Data.Function Data.Tuple
λ> :!tmux display-message -p "#{status-right}"
#(/nix/store/8v78vjs9qwl51z4c6lafakx2fhkp90qk-tmuxplugin-powerline-3.0.0/share/tmux-plugins/powerline/powerline.sh right)
-}

{-| catch even "runtime" exceptions, throw them as UserErrors -}
-- catchUserE ∷ ∀ ε α η . (AsIOError ε, MonadError ε η) ⇒ IO (η α) → IO (η α)
-- catchUserE io = catch io (\ (e∷SomeException) → ѥ (throwUserError $ show e))

----------------------------------------

isSimpleAscii ∷ ℂ → 𝔹
isSimpleAscii c = isAscii c ∧ ﬧ  (isControl c)

------------------------------------------------------------

newtype LanIPs = LanIPs { unLanIPs ∷ [NI.IPv4] }
  deriving Show

lanIPs ∷ MonadIO μ ⇒ μ LanIPs
lanIPs = liftIO $ LanIPs ⊳
  (getNetworkInterfaces ⊲ \ nis → [ NI.ipv4 ni | ni ← nis
           {- exclude lo                    -} , NI.name ni ≠ "lo"
           {- exclude unassigned interfaces -} , NI.ipv4 ni ≠ NI.IPv4 0 ])

----------------------------------------

{-| Issue an HTTP request, with a given timeout.  If no response is received
    within the time allowed, 𝓝 is returned -}
httpReq ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
          𝕊 → Duration → μ (𝕄 𝕋)
httpReq url timeoutμs =
  let catcher io =
        let some_ex_h (e∷SomeException) =
              let take_take      = take 20 ∘ takeWhile isSimpleAscii
              in  ѥ (throwUserError ∘ take_take $ displayException e)

            http_ex_h (e∷HttpException) =
              case e of
                HttpExceptionRequest _ ex → return $ throwUserError $ show ex
                InvalidUrlException _ ex  → return $ throwUserError ex

        in  catches io [ Handler http_ex_h, Handler some_ex_h ]
  in  join ∘ liftIO ∘ catcher ∘ ѥ ∘ asIOError $ do
        manager ← newManager tlsManagerSettings
        request ← parseRequest url
        timeout (round $ timeoutμs ⊣ asMicroseconds) $ do
          response ← httpLbs request manager
          return ∘ decodeUtf8 ∘ LBS.toStrict $ responseBody response

--------------------

{-| see `httReq`; but then attempt to parse the returned text -}
httpRequest ∷ ∀ ε α μ .
              (MonadIO μ, AsParseError ε, AsIOError ε, MonadError ε μ,
               Parsecable α) ⇒
              𝕊 → Duration → μ (𝕄 α)
httpRequest url timeoutμs = do
  html ← httpReq url timeoutμs
  case html of
    𝓝 → return 𝓝
    𝓙 t → case parsec t t of
            𝓛 e → join $ throwError e
            𝓡 r → return $ 𝓙 r

----------------------------------------

wanIP ∷ MonadIO μ ⇒ μ 𝕋
wanIP =
  let url     = "http://whatismyip.akamai.com"
  in  ѥ (httpRequest @ScriptError @IP4 url (SECS 2)) ⊲ \ case
        𝓛 _e    → "-ERR- " -- ◇ T.take 8 (toText e)
        𝓡 (𝓙 r) → toText r
        𝓡 𝓝     → "NONE"

----------------------------------------

-- add this to MLCmdSpec or similar
mlCmdSpecSetCWD ∷ AbsDir → MLCmdSpec ξ → MLCmdSpec ξ
mlCmdSpecSetCWD d mlcs = mlcs & cwd ⊢ 𝓙 d

-- add this to PCRE?
{-| Match an RE, pick out a named/numbered group -}
pickGroup ∷ PCRE → GroupID → 𝕋 → 𝕄 𝕋
pickGroup re group t = re ≃ t ≫ \ match → group ! match

--------------------

{-| If a PCRE matches, and the given group is found: use that, else return the whole text -}
pickGroupIfMatches ∷ PCRE → GroupID → 𝕋 → 𝕋
pickGroupIfMatches re group t =  pickGroup re group t ⧏ t

----------------------------------------

gitRemoteOriginBase ∷ ∀ ε δ μ .
                      (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                       AsProcExitError ε, AsCreateProcError ε, AsFPathError ε,
                       AsIOError ε, AsREParseError ε,
                       Printable ε, MonadError ε μ,
                       MonadLog (Log MockIOClass) μ) ⇒
                      AbsDir → μ 𝕋
gitRemoteOriginBase d = do
  remote_origin ← git d ["config", "--get", "remote.origin.url"]

  basename_re ← compRE "^git@[\\w.]+:([-\\w/]+/)*${name}([-\\w]+)\\.git$"
  let remote_origin_base =
        pickGroupIfMatches basename_re (GIDName "name") remote_origin

  say $ [fmtT|remote_origin: %w (%w)|] remote_origin_base remote_origin

  return remote_origin_base

----------------------------------------

chomp ∷ 𝕋 → 𝕋
chomp = T.dropWhileEnd (∈ ['\r','\n'])

gitSymbolicRefHeadBase ∷ ∀ ε δ μ .
                      (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                       AsProcExitError ε, AsCreateProcError ε, AsFPathError ε,
                       AsIOError ε, Printable ε, MonadError ε μ,
                       MonadLog (Log MockIOClass) μ) ⇒
                      AbsDir → μ 𝕋
gitSymbolicRefHeadBase d = do
  let setCWD = mlCmdSpecSetCWD @𝕋 d
  head_ref ← snd ⊳ ꙩ (git_path,["symbolic-ref","HEAD"∷𝕋],setCWD)

  let head_ref_base = chomp $ T.takeWhileEnd (≠ '/') head_ref
  say $ [fmtT|head_ref: %w (%w)|] head_ref_base head_ref

  return head_ref_base

----------------------------------------

data GitCommitDiffCount = GitCommitDiffCount { _git_dir        ∷ AbsDir
                                             , _commits_from   ∷ 𝕋
                                             , _commits_to     ∷ 𝕋
                                             {-| number of commits that `to` has
                                                 that `from` does not -}
                                             , _commits_ahead  ∷ ℕ
                                             {-| number of commits that `from`
                                                 has that `to` does not -}
                                             , _commits_behind ∷ ℕ
                                             }
  deriving Show

--------------------

commits_ahead ∷ Lens' GitCommitDiffCount ℕ
commits_ahead = lens _commits_ahead (\ gcdf a → (gcdf { _commits_ahead = a }))

--------------------

commits_behind ∷ Lens' GitCommitDiffCount ℕ
commits_behind = lens _commits_behind (\ gcdf b → (gcdf { _commits_behind = b }))

----------------------------------------

-- `git rev-list --left-right --count HEAD...origin/master will show something
-- like `35\t0` meaning that HEAD is 35 commits ahead of (remote) origin/master
-- and 0 commits behind
{-| How far ahead/behind is the local commit state compared to remote? -}
gitCommitDiffCount ∷ ∀ ε δ μ .
                      (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                       AsProcExitError ε, AsCreateProcError ε, AsFPathError ε,
                       AsIOError ε, AsProcOutputParseError ε,
                       Printable ε, MonadError ε μ,
                       MonadLog (Log MockIOClass) μ) ⇒
                      AbsDir → 𝕋 → 𝕋 → μ GitCommitDiffCount
gitCommitDiffCount d from {-^ e.g., "origin/master -} to {-^ e.g., "HEAD" -} = do
  let git_args = ["rev-list", "--left-right", "--count", to ◇ "..." ◇ from ]
  head_ref ← chomp ⊳ git d git_args

  case T.splitOn "\t" head_ref of
    [ahead_,behind_] → do
      ahead  ← readℕ "commits ahead" ahead_
      behind ← readℕ "commits behind" behind_
      return $ GitCommitDiffCount d from to ahead behind
    _ → throwAsProcOutputParseError $ [fmtT|no parse of rev-list: '%t'|] head_ref

newtype TMuxStatusGitCommitDiffCount =
  TMuxStatusGitCommitDiffCount GitCommitDiffCount
  deriving Show

instance Printable TMuxStatusGitCommitDiffCount where
  print (TMuxStatusGitCommitDiffCount gcdf) = P.text $
    [fmt|↑%d↓%d|] (gcdf ⊣ commits_ahead) (gcdf ⊣ commits_behind)

----------------------------------------

{-| Get the latest tag, the number of changes since then and the short name of
    the most recent commit -}
gitTagState ∷ ∀ ε δ μ .
              (MonadIO μ, HasDoMock δ, MonadReader δ μ,
               AsProcExitError ε,AsCreateProcError ε,AsFPathError ε,AsIOError ε,
               Printable ε, MonadError ε μ,
               MonadLog (Log MockIOClass) μ) ⇒
              AbsDir → μ (𝕋,𝕋,𝕋)
gitTagState d = do
  tag_state ← git d ["describe", "--tags", "--long"]

  let (tagname,tagchanges,tagref) =
        case reverse $ T.split (≡'-') tag_state of
          (ref : changes : name_r) →
            (T.intercalate "-" (reverse name_r), changes, ref)
          _ → (tag_state,"","")

  say $ [fmtT|tag state: %t//%t//%t|] tagname tagchanges tagref
  return (tagname,tagchanges,tagref)

------------------------------------------------------------

data FileChangeStats = FileChangeStats { _changedFile  ∷ RelFile
                                       , _linesAdded   ∷ ℕ
                                       , _linesRemoved ∷ ℕ
                                       }

----------------------------------------

readℕ ∷ (AsProcOutputParseError ε, MonadError ε η) ⇒ 𝕋 → 𝕋 → η ℕ
readℕ name t =
  case readEither (T.unpack t) of
    𝓛 e → throwAsProcOutputParseError $
            [fmtT|failed to read %t '%t' as ℕ: %s|] name t e
    𝓡 r → return r

parseFileChangeStats ∷ (AsProcOutputParseError ε,AsFPathError ε,MonadError ε η)⇒
                       𝕋 → η FileChangeStats
parseFileChangeStats t =
  let throwAP = throwAsProcOutputParseError
  in  case T.splitOn "\t" t of
        [added_,removed_,fn_] → do
          added   ← readℕ "lines added"   added_
          removed ← readℕ "lines removed" removed_
          fn      ← FPath.Parseable.parse fn_
          return $ FileChangeStats fn added removed

        _ → throwAP $ [fmtT|no parse of output line to git diff --numstat: %t|] t

------------------------------------------------------------

data StagedChangesFileStats  = StagedChangesFiles  FileChangeStats
data WorkingChangesFileStats = WorkingChangesFiles FileChangeStats

--------------------

data GitChangedFilesStats =
  GitChangedFilesStats { _workingChangesFilesStats ∷ [WorkingChangesFileStats]
                       , _stagedChangesFilesStats  ∷ [StagedChangesFileStats]
                       }

--------------------

workingChangesFileStats ∷ Lens' GitChangedFilesStats [WorkingChangesFileStats]
workingChangesFileStats =
  lens _workingChangesFilesStats (\ cfs w → cfs { _workingChangesFilesStats = w})

--------------------

workingChangesFileCount ∷ GitChangedFilesStats → ℕ
workingChangesFileCount = ỻ ∘ (⊣ workingChangesFileStats)

--------------------

stagedChangesFileStats ∷ Lens' GitChangedFilesStats [StagedChangesFileStats]
stagedChangesFileStats =
  lens _stagedChangesFilesStats (\ cfs s → cfs { _stagedChangesFilesStats = s })

--------------------

stagedChangesFileCount ∷ GitChangedFilesStats → ℕ
stagedChangesFileCount = ỻ ∘ (⊣ stagedChangesFileStats)

------------------------------------------------------------

gitWorkingChangesFileStats ∷ ∀ ε δ μ .
                             (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                              AsProcOutputParseError ε, AsFPathError ε,
                              AsCreateProcError ε, AsProcExitError ε,
                              AsIOError ε, Printable ε, MonadError ε μ,
                              MonadLog (Log MockIOClass) μ) ⇒
                             AbsDir → μ [WorkingChangesFileStats]
gitWorkingChangesFileStats d = do
  working_file_diffs ← gits d ["diff", "--numstat"∷𝕋]
  WorkingChangesFiles ⊳⊳ (mapM parseFileChangeStats working_file_diffs)

--------------------

gitStagedChangesFileStats ∷ ∀ ε δ μ .
                             (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                              AsProcOutputParseError ε, AsFPathError ε,
                              AsCreateProcError ε, AsProcExitError ε,
                              AsIOError ε, Printable ε, MonadError ε μ,
                              MonadLog (Log MockIOClass) μ) ⇒
                             AbsDir → μ [StagedChangesFileStats]
gitStagedChangesFileStats d = do
  working_file_diffs ← gits d ["diff", "--cached", "--numstat"∷𝕋]
  StagedChangesFiles ⊳⊳ (mapM parseFileChangeStats working_file_diffs)

{-| Get the latest tag, the number of changes since then and the short name of
    the most recent commit -}
gitChangedFilesStats ∷ ∀ ε δ μ .
              (MonadIO μ, HasDoMock δ, MonadReader δ μ,
               AsProcExitError ε, AsCreateProcError ε, AsFPathError ε,
               AsIOError ε, AsProcOutputParseError ε, Printable ε,MonadError ε μ,
               MonadLog (Log MockIOClass) μ) ⇒
              AbsDir → μ GitChangedFilesStats
gitChangedFilesStats d = do
  -- the "index" is files we have staged for commit; but not yet committed

  -- use `git diff-index --quiet HEAD --` to check for diffs between working
  --                                      directory and HEAD (by exit code)

  -- use `git diff-index --quiet HEAD --cached --` to check for diffs between
  --                                               the index and HEAD (by exit)

  -- use `git diff-files --quiet` to check for diffs between the working
  --                              directory and the index

  working_changes_file_stats ← gitWorkingChangesFileStats d
  staged_changes_file_stats ← gitStagedChangesFileStats d

  return $
    GitChangedFilesStats { _workingChangesFilesStats = working_changes_file_stats
                         , _stagedChangesFilesStats  = staged_changes_file_stats
                         }

------------------------------------------------------------

newtype TMuxStatusGitChangedFilesStats =
  TMuxStatusGitChangedFilesStats GitChangedFilesStats

----------

instance Printable TMuxStatusGitChangedFilesStats where
  print (TMuxStatusGitChangedFilesStats gcfs) = P.text $
    case (workingChangesFileCount gcfs, stagedChangesFileCount gcfs) of
      (  0,  0) → ""
      (wfc,  0) → [fmt|%d★|]      wfc -- ⭐ -- ᕯ
      (  0,sfc) → [fmt|⁑[%d]|]    sfc -- 🔯
      (wfc,sfc) → [fmt|%d⁂[%d]|] wfc sfc -- 🌠

------------------------------------------------------------

(‼) ∷ (MonadIO μ, MonadReader δ μ, HasDoMock δ, ToMLCmdSpec (α, β) (),
       AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
       Printable ε, MonadError ε μ, MonadLog (Log MockIOClass) μ) =>
      α → β → μ ()
cmd ‼ args = ꙩ (cmd,args) ≫ return ∘ snd

spaces ∷ 𝕋 → 𝕋
spaces t = " " ◇ t ◇ " "

colourFmt ∷ 𝕋 → (Colour8, Colour8) → 𝕋
colourFmt t (fg_,bg_) = [fmtT|%T%T|] (tmf $ ꝏ & fg ⊩ fg_ & bg ⊩ bg_ & styleDef) t

----------------------------------------

-- `ip monitor -tshort address` to see addresses come & go
-- output e.g.,
-- [2025-10-09T12:42:14.472565] Deleted 2: wlp0s20f3    inet 192.168.0.10/24 brd 192.168.0.255 scope global noprefixroute wlp0s20f3
-- [2025-10-09T12:42:18.677739] 2: wlp0s20f3    inet 192.168.0.10/24 brd 192.168.0.255 scope global noprefixroute wlp0s20f3
lanWanIPs ∷ MonadIO μ ⇒ μ [𝕋]
lanWanIPs = do
  lan_ips ← lanIPs
  wan_ip ← case lan_ips of
             LanIPs [] → return ""
             _         → wanIP

  let lan_ips_str = case unLanIPs lan_ips of
                      []  → "NONE"
                      ips → T.intercalate "," (T.pack ∘ show ⊳ ips)
  return $ "ⓛ " ◇ lan_ips_str : case wan_ip of "" → []; _ → ["ⓦ " ◇ wan_ip]
-- XXX don't even try if there is no route?
-- XXX just drop this if there is no wan_ip
--         , "ⓦ " ◇ (wan_ip {- ⧏ "UNKNOWN" -})
--         ]

----------------------------------------

gitInWorkTree ∷ ∀ ε δ μ . (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                           AsFPathError ε, AsREParseError ε, AsCreateProcError ε,
                           AsProcExitError ε, AsIOError ε,
                           Printable ε, MonadError ε μ,
                           MonadLog (Log MockIOClass) μ) ⇒
                AbsDir → μ (𝔼 𝕋 ())
gitInWorkTree d =
  ѥ @ScriptError (git d ["rev-parse", "--is-inside-work-tree"]) ≫ \ case
    𝓛 e → return (𝓛 $ toText e)
    𝓡 _ → return (𝓡 ())

gitStatus ∷ ∀ ε δ μ . (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                       AsFPathError ε, AsREParseError ε, AsCreateProcError ε,
                       AsProcExitError ε, AsProcOutputParseError ε, AsIOError ε,
                       Printable ε,MonadError ε μ,
                       MonadLog (Log MockIOClass) μ) ⇒
            AbsDir → μ [𝕋]
gitStatus d = do
  in_work_tree ← traceShow "bart" $ gitInWorkTree d

  case traceShow "lisa" $ in_work_tree of
    𝓛 _ → traceShow "marge" $ return []
    𝓡 _ → traceShow "homer" $ do
      gcdf ← TMuxStatusGitCommitDiffCount ⊳ gitCommitDiffCount d "origin/master" "HEAD"

      remote_origin_base ← gitRemoteOriginBase d
      head_ref_base ← gitSymbolicRefHeadBase d
      return ()
      (tagname,tagchanges,_) ← ѥ @ScriptError (gitTagState d) ≫ \ case
                                 𝓛 _  → return ("","","")
                                 𝓡 xs → return xs


      stw ← ѥ @ScriptError (TMuxStatusGitChangedFilesStats ⊳ gitChangedFilesStats d) ≫ \ case
                𝓛 e → return $ T.take 8 $ toText e
                𝓡 cfs → return $ toText cfs

      return [ remote_origin_base
             , head_ref_base
             , ю [ if tagchanges≡"0"
                   then "✓" ◇ tagname
                   else tagname ◇ "+" ◇ tagchanges
                 , case stw of "" → ""; _ → " «" ◇ stw ◇ "»"
                 ]
             , toText gcdf
             ]

----------------------------------------

-- SessionName:WindowIndex.PaneIndex
-- WE SHOULD SET THE STATUS TO USE #S, etc., RATHER THAN CALLING THIS
{- | The current tmux session/window/pane ID, as a [𝕋] to form a tmux status-bar
     text group -}
sessionWindowPane ∷ ∀ ε δ μ .
                    (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                     AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
                     AsIOError ε, Printable ε, MonadError ε μ,
                     MonadLog (Log MockIOClass) μ) ⇒ μ [𝕋]
sessionWindowPane = snd ⊳ ꙩ (tmux_path,["display-message"∷𝕋, "-p", "#S:#I.#P"])

----------------------------------------

{- | The name of the local host (local part only), as a [𝕋] to form a tmux
     status-bar text group -}
getHost ∷ ∀ ε δ μ .
          (MonadIO μ, HasDoMock δ, MonadReader δ μ,
           AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
           AsProcOutputParseError ε,AsIOError ε,Printable ε,MonadError ε μ,
           MonadLog (Log MockIOClass) μ) ⇒
          μ [𝕋]
getHost = do hostname Informational ⊲ hostlocal ⊲ toText ⊲ pure


----------------------------------------

data TMuxColour = TMuxCol_Black | TMuxCol_White | TMuxCol_Yellow
                | TMuxCol_Magenta | TMuxCol_Blue_Dusky | TMuxCol_Red_Deep
                | TMuxCol_Grey_Green
  deriving (Eq,Ord)

-- these are tmux' colour numbers, run tmux-colours to see them all
tmuxColour8 ∷ TMuxColour → Colour8
tmuxColour8 TMuxCol_Black      = Colour8 234
tmuxColour8 TMuxCol_Yellow     = Colour8 148
tmuxColour8 TMuxCol_White      = Colour8 255
tmuxColour8 TMuxCol_Magenta    = Colour8 90
tmuxColour8 TMuxCol_Red_Deep   = Colour8 88
tmuxColour8 TMuxCol_Blue_Dusky = Colour8 24
tmuxColour8 TMuxCol_Grey_Green = Colour8 29


lefty_stuff ∷ ∀ ε δ μ .
              (MonadIO μ, HasDoMock δ, MonadReader δ μ,
               AsFPathError ε, AsCreateProcError ε, AsProcOutputParseError ε,
               AsProcExitError ε, AsParseError ε, AsREParseError ε, AsIOError ε,
               Printable ε, MonadError ε μ, MonadLog (Log MockIOClass) μ)⇒
              [(AbsDir → μ [𝕋],(TMuxColour,TMuxColour))]

lefty_stuff = [ (const sessionWindowPane, (TMuxCol_Black,    TMuxCol_Yellow))
              , (const getHost,           (TMuxCol_White,    TMuxCol_Magenta))
              , (const lanWanIPs,         (TMuxCol_White,    TMuxCol_Blue_Dusky))
              , (gitStatus,               (TMuxCol_Red_Deep, TMuxCol_Grey_Green))
              , (const (return [""]),     (TMuxCol_Black, TMuxCol_Black))
              ]

----------------------------------------

myMain ∷ ∀ ε . (HasCallStack, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                AsProcOutputParseError ε, AsProcExitError ε, AsREParseError ε,
                AsParseError ε, Printable ε) ⇒
               Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain opts = flip runReaderT NoMock $ do
  let status_format =
        let formats = [ tmf lrBar
                      , tmf [ listAlignMark (AlignOpt StatusJustify) "<" ">"
                            , forEachWindow (windowFormat NotCurrent)
                                            (𝓙 $ windowFormat IsCurrent)
                            ]
                      ]
        in  [ SetOptionL (optionName "status-format")
                         (OptionFlags OptionScopeGlobal) formats
            , SetOption (optionName "status") (OptionFlags OptionScopeGlobal)
                        (tmf $ NatLit (ỻ formats))
            ]


  -- emit a tmux command for each (set-option) in status_format
  forM_ (toTextss status_format) (tmux_path ‼)

  liftIO getNetworkInterfaces ≫ say ∘ [fmtT|getNetworkInterfaces: %w|]

  -- encapsulate into a function

  -- XXX USE THIS
  let join_outer :: LeftRight → [(𝕋,TMuxColour)] → 𝕋
      join_outer lr vals_cols =
        let join_two ∷ TMuxColour → TMuxColour → 𝕋
            join_two bg_ bg_' = colourFmt (separator Bold lr) (tmuxColour8 bg_,tmuxColour8 bg_')
            go ∷ [(𝕋,TMuxColour)] → 𝕋
            go (vc:vc':xs) = go [vc] ◇ join_two (snd vc) (snd vc') ◇ go (vc':xs)
            go [(t,_)] = t
            go [] = ""
        in go vals_cols

  let join_thin ∷ LeftRight → [𝕋] → (TMuxColour,TMuxColour) → 𝕋
      join_thin lr vals (fg_,bg_) =
        colourFmt (T.intercalate (separator Thin lr) (spaces ⊳ vals))
                  (tmuxColour8 fg_,tmuxColour8 bg_)

      -- like join_thin, but passes through the bg colour for use in
      -- join_outer
      join_thin' ∷ LeftRight → [𝕋] → (TMuxColour,TMuxColour) → (𝕋,TMuxColour)
      join_thin' lr vals (fg_,bg_) = (,bg_) $ join_thin lr vals (fg_,bg_)

  let mk_articles ∷ (Monad μ, Traversable ψ) ⇒
                    α → ((β,γ) → ω) → ψ (α → μ β,γ) → μ (ψ ω)
      mk_articles d g as = sequence $ (\ (f,x) → g ∘ (,x) ⊳ f d) ⊳ as
      articles' ∷ (MonadIO μ, HasDoMock δ, MonadReader δ μ,
                   AsFPathError ε,MonadError ε μ,MonadLog (Log MockIOClass) μ) ⇒
                   μ [(𝕋,TMuxColour)]
      articles' = mk_articles (opts ⊣ dir)
                              (uncurry $ join_thin' Left_)
                              lefty_stuff

  articles'' ∷ 𝕋 ← join_outer Left_ ⊳ articles'

  tmux_path ‼ ["display-message", "-d", "5000", articles'']
  return 0

data LeftRight = Left_ | Right_
data BoldThin = Bold | Thin
data PatchedFontInUse = PatchedFontInUse | NoPatchedFontInUse

separator' ∷ PatchedFontInUse → BoldThin → LeftRight → 𝕋
separator' PatchedFontInUse Bold Right_ = "\xe0b2" -- 
separator' PatchedFontInUse Bold Left_ = "\xe0b0" -- 
separator' PatchedFontInUse Thin Right_ = "\xe0b3" -- 
separator' PatchedFontInUse Thin Left_ = "\xe0b1" -- 
separator' NoPatchedFontInUse Bold Right_ = "\x25c0" -- ◀
separator' NoPatchedFontInUse Bold Left_ = "\x25b6" -- ▶
separator' NoPatchedFontInUse Thin Right_ = "\x276e" -- ❮
separator' NoPatchedFontInUse Thin Left_ = "\x276f" -- ❯

separator ∷ BoldThin → LeftRight → 𝕋
separator = separator' PatchedFontInUse

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "tmux config & helper"
      my_main = myMain @PCREScriptError
  getArgs ≫ (\ args → stdMainNoDR progDesc parseOptions my_main args)


--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = localOption Never $
  let ç      = T.intercalate ","

      len3               ∷ FormatSpecifier α → FormatSpecifier α
      len3               = MaxLen $ FixedLen 3
      len_left_length    ∷ FormatSpecifier α → FormatSpecifier α
      len_left_length    = MaxLen $ OptLen StatusLeftLength

      status_left_style  ∷ FormatSpecifier StyleVariable
      status_left_style  = _e StatusLeftStyle
      status_right_style ∷ FormatSpecifier StyleVariable
      status_right_style = _e StatusRightStyle
      status_left        ∷ FormatSpecifier FormatVariable
      status_left        = BareVariable StatusLeft
      status_right       ∷ FormatSpecifier FormatVariable
      status_right       = BareVariable StatusRight

      ts_ :: [(𝕋,Format SavedDefault)]
      ts_ =
        (second (Format ∘ toText) ⊳
         [ ( "#{window_name}", tmfv WindowName )
         , ( "#{@foobie}", tmf $ userVariable "@foobie" )
         , ( "#{=3:window_name}", tmf $ len3 $ BareVariable WindowName )
         , ( "#{=/#{status-left-length}:window_name}"
           , tmf $ (MaxLen $ OptLen StatusLeftLength) (BareVariable WindowName))
         , ( "#{T:@foobie}" , tmf $ _t $ userVariable "@foobie" )
         , ( -- "#{=3:#{E:@foobie}}" would also work, but is less compact
            "#{E;=3:@foobie}", tmf $ len3 (_e $ userVariable "@foobie") )

         {- The ordering of the T and the =1 doesn't matter; the T always
            effects:
            > $ tmux set-option @foobie %Y-%M-%d
            > $ tmux display-message -p '#{T;=/1:#{@foobie}}'
            > 2
            > $ tmux display-message -p '#{=/1:#{T:@foobie}}'
            > 2
         -}
         , ( "#{T;=3:@foobie}"
           , tmf $ _T $ len3 $ BareVariable $ userVariable "@foobie")
         , ( "#{T;=3:@foobie}", tmf $ len3 $ _t $ userVariable "@foobie")
         , ( "#{=/#{status-left-length}:window_name}"
           , tmf $ len_left_length $ BareVariable WindowName )
         , ( "#{E;=3:window_name}",
             tmf $ _E $ len3 $ BareVariable WindowName )
         , ( "#[range=left align=left #{E:status-left-style}]"
           , tmf $ style status_left_style & alignStyle   ⊩ AlignLeft
                                           & rangeStyle   ⊩ RangeLeft
           )
         , ( ю [ "#[push-default]"
               , "#{T;=/#{status-left-length}:status-left}"
               , "#[pop-default]" ]
           , tmf $ saveDefault (maxLen StatusLeftLength StatusLeft)
           )
         , ( ю [ "#[default norange]"
               , "#[range=right nolist align=right #{E:status-right-style}]"
               ]
           , tmf [ tmf $ ꝏ & rangeNone & styleDef
                 , tmf $ ð & listStyle    ⊩ ListNone
                           & alignStyle   ⊩ AlignRight
                           & rangeStyle   ⊩ RangeRight
                           & stylePayload ⊩ status_right_style
                 ]
           )
         , ( ю [ "#[push-default]"
               , "#{T;=/#{status-right-length}:status-right}"
               , "#[pop-default]"
               ]
           , tmf $ saveDefault (maxLen StatusRightLength StatusRight)
           )
         , ( "#[list=on align=#{status-justify}]"
           , tmf $ ꝏ & listOn & alignStyle ⊩ AlignOpt StatusJustify
           )
         , ( "#[list=left-marker]<"
           , tmf $ ꝏ & listStyle ⊩ ListLeftMarker "<"
           )
         , ( "#[list=right-marker]>"
           , tmf $ ꝏ & listStyle ⊩ ListRightMarker ">"
           )
         , ( "#[list=on]", tmf $ ꝏ & listOn )
         , ( "#{W:#{status-left},#{status-right}}",
             forEachWindow status_left (𝓙 status_right)
           )
         , ( "#{W:#[list=on],#[list=focus]}",
             forEachWindow (ꝏ & listOn) (𝓙 $ ꝏ & listFocus)
           )
         , ("#{?window_end_flag,,#{window-status-separator}}"
           , tmf $ TMFC
                (BVar WindowEndFlag) 𝓝 (𝓙 ∘ tmft $ WindowStatusSeparator)
           )
         , ( "#[push-default]#{T:window-status-format}#[pop-default]"
           , tmf $ saveDefault (_t WindowStatusFormat)
           )
         , ( "#[range=window|#{window_index} foo]"
           , tmf $ style (StyleText "foo") & rangeWinIY
           )
         , ( T.intercalate "," [ "#{&&:#{window_last_flag}"
                               , "#{!=:#{E:window-status-last-style}"
                               , "default}}"
                               ]
           , tmf (And (BVar WindowLastFlag)
                      (strNotEq (_e WindowStatusLastStyle) DefaultStyle))
           )
         , ( T.intercalate "," [ "#{?#{&&:#{window_last_flag}"
                               , "#{!=:#{E:window-status-last-style}"
                               , "default}}"
                               , "#{E:window-status-last-style}"
                               , "}"
                               ]
           , tmf $ windowStatusLastStyle
           )
         , ( "#{||:#{window_activity_flag},#{window_silence_flag}}"
           , tmf $ Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag)
           )

         , ( T.intercalate ","
             [ "#{&&:#{||:#{window_activity_flag},#{window_silence_flag}}"
             , "#{!=:#{E:window-status-activity-style}"
             , "default}}" ]
           , tmf $
               And (Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag))
                   (strNotEq (_e WindowStatusActivityStyle)
                             DefaultStyle)
           )
         , (ç [ ç [ "#{?#{&&:#{||:#{window_activity_flag}"
                  , "#{window_silence_flag}}"
                  , ç ["#{!=:#{E:window-status-activity-style}", "default}}"]
                  ]
              , "#{E:window-status-activity-style}"
              , "}"
              ]
           , tmf $ conditional
                 (And (Or (BVar WindowActivityFlag)(BVar WindowSilenceFlag))
                      (strNotEq (_e WindowStatusActivityStyle)
                                DefaultStyle))
                                (𝓙 $ _e WindowStatusActivityStyle) 𝓝
           )
         , ( "#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}"
           , tmf $ showWindowBellOrActivity
           )
         , ( "#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]"
           , let payload =
                   [ tmf windowCurrentStatusOrStyle
                   , tmf windowStatusLastStyle
                   , tmf showWindowBellOrActivity
                   ]
             in  tmf $ ð & rangeWinIY & listFocus ≈ payload
           )

         , ( ю [ "#[list=on align=#{status-justify}]#[list=left-marker]<#[list=right-marker]>#[list=on]#{W:#[range=window|#{window_index} #{E:window-status-style}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]#[push-default]#{T:window-status-format}#[pop-default]#[default norange]#{?window_end_flag,,#{window-status-separator}},#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]#[push-default]#{T:window-status-current-format}#[pop-default]#[default norange list=on]#{?window_end_flag,,#{window-status-separator}}}"
               ]
           , {-| the status line for each window, notably showing its
                 number, running program, and directory -}
             tmf [ listAlignMark (AlignOpt StatusJustify) "<" ">"
                 , forEachWindow
                     (windowFormat NotCurrent) (𝓙 $ windowFormat IsCurrent)
                 ]
           )

         , ( ю [ "#[range=left align=left #{E:status-left-style}]"
               , ю [ "#[push-default]"
                   , "#{T;=/#{status-left-length}:status-left}"
                   , "#[pop-default]"
                   ]
               , "#[default norange]"
               , "#[range=right nolist align=right #{E:status-right-style}]"
               , ю [ "#[push-default]"
                   , "#{T;=/#{status-right-length}:status-right}"
                   , "#[pop-default]"
                   ]
               ]
           , tmf lrBar
           )
         ])

      do_test ∷ (𝕋, Format α) → TestTree
      do_test (t,x) = let tname = if T.length t > 60
                                  then T.unpack (T.take 60 t) ◇ "…"
                                  else T.unpack t
                      in  testCase tname (t @=? toText x)
  in  testGroup "tests" $ do_test ⊳ ts_

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
