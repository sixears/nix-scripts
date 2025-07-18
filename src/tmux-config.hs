{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

import Base1

import Prelude  ( error )

-- base --------------------------------

import Data.List   ( reverse, sortOn )
import Data.Maybe  ( catMaybes, isJust )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode  ( (∧) )

-- monadio-plus ------------------------

import MonadIO  ( say )

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe  ( (⧐) )
import Data.MoreUnicode.Lens   ( (⊩) )
import Data.MoreUnicode.Maybe  ( pattern 𝓙, pattern 𝓝 )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  qualified as  T

-- text-printer ------------------------

import qualified Text.Printer  as P

--------------------------------------------------------------------------------

data SavedDefault -- = SavedDefault
data UnsavedDefault -- = UnsavedDefault
newtype Format α = Format { unFormat ∷ 𝕋 }

instance Printable (Format α) where
  print = P.text ∘ unFormat

class ToFormat α where
  toFormat :: α -> Format β

saveDefault ∷ Format UnsavedDefault → Format SavedDefault
saveDefault f = Format $ "#[push-default]" ⊕ toText f ⊕ "#[pop-default]"

------------------------------------------------------------

data StyleOption = StatusLeftStyle deriving Show

instance Printable StyleOption where
  print StatusLeftStyle = P.text "status-left-style"

instance ToFormat StyleOption where
  toFormat o = Format $ [fmt|%T|] o

------------------------------------------------------------

{-| A user-option, which should begin with a '@'.  In a better world,
    we would check that at construction time.  We could use quasi-quoting,
    but that requires a separate file due to staging restrictions. -}
newtype UserOption = UserOption 𝕋

instance Show UserOption where
  show (UserOption t) = "UserOption: '" ◇ T.unpack t ◇ "'"

instance Printable UserOption where
  print (UserOption t) = P.text t

instance ToFormat UserOption where
  toFormat o = Format $ [fmt|#{%T}|] o

userOption ∷ 𝕋 → UserOption
userOption   (T.uncons → 𝓝)          = error "userOption: empty text"
userOption t@(T.uncons → 𝓙 ('@', _)) = UserOption t
userOption t                         = error $ "userOption: '" ◇ T.unpack t ◇ "'"

------------------------------------------------------------

data FormatOption = StatusLeft
  deriving Show

instance Printable FormatOption where
  print StatusLeft = P.text "status-left"

instance ToFormat FormatOption where
  toFormat o = Format $ [fmt|#{%T}|] o

------------------------------------------------------------

data FormatVariable = WindowName deriving Show

instance ToFormat FormatVariable where
  toFormat WindowName = Format "#{window_name}"

instance Printable FormatVariable where
  print WindowName = P.text "window_name"

------------------------------------------------------------

{-| Tmux "options" that evaluate to an integer value -}
data IntOption = StatusLeftLength deriving Show

instance Printable IntOption where
  print StatusLeftLength = P.text "status-left-length"

instance ToFormat IntOption where
  toFormat io = Format $ [fmt|#{%T}|] io

------------------------------------------------------------

newtype Option α = Option α
  deriving (Printable, Show, ToFormat)

------------------------------------------------------------

data Alignment = AlignLeft | AlignRight | AlignCentre

instance Printable Alignment where
  print AlignLeft   = P.text "left"
  print AlignCentre = P.text "centre"
  print AlignRight  = P.text "right"

------------------------------------------------------------

data RangeStyle = RangeLeft | RangeRight

instance Printable RangeStyle where
  print RangeLeft  = P.text "left"
  print RangeRight = P.text "right"

------------------------------------------------------------

data Style = Style { _align        :: 𝕄 Alignment
                   , _range        :: 𝕄 RangeStyle
                   , _stylePayload :: 𝕄 (FormatSpecifier StyleOption)
                   }

align :: Lens' Style (𝕄 Alignment)
align = lens _align (\ s a -> s { _align = a })

range :: Lens' Style (𝕄 RangeStyle)
range = lens _range (\ s a -> s { _range = a })

stylePayload ∷ Lens' Style (𝕄 (FormatSpecifier StyleOption))
stylePayload = lens _stylePayload (\ s a -> s { _stylePayload = a })

emptyStyle :: Style
emptyStyle = Style 𝓝 𝓝 𝓝

instance ToFormat Style where
  toFormat s =
    let pieces = [ [fmt|align=%T|] ⊳ (s ⊣ align)
                 , [fmt|range=%T|] ⊳ (s ⊣ range)
                 , toText ∘ toFormat ⊳ (s ⊣ stylePayload)
                 ]
    in  Format $ [fmt|#[%t]|] (T.intercalate " " $ catMaybes pieces)

------------------------------------------------------------

data LenSpec = FixedLen ℤ | OptLen IntOption
  deriving Show

instance Printable LenSpec where
  print (FixedLen l) = P.text $ [fmt|=%d|]   l
  print (OptLen   o) = P.text $ [fmt|=/#{%T}|] o

--------------------

data WithStrftime = WithStrftime | WithoutStrftime deriving Show

--------------------

instance Printable WithStrftime where
  print WithStrftime    = P.text "T"
  print WithoutStrftime = P.text "E"

--------------------

data FormatSpecifier α = BareOption (Option α)
                       | ExpandTwice WithStrftime (FormatSpecifier α)
                       | MaxLen LenSpec (FormatSpecifier α)
  deriving Show

----------------------------------------

stackRank ∷ FormatSpecifier α → Word8
stackRank (ExpandTwice _ _) = 2
stackRank (MaxLen      _ _) = 1
stackRank _                 = 0

----------------------------------------

innerFormatSpecifier :: FormatSpecifier α → 𝕄 (FormatSpecifier α)
innerFormatSpecifier (BareOption  _)    = 𝓝
innerFormatSpecifier (MaxLen      _ fs) = 𝓙 fs
innerFormatSpecifier (ExpandTwice _ fs) = 𝓙 fs

--------------------

instance (Show α, ToFormat α, Printable α) => Printable (FormatSpecifier α) where
  print (BareOption t) = print t -- P.text $ [fmt|#{%T}|] t
--  print (ExpandTwice withStrftime (BareOption o)) =
--    P.text $ [fmt|A#{%T:%t}|] (toText withStrftime) (toText o)
-- XX do we still need these?
{-
  print e@(ExpandTwice withStrftime t) =
    if stackRank t > stackRank e
    then P.text $ [fmt|#{%T:%T} (t)|] (toText withStrftime) (toFormat t)  -- (toText t)
    else P.text $ [fmt|#{%T:%T} <-|] (toText withStrftime) (toFormat t)
-}
  print (ExpandTwice w_strftime _) = P.text $ [fmt|%T|] w_strftime
  print (MaxLen len_spec _) = P.text $ [fmt|%T|] len_spec
-- XXX REMOVE THIS
  print o = error $ [fmt|print fail: %w|] o

--------------------

-- internal helper functions for ToFormat
ifsExpandTwice with_strftime fs =
  case innerFormatSpecifier fs of
    𝓙 ifs → Format $ [fmt|#{%T;%T:%T}|] with_strftime fs (toFormat ifs)
    𝓝     → Format $ [fmt|C#{%T:%T}|] with_strftime fs

ifsMaxLen len_spec fs =
  case innerFormatSpecifier fs of
    𝓙 ifs → Format $ [fmt|H#{%T:%T}|] len_spec (toFormat fs)
    𝓝     → Format $ [fmt|G#{%T:%T}|] len_spec fs

toFormat_ ∷ (Printable α, Show α, ToFormat α) => FormatSpecifier α → Format β
toFormat_ (MaxLen l_spec fs) | stackRank fs > 0 = ifsMaxLen l_spec fs
                             | otherwise = Format $ [fmt|#{%T:%T}|] l_spec ( fs)

toFormat__ ∷ (Printable α, Show α) => FormatSpecifier α → Format β
toFormat__ (BareOption o) = Format $ [fmt|#{%T}|] o
toFormat__ x = Format $ [fmt|UUU [%w]|] x

toStackedFormat ∷ (Printable α, ToFormat α, Show α) => [FormatSpecifier α] → FormatSpecifier α → Format β
toStackedFormat stack ofs =
  case innerFormatSpecifier ofs of
    𝓙 ifs → toStackedFormat (ofs:stack) ifs
    𝓝     → Format $ [fmt|#{%t:%T}|] (T.intercalate ";" $ toText ⊳ (reverse $ sortOn stackRank stack)) (ofs)

instance (Show α, ToFormat α, Printable α) => ToFormat (FormatSpecifier α) where
  -- each output has a leading character, A…; they are removed only if there is a
  -- passing test for that
  toFormat (BareOption o) = Format $ [fmt|#{%T}|] o
--  toFormat (ExpandTwice w_strftime fs) | stackRank fs > 0 = ifsExpandTwice w_strftime fs
--                                       | otherwise = Format $ [fmt|#{%T:%T}|] w_strftime fs

  toFormat ofs = toStackedFormat [] ofs
  toFormat ofs =
    case innerFormatSpecifier ofs of
      𝓙 ifs → if stackRank ifs ≡ 0 ∨ stackRank ofs ≡ 0
              then toFormat_ ofs -- [fmt|XXX %w // %w|] ofs ifs
              else if (stackRank ifs > stackRank ofs)
                   then Format $ [fmt|ZZZ %T [%T] <%T> (%d) %w|] ifs (toFormat ifs) ( ofs) (stackRank ifs) ofs
                   else Format $ [fmt|WWW (%d) %w|] (stackRank ifs) ofs
      𝓝     → Format $ [fmt|YYY %w|] ofs

  toFormat (MaxLen len_spec (BareOption o)) =
    Format $ [fmt|#{%T:%T}|] len_spec (o)

  toFormat (MaxLen l_spec fs) | stackRank fs > 0 = ifsMaxLen l_spec fs
                              | otherwise = Format $ [fmt|H#{%T:%T}|] l_spec (toFormat fs)

  toFormat (MaxLen (OptLen l) (BareOption o)) =
    -- the / acts as a separator between the '=' and the #{…}.  I can't find
    -- any direct documentation for it in tmux; experimentation shows it to be
    -- required
    Format $ [fmt|I#{=/%T:%T}|] (toFormat l) (toText o)
  toFormat (MaxLen (OptLen l) f) =
    Format $ [fmt|J#{=/%T:%T}|] (toFormat l) (toText f)

bareOption ∷ α → FormatSpecifier α
bareOption = BareOption ∘ Option


-- main ------------------------------------------------------------------------

main :: IO ()
main = do
  say $ toFormat (emptyStyle & align ⊩ AlignLeft
                             & range ⊩ RangeLeft
                             & stylePayload ⊩ ExpandTwice WithoutStrftime (bareOption StatusLeftStyle)
                 )

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  let status_left_style = ExpandTwice WithoutStrftime (bareOption StatusLeftStyle)
      status_left       = bareOption StatusLeft
      ts_ :: [(𝕋,Format SavedDefault)]
      ts_ =
        let left_style_status :: Style
            left_style_status = emptyStyle & align        ⊩ AlignLeft
                                           & range        ⊩ RangeLeft
                                           & stylePayload ⊩ status_left_style
            left_status :: FormatSpecifier FormatOption
            left_status = status_left
        in  [ ( "#[align=left range=left #{E:status-left-style}]"
              , toFormat left_style_status
              )
            , ( "#{window_name}", toFormat WindowName )
            , ( "#{@foobie}", toFormat $ userOption "@foobie" )
            , ( "#{=3:window_name}",
                toFormat $ MaxLen (FixedLen 3) (bareOption WindowName) )
            , ( "#{=/#{status-left-length}:window_name}",
                toFormat $ MaxLen (OptLen StatusLeftLength) (bareOption WindowName) )
            , ( "#{T:@foobie}",
                toFormat $ ExpandTwice WithStrftime
                         $ bareOption $ userOption "@foobie" )
            , ( "#{E;=3:@foobie}", -- "#{=3:#{E:@foobie}}" would also work, less compact
                toFormat $
                  MaxLen (FixedLen 3) (ExpandTwice WithoutStrftime $
                                         bareOption $ userOption "@foobie") )

            {- The ordering of the T and the =1 doesn't matter; the T always effects:
               > $ tmux set-option @foobie %Y-%M-%d
               > $ tmux display-message -p '#{T;=/1:#{@foobie}}'
               > 2
               > $ tmux display-message -p '#{=/1:#{T:@foobie}}'
               > 2
            -}
            , ( "#{T;=3:@foobie}",
                toFormat $ ExpandTwice WithStrftime $ MaxLen (FixedLen 3) $
                                                  bareOption $ userOption "@foobie")

            , ( "#{T;=3:@foobie}",
                toFormat $ MaxLen (FixedLen 3) $ ExpandTwice WithStrftime $
                                                  bareOption $ userOption "@foobie")
            , ( "#{=/#{status-left-length}:window_name}",
                toFormat $ MaxLen (OptLen StatusLeftLength)
                                  (bareOption WindowName) )
            , ( "#{E;=3:window_name}",
                toFormat $ ExpandTwice WithoutStrftime
                         $ MaxLen (FixedLen 3) (bareOption WindowName) )
            , ( ю [ "#[push-default]"
                  , "#{T;=/#{status-left-length}:status-left}"
                  , "#[pop-default]" ]
              , saveDefault $ toFormat left_status
            )
            ]
      do_test :: (𝕋, Format SavedDefault) → TestTree
      do_test (t,x) = testCase (T.unpack t) (t @=? toText x)
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
