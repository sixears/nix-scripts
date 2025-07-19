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
import Data.Maybe  ( catMaybes )

-- monadio-plus ------------------------

import MonadIO  ( say )

-- more-unicode ------------------------

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
  print (BareOption  t)            = print t
  print (ExpandTwice w_strftime _) = P.text $ [fmt|%T|] w_strftime
  print (MaxLen      len_spec   _) = P.text $ [fmt|%T|] len_spec

--------------------

toStackedFormat ∷ (Printable α, ToFormat α, Show α) =>
                  [FormatSpecifier α] → FormatSpecifier α → Format β
toStackedFormat stack ofs =
  case innerFormatSpecifier ofs of
    𝓙 ifs → toStackedFormat (ofs:stack) ifs
    𝓝     → let stck = toText ⊳ reverse (sortOn stackRank stack)
             in  Format $ [fmt|#{%t:%T}|] (T.intercalate ";" $ stck) ofs

instance (Show α, ToFormat α, Printable α) => ToFormat (FormatSpecifier α) where
  -- each output has a leading character, A…; they are removed only if there is a
  -- passing test for that
  toFormat (BareOption o) = Format $ [fmt|#{%T}|] o
--  toFormat (ExpandTwice w_strftime fs) | stackRank fs > 0 = ifsExpandTwice w_strftime fs
--                                       | otherwise = Format $ [fmt|#{%T:%T}|] w_strftime fs

  toFormat ofs = toStackedFormat [] ofs

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
  let _E                ∷ FormatSpecifier α → FormatSpecifier α
      _E                = ExpandTwice WithoutStrftime
      _T                ∷ FormatSpecifier α → FormatSpecifier α
      _T                = ExpandTwice WithStrftime
      len3              ∷ FormatSpecifier α → FormatSpecifier α
      len3              = MaxLen $ FixedLen 3
      len_left_length   ∷ FormatSpecifier α → FormatSpecifier α
      len_left_length   = MaxLen $ OptLen StatusLeftLength
      status_left_style ∷ FormatSpecifier StyleOption
      status_left_style = _E $ bareOption StatusLeftStyle
      status_left       ∷ FormatSpecifier FormatOption
      status_left       = bareOption StatusLeft
      user_foobie       ∷ UserOption
      user_foobie       = userOption "@foobie"
      bare_foobie       ∷ FormatSpecifier UserOption
      bare_foobie       = bareOption user_foobie
      bare_wname        ∷ FormatSpecifier FormatVariable
      bare_wname        = bareOption WindowName
      ts_ :: [(𝕋,Format SavedDefault)]
      ts_ =
        let left_style_status :: Style
            left_style_status = emptyStyle & align        ⊩ AlignLeft
                                           & range        ⊩ RangeLeft
                                           & stylePayload ⊩ status_left_style
        in  [ ( "#{window_name}", toFormat WindowName )
            , ( "#{@foobie}", toFormat $ user_foobie )
            , ( "#{=3:window_name}", toFormat $ len3 bare_wname )
            , ( "#{=/#{status-left-length}:window_name}",
                toFormat $ len_left_length bare_wname )
            , ( "#{T:@foobie}",
                toFormat $ ExpandTwice WithStrftime $ bare_foobie )
            , ( "#{E;=3:@foobie}",
                -- "#{=3:#{E:@foobie}}" would also work, but is less compact
                toFormat $ len3 (_E bare_foobie) )

            {- The ordering of the T and the =1 doesn't matter; the T always
               effects:
               > $ tmux set-option @foobie %Y-%M-%d
               > $ tmux display-message -p '#{T;=/1:#{@foobie}}'
               > 2
               > $ tmux display-message -p '#{=/1:#{T:@foobie}}'
               > 2
            -}
            , ( "#{T;=3:@foobie}", toFormat $ _T $ len3 bare_foobie)

            , ( "#{T;=3:@foobie}", toFormat $ len3 $ _T bare_foobie)
            , ( "#{=/#{status-left-length}:window_name}",
                toFormat $ len_left_length bare_wname )
            , ( "#{E;=3:window_name}", toFormat $ _E $ len3 bare_wname )
            , ( "#[align=left range=left #{E:status-left-style}]"
              , toFormat left_style_status
              )
            , ( ю [ "#[push-default]"
                  , "#{T;=/#{status-left-length}:status-left}"
                  , "#[pop-default]" ]
              , saveDefault $ toFormat (_T $ len_left_length status_left)
              )

            , ( ю [ "#[norange default]"
                  , "#[nolist align=right range=right #{E:status-right-style}]"
                  ]
              , saveDefault $ toFormat (_T $ len_left_length status_left)
              )
            , ( ю [ "#[push-default]"
                  , "#{T;=|#{status-right-length}:status-right}"
                  , "#[pop-default]"
                  ]
              , saveDefault $ toFormat (_T $ len_left_length status_left)
              )
            , ( ю [ "#[list=on align=#{status-justify}]#[list=left-marker]<#[list=right-marker]>#[list=on]#{W:#[range=window|#{window_index} #{E:window-status-style}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}}, #{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}, #{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}]#[push-default]#{T:window-status-format}#[pop-default]#[norange default]#{?window_end_flag,,#{window-status-separator}},#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}}, #{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}, #{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}]#[push-default]#{T:window-status-current-format}#[pop-default]#[norange list=on default]#{?window_end_flag,,#{window-status-separator}}}"
                  ]
              , saveDefault $ toFormat (_T $ len_left_length status_left)
              )
            ]
      do_test :: (𝕋, Format SavedDefault) → TestTree
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
