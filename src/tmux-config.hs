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
import Data.MoreUnicode.Maybe  ( pattern 𝓙, pattern 𝓝, (⧐) )

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

instance ToFormat 𝕋 where
  toFormat = Format

instance ToFormat α => ToFormat [α] where
  toFormat as = Format $ ю [ unFormat $ toFormat a | a ← as ]

saveDefault ∷ Format UnsavedDefault → Format SavedDefault
saveDefault f = Format $ "#[push-default]" ⊕ toText f ⊕ "#[pop-default]"

------------------------------------------------------------

data StyleOption = StatusLeftStyle | StatusRightStyle deriving Show

instance Printable StyleOption where
  print StatusLeftStyle  = P.text "status-left-style"
  print StatusRightStyle = P.text "status-right-style"

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

data FormatOption = StatusLeft | StatusRight deriving Show

instance Printable FormatOption where
  print StatusLeft = P.text "status-left"
  print StatusRight = P.text "status-right"

instance ToFormat FormatOption where
  toFormat o = Format $ [fmt|#{%T}|] o

------------------------------------------------------------

data FormatVariable = WindowName deriving Show

instance ToFormat FormatVariable where
  toFormat WindowName = Format "#{window_name}"

instance Printable FormatVariable where
  print WindowName = P.text "window_name"

------------------------------------------------------------

data BoolOption = WindowEndFlag deriving Show

instance Printable BoolOption where
  print WindowEndFlag = P.string $ show WindowEndFlag

instance ToFormat BoolOption where
  toFormat WindowEndFlag = Format "window_end_flag"

------------------------------------------------------------

data StringOption = WindowStatusSeparator | StringOptionText 𝕋 deriving Show

instance Printable StringOption where
  print (StringOptionText sot) = P.text sot
  print WindowStatusSeparator  = "window-status-separator"
  print so = P.string $ show so

instance ToFormat StringOption where
  toFormat WindowStatusSeparator = Format "window-status-separator"
  toFormat (StringOptionText t)  = Format t

------------------------------------------------------------

{-| Tmux "options" that evaluate to an integer value -}
data IntOption = StatusLeftLength | StatusRightLength deriving Show

instance Printable IntOption where
  print StatusLeftLength  = P.text "status-left-length"
  print StatusRightLength = P.text "status-right-length"

-- instance ToFormat IntOption where
--   toFormat io = Format $ [fmt|#{%T}|] io

------------------------------------------------------------

newtype Option α = Option α
  deriving (Printable, Show, ToFormat)

------------------------------------------------------------

data AlignOption = StatusJustify deriving Show

instance Printable AlignOption where
  print StatusJustify = P.text "status-justify"

-- instance ToFormat AlignOption where
--   toFormat ao = Format $ [fmt|#{%T}|] ao

------------------------------------------------------------

data Alignment = AlignLeft | AlignRight | AlignCentre | AlignOpt AlignOption
  deriving Show

instance Printable Alignment where
  print AlignLeft     = P.text "align=left"
  print AlignCentre   = P.text "align=centre"
  print AlignRight    = P.text "align=right"
  print (AlignOpt ao) = P.text $ [fmt|align=#{%T}|] ao

------------------------------------------------------------

data RangeStyle = RangeLeft | RangeRight | RangeNone deriving Show

instance Printable RangeStyle where
  print RangeLeft  = P.text "range=left"
  print RangeRight = P.text "range=right"
  print RangeNone  = P.text "norange"

------------------------------------------------------------

data ListStyle = ListOn | ListFocus | ListLeftMarker 𝕋 | ListRightMarker 𝕋
               | ListNone
  deriving Show

instance Printable ListStyle where
  print ListOn              = P.text "list=on"
  print ListFocus           = P.text "list=focus"
  print (ListLeftMarker _)  = P.text "list=left-marker"
  print (ListRightMarker _) = P.text "list=right-marker"
  print ListNone            = P.text "nolist"

listPayload ∷ ListStyle → 𝕄 𝕋
listPayload ListOn              = 𝓝
listPayload ListFocus           = 𝓝
listPayload (ListLeftMarker  t) = 𝓙 t
listPayload (ListRightMarker t) = 𝓙 t
listPayload ListNone            = 𝓝

------------------------------------------------------------

data StyleDefault = StyleDefault | NoStyleDefault deriving Show

data Style = Style { _styleDefault ∷ StyleDefault
                   , _alignStyle   ∷ 𝕄 Alignment
                   , _rangeStyle   ∷ 𝕄 RangeStyle
                   , _listStyle    ∷ 𝕄 ListStyle
                   , _stylePayload ∷ 𝕄 (FormatSpecifier StyleOption)
                   }
  deriving Show

alignStyle :: Lens' Style (𝕄 Alignment)
alignStyle = lens _alignStyle (\ s a -> s { _alignStyle = a })

rangeStyle :: Lens' Style (𝕄 RangeStyle)
rangeStyle = lens _rangeStyle (\ s a -> s { _rangeStyle = a })

styleDefault :: Lens' Style StyleDefault
styleDefault = lens _styleDefault (\ s a -> s { _styleDefault = a })

listStyle :: Lens' Style (𝕄 ListStyle)
listStyle = lens _listStyle (\ s a -> s { _listStyle = a })

stylePayload ∷ Lens' Style (𝕄 (FormatSpecifier StyleOption))
stylePayload = lens _stylePayload (\ s a -> s { _stylePayload = a })

emptyStyle :: Style
emptyStyle = Style NoStyleDefault 𝓝 𝓝 𝓝 𝓝

instance Printable Style where print s = P.string (show s)

instance ToFormat Style where
  toFormat s =
    let pieces = [ [fmt|%T|] ⊳ (s ⊣ listStyle)
                 , [fmt|%T|] ⊳ (s ⊣ alignStyle)
                 , [fmt|%T|] ⊳ (s ⊣ rangeStyle)
                 , toText ∘ toFormat ⊳ (s ⊣ stylePayload)
                 , case s ⊣ styleDefault of
                     StyleDefault   → 𝓙 "default"
                     NoStyleDefault → 𝓝
                 ]
        payload = "" ⧐ (s ⊣ listStyle ≫ listPayload)
    in  Format $ [fmt|#[%t]%t|] (T.intercalate " " $ catMaybes pieces) payload

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
                       | ForEachWindow α α
                       | Conditional 𝕋 𝕋 𝕋
                       | BareText 𝕋
  deriving Show

----------------------------------------

conditional :: (ToFormat β, ToFormat γ) => BoolOption → β → γ → FormatSpecifier α
conditional a b c = Conditional (toText $ toFormat a) (toText $ toFormat b) (toText $ toFormat c)

----------------------------------------

stackRank ∷ FormatSpecifier α → Word8
stackRank (ExpandTwice _ _) = 2
stackRank (MaxLen      _ _) = 1
stackRank _                 = 0

----------------------------------------

innerFormatSpecifier :: FormatSpecifier α → 𝕄 (FormatSpecifier α)
innerFormatSpecifier (BareOption    _)      = 𝓝
innerFormatSpecifier (MaxLen        _  fs)  = 𝓙 fs
innerFormatSpecifier (ExpandTwice   _  fs)  = 𝓙 fs
innerFormatSpecifier (ForEachWindow _ _)    = 𝓝
innerFormatSpecifier (Conditional   _ _ _)  = 𝓝
innerFormatSpecifier (BareText      _)      = 𝓝

--------------------

instance (Show α, ToFormat α, Printable α) => Printable (FormatSpecifier α) where
  print (BareOption  t)            = print t
  print (ExpandTwice w_strftime _) = P.text $ [fmt|%T|] w_strftime
  print (MaxLen      len_spec   _) = P.text $ [fmt|%T|] len_spec
  print (ForEachWindow other current) =
    P.text $ [fmt|W:%T,%T|] (toFormat other) (toFormat current)
  print (Conditional condition ifthen ifelse) =
    P.text $ [fmt|?%T,%T,%T|]
                 (toFormat condition) (toFormat ifthen) (toFormat ifelse)
  print (BareText  t)              = print $ "ZZZ" ◇ t

--------------------

toStackedFormat ∷ (Printable α, ToFormat α, Show α) =>
                  [FormatSpecifier α] → FormatSpecifier α → Format β
toStackedFormat stack ofs =
  case innerFormatSpecifier ofs of
    𝓙 (  ifs) → toStackedFormat (ofs:stack) ifs
    _          → case toText ⊳ reverse (sortOn stackRank stack) of
                   []   → Format $ [fmt|#{%T}|] ofs
                   stck → Format $ [fmt|#{%t:%T}|] (T.intercalate ";" stck) ofs

instance (Show α, ToFormat α, Printable α) => ToFormat (FormatSpecifier α) where
  toFormat (BareOption o) = Format $ [fmt|#{%T}|] o
  toFormat (BareText   t) = Format t
  toFormat ofs            = toStackedFormat [] ofs

bareOption ∷ α → FormatSpecifier α
bareOption = BareOption ∘ Option


-- main ------------------------------------------------------------------------

main :: IO ()
main = do
  say $ toFormat (emptyStyle & alignStyle   ⊩ AlignLeft
                             & rangeStyle   ⊩ RangeLeft
                             & stylePayload ⊩ ExpandTwice WithoutStrftime (bareOption StatusLeftStyle)
                 )

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = localOption Never $
  let _E                 ∷ FormatSpecifier α → FormatSpecifier α
      _E                 = ExpandTwice WithoutStrftime
      _T                 ∷ FormatSpecifier α → FormatSpecifier α
      _T                 = ExpandTwice WithStrftime
      len3               ∷ FormatSpecifier α → FormatSpecifier α
      len3               = MaxLen $ FixedLen 3
      len_left_length    ∷ FormatSpecifier α → FormatSpecifier α
      len_left_length    = MaxLen $ OptLen StatusLeftLength
      len_right_length   ∷ FormatSpecifier α → FormatSpecifier α
      len_right_length   = MaxLen $ OptLen StatusRightLength
      status_left_style  ∷ FormatSpecifier StyleOption
      status_left_style  = _E $ bareOption StatusLeftStyle
      status_right_style ∷ FormatSpecifier StyleOption
      status_right_style = _E $ bareOption StatusRightStyle
      status_left        ∷ FormatSpecifier FormatOption
      status_left        = bareOption StatusLeft
      status_right       ∷ FormatSpecifier FormatOption
      status_right       = bareOption StatusRight
      user_foobie        ∷ UserOption
      user_foobie        = userOption "@foobie"
      bare_foobie        ∷ FormatSpecifier UserOption
      bare_foobie        = bareOption user_foobie
      bare_wname         ∷ FormatSpecifier FormatVariable
      bare_wname         = bareOption WindowName
      ts_ :: [(𝕋,Format SavedDefault)]
      ts_ =
        let left_style_status :: Style
            left_style_status = emptyStyle & alignStyle   ⊩ AlignLeft
                                           & rangeStyle   ⊩ RangeLeft
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
              , toFormat [ emptyStyle & rangeStyle   ⊩ RangeNone
                                      & styleDefault ⊢ StyleDefault
                         , emptyStyle & listStyle    ⊩ ListNone
                                      & alignStyle   ⊩ AlignRight
                                      & rangeStyle   ⊩ RangeRight
                                      & stylePayload ⊩ status_right_style
                         ]
              )

            , ( ю [ "#[push-default]"
                  , "#{T;=/#{status-right-length}:status-right}"
                  , "#[pop-default]"
                  ]
              , saveDefault $ toFormat (_T $ len_right_length status_right)
              )

            , ( "#[list=on align=#{status-justify}]"
              , toFormat (emptyStyle & listStyle ⊩ ListOn
                                     & alignStyle ⊩ AlignOpt StatusJustify)
              )

            , ( "#[list=left-marker]<"
              , toFormat $ emptyStyle & listStyle ⊩ ListLeftMarker "<"
              )

            , ( "#[list=right-marker]>"
              , toFormat $ emptyStyle & listStyle ⊩ ListRightMarker ">"
              )

            , ( "#[list=on]", toFormat (emptyStyle & listStyle ⊩ ListOn) )

            , ( "#{W:#{status-left},#{status-right}}",
                toFormat (ForEachWindow status_left status_right)
              )

            , ( "#{W:#[list=on],#[list=focus]}",
                toFormat (ForEachWindow (emptyStyle & listStyle ⊩ ListOn) (emptyStyle & listStyle ⊩ ListFocus))
              )

            , ("#{?window_end_flag,,#{window-status-separator}}"
              , toFormat @(FormatSpecifier 𝕋)
                  (conditional WindowEndFlag (StringOptionText "")
                                             (bareOption WindowStatusSeparator))
              )

            , ( ю [ "#[list=on align=#{status-justify}]#[list=left-marker]<#[list=right-marker]>#[list=on]#{W:#[range=window|#{window_index} #{E:window-status-style}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}}, #{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}, #{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}]#[push-default]#{T:window-status-format}#[pop-default]#[norange default]#{?window_end_flag,,#{window-status-separator}},#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}}, #{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}, #{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}]#[push-default]#{T:window-status-current-format}#[pop-default]#[norange list=on default]#{?window_end_flag,,#{window-status-separator}}}"
                  ]
              , toFormat [ toText ∘ toFormat $
                             emptyStyle & listStyle ⊩ ListOn
                                        & alignStyle ⊩ AlignOpt StatusJustify
                         , toText ∘ toFormat $
                             emptyStyle & listStyle ⊩ ListLeftMarker "<"
                         , toText ∘ toFormat $
                             emptyStyle & listStyle ⊩ ListRightMarker ">"
                         , "#[list=on]"
                         , toText ∘ toFormat $
                             ForEachWindow @(FormatSpecifier 𝕋)
                               (BareText $
                                 (toText ∘ toFormat $ BareText @𝕋 "#[range=window|#{window_index} #{E:window-status-style}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}}, #{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}, #{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}]")
                                ◇ "#[push-default]#{T:window-status-format}#[pop-default]#[norange default]#{?window_end_flag,,#{window-status-separator}}")
                               (BareText "#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}}, #{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}, #{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}]#[push-default]#{T:window-status-current-format}#[pop-default]#[norange list=on default]#{?window_end_flag,,#{window-status-separator}}")
                         ]
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
