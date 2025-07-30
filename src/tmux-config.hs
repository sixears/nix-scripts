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
-- a Format is basically a newtype around Text, but used with
-- SavedDefault or UnsavedDefault
newtype Format α = Format { unFormat ∷ 𝕋 }

instance Printable (Format α) where
  print = P.text ∘ unFormat

class ToFormat α where
  toFormat :: α -> Format UnsavedDefault

instance ToFormat () where
  toFormat () = Format ""

instance ToFormat 𝕋 where
  toFormat = Format

instance ToFormat α => ToFormat [α] where
  toFormat as = Format $ ю [ unFormat $ toFormat a | a ← as ]

-- saveDefault ∷ Format UnsavedDefault → Format SavedDefault
-- saveDefault f = Format $ "#[push-default]" ⊕ toText f ⊕ "#[pop-default]"

saveDefault ∷ ToFormat α => α → Format SavedDefault
saveDefault f = Format $ "#[push-default]" ⊕ toText (toFormat f) ⊕ "#[pop-default]"

noSaveDefault ∷ Format UnsavedDefault → Format SavedDefault
noSaveDefault = Format ∘ unFormat

------------------------------------------------------------

data StyleVariable = StatusLeftStyle
                   | StatusRightStyle
                   | StyleText 𝕋
                   | WindowStatusStyle
                   | WindowStatusActivityStyle
                   | WindowStatusBellStyle
                   | WindowStatusLastStyle
  deriving Show

instance Printable StyleVariable where
  print StatusLeftStyle           = P.text "status-left-style"
  print StatusRightStyle          = P.text "status-right-style"
  print (StyleText t)             = P.text t
  print WindowStatusStyle         = P.text "window-status-style"
  print WindowStatusActivityStyle = P.text "window-status-activity-style"
  print WindowStatusBellStyle     = P.text "window-status-bell-style"
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

data FormatOption = StatusLeft | StatusRight | WindowStatusFormat deriving Show

instance Printable FormatOption where
  print StatusLeft         = P.text "status-left"
  print StatusRight        = P.text "status-right"
  print WindowStatusFormat = P.text "window-status-format"

instance ToFormat FormatOption where
  toFormat o = Format $ [fmt|#{%T}|] o

------------------------------------------------------------

data FormatVariable = WindowName deriving Show

instance ToFormat FormatVariable where
  toFormat WindowName = Format "#{window_name}"

instance Printable FormatVariable where
  print WindowName = P.text "window_name"

------------------------------------------------------------

data BooleanVariable = WindowEndFlag | WindowLastFlag | WindowActivityFlag
                     | WindowBellFlag | WindowSilenceFlag
  deriving Show

instance Printable BooleanVariable where
  print WindowEndFlag      = P.text "window_end_flag"
  print WindowBellFlag     = P.text "window_bell_flag"
  print WindowLastFlag     = P.text "window_last_flag"
  print WindowActivityFlag = P.text "window_activity_flag"
  print WindowSilenceFlag  = P.text "window_silence_flag"

------------------------------------------------------------

data StringVariable = Unused deriving Show

------------------------------------------------------------

data Variable = BoolVar BooleanVariable | StyleVar StyleVariable  deriving Show

instance Printable Variable where
  print (BoolVar  bv) = print bv
  print (StyleVar sv) = print sv

instance ToFormat Variable where
  toFormat (BoolVar bv) = Format $ [fmt|#{%T}|] bv

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

------------------------------------------------------------

data StringOption = WindowStatusSeparator | StringOptionText 𝕋 deriving Show

instance Printable StringOption where
  print (StringOptionText sot) = P.text sot
  print WindowStatusSeparator  = "window-status-separator"

instance ToFormat StringOption where
  toFormat WindowStatusSeparator = Format "window-status-separator"
  toFormat (StringOptionText t)  = Format t

------------------------------------------------------------

{-| Tmux "options" that evaluate to an integer value -}
data IntOption = StatusLeftLength | StatusRightLength | WindowIndex
  deriving Show

instance Printable IntOption where
  print StatusLeftLength  = P.text "status-left-length"
  print StatusRightLength = P.text "status-right-length"
  print WindowIndex       = P.text "window_index"

instance ToFormat IntOption where
  toFormat io = Format $ [fmt|#{%T}|] io

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

data RangeStyle = RangeLeft | RangeRight | RangeNone
                | RangeWindow IntOption
  deriving Show

instance Printable RangeStyle where
  print RangeLeft        = P.text "range=left"
  print RangeRight       = P.text "range=right"
  print RangeNone        = P.text "norange"
  print (RangeWindow io) = P.text $ [fmt|range=window|%T|] (toFormat io)

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

data Style α = Style { _styleDefault ∷ StyleDefault
                     , _alignStyle   ∷ 𝕄 Alignment
                     , _rangeStyle   ∷ 𝕄 RangeStyle
                     , _listStyle    ∷ 𝕄 ListStyle
                     , _stylePayload ∷ 𝕄 α
                     }
  deriving Show

alignStyle :: Lens' (Style α) (𝕄 Alignment)
alignStyle = lens _alignStyle (\ s a -> s { _alignStyle = a })

rangeStyle :: Lens' (Style α) (𝕄 RangeStyle)
rangeStyle = lens _rangeStyle (\ s a -> s { _rangeStyle = a })

styleDefault :: Lens' (Style α) StyleDefault
styleDefault = lens _styleDefault (\ s a -> s { _styleDefault = a })

listStyle :: Lens' (Style α) (𝕄 ListStyle)
listStyle = lens _listStyle (\ s a -> s { _listStyle = a })

stylePayload ∷ Lens' (Style α) (𝕄 α)
stylePayload = lens _stylePayload (\ s a -> s { _stylePayload = a })

emptyStyle :: Style α
emptyStyle = Style NoStyleDefault 𝓝 𝓝 𝓝 𝓝

instance Show α => Printable (Style α) where print s = P.string (show s)

instance ToFormat α => ToFormat (Style α) where
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
                       | BareVariable Variable
                       | ExpandTwice WithStrftime (FormatSpecifier α)
                       | MaxLen LenSpec (FormatSpecifier α)
                       | ForEachWindow α α
                       | Conditional 𝕋 𝕋 𝕋
                       -- XXX replace this with Format?
                       | BareText 𝕋
  deriving Show

----------------------------------------

conditional :: (ToFormat β, ToFormat γ) => BoolExpr → β → γ → FormatSpecifier α
conditional a b c =
  Conditional (toText $ toFormat a) (toText $ toFormat b) (toText $ toFormat c)

----------------------------------------

stackRank ∷ FormatSpecifier α → Word8
stackRank (ExpandTwice _ _) = 2
stackRank (MaxLen      _ _) = 1
stackRank _                 = 0

----------------------------------------

innerFormatSpecifier :: FormatSpecifier α → 𝕄 (FormatSpecifier α)
innerFormatSpecifier (BareOption    _)      = 𝓝
innerFormatSpecifier (BareVariable  _)      = 𝓝
innerFormatSpecifier (MaxLen        _  fs)  = 𝓙 fs
innerFormatSpecifier (ExpandTwice   _  fs)  = 𝓙 fs
innerFormatSpecifier (ForEachWindow _ _)    = 𝓝
innerFormatSpecifier (Conditional   _ _ _)  = 𝓝
innerFormatSpecifier (BareText      _)      = 𝓝

--------------------

instance (Show α, ToFormat α, Printable α) => Printable (FormatSpecifier α) where
  print (BareOption   t)           = print t
  print (BareVariable t)           = print t
  print (ExpandTwice w_strftime _) = P.text $ [fmt|%T|] w_strftime
  print (MaxLen      len_spec   _) = P.text $ [fmt|%T|] len_spec
  print (ForEachWindow other current) =
    P.text $ [fmt|W:%T,%T|] (toFormat other) (toFormat current)
  print (Conditional condition ifthen ifelse) =
    P.text $ [fmt|?%T,%T,%T|]
                 condition (toFormat ifthen) (toFormat ifelse)
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
      status_left_style  ∷ FormatSpecifier StyleVariable
      status_left_style  = _E $ bareOption StatusLeftStyle
      status_right_style ∷ FormatSpecifier StyleVariable
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
        let left_style_status :: Style (FormatSpecifier StyleVariable)
            left_style_status = emptyStyle & alignStyle   ⊩ AlignLeft
                                           & rangeStyle   ⊩ RangeLeft
                                           & stylePayload ⊩ status_left_style
            toF ∷ ToFormat α => α -> Format SavedDefault
            toF = noSaveDefault ∘ toFormat
            toT ∷ ToFormat α => α -> 𝕋
            toT = toText ∘ toFormat
            toT_ = toT @(FormatSpecifier 𝕋)
            toF_SV ∷ FormatSpecifier StyleVariable → 𝕋
            toF_SV = toText ∘ toFormat @(FormatSpecifier StyleVariable)

        in  [ ( "#{window_name}", toF WindowName )
            , ( "#{@foobie}", toF user_foobie )
            , ( "#{=3:window_name}", toF $ len3 bare_wname )
            , ( "#{=/#{status-left-length}:window_name}",
                toF $ len_left_length bare_wname )
            , ( "#{T:@foobie}", toF $ ExpandTwice WithStrftime bare_foobie )
            , ( -- "#{=3:#{E:@foobie}}" would also work, but is less compact
                "#{E;=3:@foobie}", toF $ len3 (_E bare_foobie) )

            {- The ordering of the T and the =1 doesn't matter; the T always
               effects:
               > $ tmux set-option @foobie %Y-%M-%d
               > $ tmux display-message -p '#{T;=/1:#{@foobie}}'
               > 2
               > $ tmux display-message -p '#{=/1:#{T:@foobie}}'
               > 2
            -}
            , ( "#{T;=3:@foobie}", toF $ _T $ len3 bare_foobie)

            , ( "#{T;=3:@foobie}", toF $ len3 $ _T bare_foobie)
            , ( "#{=/#{status-left-length}:window_name}",
                toF $ len_left_length bare_wname )
            , ( "#{E;=3:window_name}", toF $ _E $ len3 bare_wname )
            , ( "#[align=left range=left #{E:status-left-style}]"
              , toF left_style_status
              )
            , ( ю [ "#[push-default]"
                  , "#{T;=/#{status-left-length}:status-left}"
                  , "#[pop-default]" ]
              , saveDefault (_T $ len_left_length status_left)
              )
            , ( ю [ "#[norange default]"
                  , "#[nolist align=right range=right #{E:status-right-style}]"
                  ]
              , toF [ emptyStyle & rangeStyle   ⊩ RangeNone
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
              , saveDefault (_T $ len_right_length status_right)
              )

            , ( "#[list=on align=#{status-justify}]"
              , toF (emptyStyle @() & listStyle ⊩ ListOn
                                  & alignStyle ⊩ AlignOpt StatusJustify)
              )

            , ( "#[list=left-marker]<"
              , toF $ emptyStyle @() & listStyle ⊩ ListLeftMarker "<"
              )

            , ( "#[list=right-marker]>"
              , toF $ emptyStyle @() & listStyle ⊩ ListRightMarker ">"
              )

            , ( "#[list=on]", toF (emptyStyle @() & listStyle ⊩ ListOn) )

            , ( "#{W:#{status-left},#{status-right}}",
                toF (ForEachWindow status_left status_right)
              )
            , ( "#{W:#[list=on],#[list=focus]}",
                toF (ForEachWindow (emptyStyle @() & listStyle ⊩ ListOn)
                                   (emptyStyle @() & listStyle ⊩ ListFocus))
              )
            , ("#{?window_end_flag,,#{window-status-separator}}"
              , toF @(FormatSpecifier 𝕋)
                  (conditional @()
                   (BVar WindowEndFlag) () (bareOption WindowStatusSeparator))
              )
            , ( "#[push-default]#{T:window-status-format}#[pop-default]"
              , saveDefault (_T (bareOption WindowStatusFormat))
              )
            , ( "#[range=window|#{window_index} foo]"
              , toF (emptyStyle & rangeStyle ⊩ RangeWindow WindowIndex
                                & stylePayload ⊩ (StyleText "foo"))
              )

            , ( T.intercalate "," [ "#{&&:#{window_last_flag}"
                                  , "#{!=:#{E:window-status-last-style}"
                                  , "default}}"
                                  ]
              , let win_stat_last =
                      BareVariable $ StyleVar WindowStatusLastStyle
                in  toF (And (BVar WindowLastFlag)
                             (StrNotEq (StrTxt ∘ toF_SV $ _E win_stat_last)
                                       (StyExp DefaultStyle)))
              )
            , ( T.intercalate "," [ "#{?#{&&:#{window_last_flag}"
                                  , "#{!=:#{E:window-status-last-style}"
                                  , "default}}"
                                  , "#{E:window-status-last-style}"
                                  , "}"
                                  ]
              , let win_stat_last ∷ FormatSpecifier StyleVariable
                    win_stat_last =
                      BareVariable $ StyleVar WindowStatusLastStyle
                    win_last_style =
                      And (BVar WindowLastFlag)
                          (StrNotEq (StrTxt ∘ toF_SV $ _E win_stat_last)
                                    (StyExp DefaultStyle))
                in  toF @(FormatSpecifier 𝕋) $
                      conditional (win_last_style∷BoolExpr)
                                  (_E win_stat_last) ()
              )
            , ( "#{||:#{window_activity_flag},#{window_silence_flag}}"
              , toF $ Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag)
              )

            , ( T.intercalate ","
                [ "#{&&:#{||:#{window_activity_flag},#{window_silence_flag}}"
                , "#{!=:#{E:window-status-activity-style}"
                , "default}}" ]
              , toF $
                  And (Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag))
                      (StrNotEq (StrTxt $ toText ∘ toFormat @(FormatSpecifier StyleVariable) $ _E $ BareVariable $ StyleVar WindowStatusActivityStyle)
                                (StyExp DefaultStyle))
              )

            , let ç    = T.intercalate ","
                  toT  ∷ ToFormat α => α -> 𝕋
                  toT  = toText ∘ toFormat
                  toT_ = toT @(FormatSpecifier 𝕋)
              in  (ç [ ç [ "#{?#{&&:#{||:#{window_activity_flag}"
                          , "#{window_silence_flag}}"
                          , ç ["#{!=:#{E:window-status-activity-style}"
                              , "default}}" ]
                          ]
                      , "#{E:window-status-activity-style}"
                      , "}"
                      ]
              , Format $ toT_ $
                  conditional @(FormatSpecifier 𝕋)
                    (And (Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag))
                         (StrNotEq (StrTxt $ toText ∘ toFormat @(FormatSpecifier StyleVariable) $ _E $ BareVariable $ StyleVar WindowStatusActivityStyle)
                                   (StyExp DefaultStyle)))
                                   (_E $ BareVariable $ StyleVar WindowStatusActivityStyle)()
              )

            , ( "#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}"
              , let xx_ ∷ FormatSpecifier 𝕋 =
                      (conditional @(FormatSpecifier 𝕋)
                         (And (Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag))
                              (StrNotEq
                                 (StrTxt $
                                    toText ∘ toFormat @(FormatSpecifier StyleVariable) $
                                      _E $ BareVariable $ StyleVar WindowStatusActivityStyle)
                                 (StyExp DefaultStyle))
                         )
                         (_E $ BareVariable $
                            StyleVar WindowStatusActivityStyle)
                         ()
                      )
                in -- "#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}}"
                  toF @(FormatSpecifier 𝕋) $ conditional @(FormatSpecifier 𝕋)
                   (let win_stat_bell =
                         BareVariable $ StyleVar WindowStatusBellStyle
                    in  {- toF -} (And (BVar WindowBellFlag)
                                (StrNotEq (StrTxt ∘ toF_SV $ _E win_stat_bell)
                                          (StyExp DefaultStyle))))
                    -- "#{E:window-status-bell-style}"
                    (_E $ BareVariable $ StyleVar WindowStatusBellStyle)
                    -- "#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}}, #{E:window-status-activity-style},}}"
                    xx_

              )

            , ( ю [ "#[list=on align=#{status-justify}]#[list=left-marker]<#[list=right-marker]>#[list=on]#{W:#[range=window|#{window_index} #{E:window-status-style}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]#[push-default]#{T:window-status-format}#[pop-default]#[norange default]#{?window_end_flag,,#{window-status-separator}},#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]#[push-default]#{T:window-status-current-format}#[pop-default]#[norange list=on default]#{?window_end_flag,,#{window-status-separator}}}"
                  ]
              , let bareT = toT ∘ BareText @𝕋
                    win_stat_last ∷ FormatSpecifier StyleVariable
                    win_stat_last =
                      BareVariable $ StyleVar WindowStatusLastStyle
                    win_last_style =
                      And (BVar WindowLastFlag)
                          (StrNotEq (StrTxt ∘ toF_SV $ _E win_stat_last)
                                    (StyExp DefaultStyle))
                    text_to_style =
                      ю [ toT (ExpandTwice WithoutStrftime
                                           (bareOption WindowStatusStyle))
                        , toText $
                            toF @(FormatSpecifier 𝕋) $
                              conditional (win_last_style∷BoolExpr)
                                          (_E win_stat_last) ()
                        , "#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}"
                        ]

                in  toF [ toText ∘ toFormat $
                             emptyStyle @() & listStyle ⊩ ListOn
                                        & alignStyle ⊩ AlignOpt StatusJustify
                         , toText ∘ toFormat $
                             emptyStyle @() & listStyle ⊩ ListLeftMarker "<"
                         , toText ∘ toFormat $
                             emptyStyle @() & listStyle ⊩ ListRightMarker ">"
                         , toText ∘ toFormat $
                             emptyStyle @() & listStyle ⊩ ListOn

                         , toText ∘ toFormat $
                             ForEachWindow @(FormatSpecifier 𝕋)
                               (BareText $ ю
                                [ toText ∘ toFormat $ emptyStyle & rangeStyle ⊩ RangeWindow WindowIndex & stylePayload ⊩ StyleText(text_to_style)
                                , toText (saveDefault (_T $ bareOption WindowStatusFormat))
                                , toT (emptyStyle @() & rangeStyle   ⊩ RangeNone
                                                      & styleDefault ⊢ StyleDefault)
                                , toT_ (conditional (BVar WindowEndFlag)
                                                   (StringOptionText "")
                                                   (bareOption WindowStatusSeparator))
                                 ]
                               )
                               (BareText "#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]#[push-default]#{T:window-status-current-format}#[pop-default]#[norange list=on default]#{?window_end_flag,,#{window-status-separator}}")
                         ]
              )
            ]
      do_test :: (𝕋, Format α) → TestTree
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
