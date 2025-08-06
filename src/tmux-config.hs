{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

{- ## conditional -> conditional2

   ## look again at conditional.  Shouldn't it be something like
   BoolExpr -> β -> β -> FormatSpecifier β?  What about empty things,
   currently (); maybe our types can have a Empty constraint that infers a
   empty ∷ α method?
-}

{- ## can we construct some class for "convert to TMuxFormat", such that all
      the conversions (constructors) are inferred from the type?

   use tmf everywhere 'stead of TMF*
-}

{- ## remove BareText ? -}

{- ## replace ForEachWindow with a version that uses TMuxFormat (x2) to avoid
      having BareText instance of FormatSpecifier.  Possibly requires, or
      would benefit from, removing the Show instance of FormatSpecifier.
-}

{- ## Do we still need StringVariableText? -}

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

data SavedDefault deriving Show -- = SavedDefault
data UnsavedDefault deriving Show -- = UnsavedDefault
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
                    | WindowStatusFormat | WindowName
  deriving Show

instance Printable FormatVariable where
  print StatusLeft                = P.text "status-left"
  print StatusRight               = P.text "status-right"
  print WindowStatusFormat        = P.text "window-status-format"
  print WindowStatusCurrentFormat = P.text "window-status-current-format"
  print WindowName                = P.text "window_name"

instance ToFormat FormatVariable where
  toFormat o = Format $ [fmt|#{%T}|] o

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

instance ToFormat BooleanVariable where
  toFormat bv = Format $ [fmt|#{%T}|] bv

------------------------------------------------------------

data Variable = BoolVar BooleanVariable
              | FormatVar FormatVariable
              | StyleVar StyleVariable
              | UserVar UserVariable
              | StringVar StringVariable
  deriving Show

instance Printable Variable where
  print (BoolVar   bv) = print bv
  print (FormatVar fv) = print fv
  print (StringVar  sv) = print sv
  print (StyleVar  sv) = print sv
  print (UserVar  sv) = print sv

instance ToFormat Variable where
  toFormat (BoolVar   bv) = Format $ [fmt|#{%T}|] bv
  -- toFormat (FormatVar fv) = Format $ [fmt|#{%T}|] fv
  toFormat (FormatVar fv) = Format $ [fmt|toFormat FormatVar %w|] fv
  toFormat (StringVar sv) = Format $ [fmt|toFormat StringVar %w|] sv
  toFormat (StyleVar  sv) = Format $ [fmt|toFormat StyleVar %w|] sv
  -- toFormat (UserVar   uv) = Format $ [fmt|#{%T}|] uv
  toFormat (UserVar   uv) = Format $ [fmt|toFormat UserVar %w|] uv

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

data StringVariable = WindowStatusSeparator | StringVariableText 𝕋 deriving Show

instance Printable StringVariable where
  print (StringVariableText sot) = P.text sot
  print WindowStatusSeparator  = "window-status-separator"

instance ToFormat StringVariable where
  toFormat (StringVariableText t)  = Format t
  toFormat v = Format $ [fmt|#{%T}|] v

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

emptyStyle ∷ Style α
emptyStyle = Style NoStyleDefault 𝓝 𝓝 𝓝 𝓝

emptyStyle_ ∷ Style () = emptyStyle


instance Show α => Printable (Style α) where print s = P.string (show s)

instance ToFormat α => ToFormat (Style α) where
  toFormat s =
    let pieces = [ [fmt|%T|] ⊳ (s ⊣ rangeStyle)
                 , [fmt|%T|] ⊳ (s ⊣ listStyle)
                 , [fmt|%T|] ⊳ (s ⊣ alignStyle)
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

class IsVariable α

instance IsVariable BooleanVariable
instance IsVariable FormatVariable
instance IsVariable StringVariable
instance IsVariable StyleVariable
instance IsVariable UserVariable

{- A format specifier is a #{…} group -}
data FormatSpecifier α = IsVariable α => BareVariable α
                       | ExpandTwice WithStrftime (FormatSpecifier α)
                       | MaxLen LenSpec (FormatSpecifier α)
                       | ForEachWindow α α
                       | Conditional 𝕋 𝕋 𝕋
                       -- XXX replace this with Format?
                       | BareText 𝕋

instance Show α => Show (FormatSpecifier α) where
  show (BareVariable v)     = [fmt|IsVariable %w|] v
  show (ExpandTwice wsf v) = [fmt|ExpandTwice %w %w|] wsf v
  show (MaxLen ls v)       = [fmt|MaxLen %w %w|] ls v
  show (ForEachWindow v w) = [fmt|ForEachWindow %w %w|] v w
  show (Conditional a b c) = [fmt|Conditional %w %w %w|] a b c
  show (BareText v)        = [fmt|BareText %w|] v

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
-- innerFormatSpecifier (BareOption    _)      = 𝓝
innerFormatSpecifier (BareVariable   _)      = 𝓝
-- innerFormatSpecifier (BareVariable  _)      = 𝓝
innerFormatSpecifier (MaxLen        _  fs)  = 𝓙 fs
innerFormatSpecifier (ExpandTwice   _  fs)  = 𝓙 fs
innerFormatSpecifier (ForEachWindow _ _)    = 𝓝
innerFormatSpecifier (Conditional   _ _ _)  = 𝓝
innerFormatSpecifier (BareText      _)      = 𝓝

--------------------

instance (Show α, ToFormat α, Printable α) => Printable (FormatSpecifier α) where
  print (BareVariable  t)           = print t
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
--  toFormat (BareOption o) = Format $ [fmt|#{%T}|] o
  toFormat (BareText   t) = Format t
  toFormat ofs            = toStackedFormat [] ofs

bareOption ∷ IsVariable α => α → FormatSpecifier α
-- bareOption = BareOption ∘ Option
bareOption = BareVariable


------------------------------------------------------------

class Empty α where
  empty ∷ α

instance Empty (TMuxFormatTyped StringVariable) where
  empty = TMFV (StringVariableText "")

------------------------------------------------------------

data TMuxFormatTyped α = (Show α, ToFormat α, IsVariable α) => TMFV α
                       | (Show α, ToFormat α) => TMFY (Style α)
                       | (Show α, ToFormat α, Printable α) =>
                           TMFS (FormatSpecifier α)
                       | Show α => TMFF (Format α)
                       | -- conditional
                         TMFC BoolExpr (𝕄 (TMuxFormatTyped α))
                                       (𝕄 (TMuxFormatTyped α))

instance Show (TMuxFormatTyped α) where
  show (TMFV v) = [fmt|TMFV: %w|] v
  show (TMFY y) = [fmt|TMFY: %w|] y
  show (TMFS s) = [fmt|TMFS: %w|] s
  show (TMFF (Format f)) = [fmt|TMFF: %w|] f

instance ToFormat (TMuxFormatTyped α) where
  toFormat t = Format $ toText t

instance Printable (TMuxFormatTyped α) where
  print (TMFV v) = P.text ∘ unFormat $ toFormat v
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
                | TMFL [TMuxFormat]

--------------------

instance Show TMuxFormat where
  show (TMFT t) = "TMuxFormat: " ◇ show t

--------------------

instance ToFormat TMuxFormat where
  toFormat t = Format $ toText t

instance Printable TMuxFormat where
  print (TMFT t) = print t
  print (TMFB b) = P.text ∘ unFormat $ toFormat b
  print (TMFL l) = P.text ∘ ю $ toText ⊳ l

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

conditional2 ∷ (TMuxFormatTypedable α) =>
               BoolExpr → Maybe α → Maybe α
             → TMuxFormatTyped (TMuxFormatTypedableType α)
conditional2 if_ then_ else_ = TMFC if_ (tmft ⊳ then_) (tmft ⊳ else_)

{- This fails with:

tmux-config.hs:525:17: error: [GHC-25897]
    • Couldn't match type ‘k’ with ‘*’
      Expected: Format @{k} α -> TMuxFormatTyped α0
        Actual: Format @{*} α0 -> TMuxFormatTyped α0
      ‘k’ is a rigid type variable bound by
        the instance declaration
        at tmux-config.hs:524:10-34
    • In the second argument of ‘(∘)’, namely ‘TMFF’
      In the expression: TMFT ∘ TMFF
      In an equation for ‘tmf’: tmf = TMFT ∘ TMFF
    • Relevant bindings include
        tmf :: Format α -> TMuxFormat (bound at tmux-config.hs:525:4)

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

--------------------

tmfv ∷ (Show α, ToFormat α, IsVariable α) => α → TMuxFormat
tmfv = TMFT ∘ TMFV

{- requires UndecidableInstances -}
-- instance (ToFormat α, IsVariable α) => TMuxFormatable α where tmf = tmfv
instance TMuxFormatable FormatVariable where
  tmf = tmfv
instance TMuxFormatable UserVariable where
  tmf = tmfv

-- main ------------------------------------------------------------------------

main :: IO ()
main = do
  say $ toFormat (emptyStyle & alignStyle   ⊩ AlignLeft
                             & rangeStyle   ⊩ RangeLeft
                             & stylePayload ⊩ ExpandTwice @StyleVariable WithoutStrftime (bareOption StatusLeftStyle)
                 )

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

_E                 ∷ FormatSpecifier α → FormatSpecifier α
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
status_left        ∷ FormatSpecifier FormatVariable
status_left        = bareOption StatusLeft
status_right       ∷ FormatSpecifier FormatVariable
status_right       = bareOption StatusRight
user_foobie        ∷ UserVariable
user_foobie        = userVariable "@foobie"
bare_foobie        ∷ FormatSpecifier UserVariable
bare_foobie        = bareOption user_foobie
bare_wname         ∷ FormatSpecifier FormatVariable
bare_wname         = bareOption WindowName

toF_SV             ∷ FormatSpecifier StyleVariable → 𝕋
toF_SV             = toText ∘ toFormat @(FormatSpecifier StyleVariable)

win_stat_last      = bareOption WindowStatusLastStyle

win_last_style     = And (BVar WindowLastFlag)
                         (StrNotEq (StrTxt ∘ toF_SV $ _E win_stat_last)
                                   (StyExp DefaultStyle))

tests ∷ TestTree
tests = localOption Never $
  let ts_ :: [(𝕋,Format SavedDefault)]
      ts_ =
        let left_style_status :: Style (FormatSpecifier StyleVariable)
            left_style_status = emptyStyle & alignStyle   ⊩ AlignLeft
                                           & rangeStyle   ⊩ RangeLeft
                                           & stylePayload ⊩ status_left_style
            toF    ∷ ToFormat α => α -> Format SavedDefault
            toF    = noSaveDefault ∘ toFormat
            toT    ∷ ToFormat α => α -> 𝕋
            toT    = toText ∘ toFormat
            toT_   ∷ FormatSpecifier 𝕋 -> 𝕋
            toT_   = toT @(FormatSpecifier 𝕋)
            ç      = T.intercalate ","

            {- if ⋀ ( (window-has-activity ∨ silence)
                    , window-status-activity-style != default )
               then window-status-activity-style
               else nothing
             -}
            show_window_activity ∷ FormatSpecifier 𝕋 =

              conditional
                (And (Or (BVar WindowActivityFlag)
                         (BVar WindowSilenceFlag))
                     (StrNotEq
                        (StrTxt $
                           toText ∘ toFormat @(FormatSpecifier StyleVariable) $
                             _E $ bareOption WindowStatusActivityStyle)
                        (StyExp DefaultStyle))
                 )
                 (_E $ bareOption WindowStatusActivityStyle)
                 ()

             {- if ⋀ ( window-has-bell
                     , window-status-bell-style != default )
                then window-status-bell-style
                else show_window_activity
              -}
            show_window_bell_or_activity ∷ FormatSpecifier 𝕋 =

             conditional
               (let win_stat_bell =
                      bareOption WindowStatusBellStyle
                in  And (BVar WindowBellFlag)
                        (StrNotEq (StrTxt ∘ toF_SV $ _E win_stat_bell)
                                  (StyExp DefaultStyle)))
               (_E $ bareOption WindowStatusBellStyle)
               show_window_activity


        in  ((second (Format ∘ toText)) ⊳
             [ ( "#{window_name}", tmf WindowName )
             , ( "#{@foobie}", tmf user_foobie )
             , ( "#{=3:window_name}", tmf $ len3 bare_wname )
             , ( "#{=/#{status-left-length}:window_name}"
               , tmf $ len_left_length bare_wname )
             , ( "#{T:@foobie}", tmf $ ExpandTwice WithStrftime bare_foobie )
             , ( -- "#{=3:#{E:@foobie}}" would also work, but is less compact
                "#{E;=3:@foobie}", tmf $ len3 (_E bare_foobie) )

             {- The ordering of the T and the =1 doesn't matter; the T always
                effects:
                > $ tmux set-option @foobie %Y-%M-%d
                > $ tmux display-message -p '#{T;=/1:#{@foobie}}'
                > 2
                > $ tmux display-message -p '#{=/1:#{T:@foobie}}'
                > 2
             -}
             , ( "#{T;=3:@foobie}", tmf $ _T $ len3 bare_foobie)
             , ( "#{T;=3:@foobie}", tmf $ len3 $ _T bare_foobie)
             , ( "#{=/#{status-left-length}:window_name}"
               , tmf $ len_left_length bare_wname )
             , ( "#{E;=3:window_name}", tmf $ _E $ len3 bare_wname )
             , ( "#[range=left align=left #{E:status-left-style}]"
               , tmf left_style_status
               )
             , ( ю [ "#[push-default]"
                   , "#{T;=/#{status-left-length}:status-left}"
                   , "#[pop-default]" ]
               , tmf $ saveDefault (_T $ len_left_length status_left)
               )
             , ( ю [ "#[norange default]"
                   , "#[range=right nolist align=right #{E:status-right-style}]"
                   ]
               , TMFL [ -- the @() is needed to specify the payload type
                        tmf $ emptyStyle_ & rangeStyle   ⊩ RangeNone
                                          & styleDefault ⊢ StyleDefault
                      , tmf $ emptyStyle  & listStyle    ⊩ ListNone
                                          & alignStyle   ⊩ AlignRight
                                          & rangeStyle   ⊩ RangeRight
                                          & stylePayload ⊩ status_right_style
                      ]
               )
             , ( ю [ "#[push-default]"
                   , "#{T;=/#{status-right-length}:status-right}"
                   , "#[pop-default]"
                   ]
               , tmf $ saveDefault (_T $ len_right_length status_right)
               )
             , ( "#[list=on align=#{status-justify}]"
               , tmf $ emptyStyle_ & listStyle ⊩ ListOn
                                   & alignStyle ⊩ AlignOpt StatusJustify
               )
             , ( "#[list=left-marker]<"
               , tmf $ emptyStyle_ & listStyle ⊩ ListLeftMarker "<"
               )
             , ( "#[list=right-marker]>"
               , tmf $ emptyStyle_ & listStyle ⊩ ListRightMarker ">"
               )
             , ( "#[list=on]", tmf $ emptyStyle_ & listStyle ⊩ ListOn )
             , ( "#{W:#{status-left},#{status-right}}",
                 tmf $ ForEachWindow status_left status_right
               )
             , ( "#{W:#[list=on],#[list=focus]}",
                 tmf $ ForEachWindow (emptyStyle_ & listStyle ⊩ ListOn)
                                     (emptyStyle_ & listStyle ⊩ ListFocus)
               )
             , ("#{?window_end_flag,,#{window-status-separator}}"
               , TMFT $ TMFC
                    (BVar WindowEndFlag) 𝓝
                    (𝓙 ∘ TMFS $ BareVariable WindowStatusSeparator)
               )
             , ( "#[push-default]#{T:window-status-format}#[pop-default]"
               , tmf $ saveDefault (_T (bareOption WindowStatusFormat))
               )
             , ( "#[range=window|#{window_index} foo]"
               , tmf $ emptyStyle & rangeStyle ⊩ RangeWindow WindowIndex
                                   & stylePayload ⊩ (StyleText "foo")
               )
             , ( T.intercalate "," [ "#{&&:#{window_last_flag}"
                                   , "#{!=:#{E:window-status-last-style}"
                                   , "default}}"
                                   ]
               , TMFB (And (BVar WindowLastFlag)
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
                       bareOption WindowStatusLastStyle
                 in  tmf $ conditional2
                            win_last_style (𝓙 $ _E win_stat_last) 𝓝
               )
             , ( "#{||:#{window_activity_flag},#{window_silence_flag}}"
               , TMFB $ Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag)
               )

             , ( T.intercalate ","
                 [ "#{&&:#{||:#{window_activity_flag},#{window_silence_flag}}"
                 , "#{!=:#{E:window-status-activity-style}"
                 , "default}}" ]
               , TMFB $
                   And (Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag))
                       (StrNotEq (StrTxt $ toText ∘ toFormat @(FormatSpecifier StyleVariable) $ _E $ bareOption WindowStatusActivityStyle)
                                 (StyExp DefaultStyle))
               )
             , (ç [ ç [ "#{?#{&&:#{||:#{window_activity_flag}"
                      , "#{window_silence_flag}}"
                      , ç ["#{!=:#{E:window-status-activity-style}", "default}}"]
                      ]
                  , "#{E:window-status-activity-style}"
                  , "}"
                  ]
               , tmf $ conditional2
                     (And (Or (BVar WindowActivityFlag) (BVar WindowSilenceFlag))
                          (StrNotEq (StrTxt $ toText ∘ toFormat @(FormatSpecifier StyleVariable) $ _E $ bareOption WindowStatusActivityStyle)
                                    (StyExp DefaultStyle)))
                                    (𝓙 ∘ _E $ bareOption WindowStatusActivityStyle) 𝓝
               )
             , ( "#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}"
               , tmf $ show_window_bell_or_activity
               )
             , ( "#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]"
               , let text_to_style ∷ [TMuxFormat] =
                       [ tmf $
                             conditional2
                               (StrNotEq (StrTxt ∘ toF_SV $ _E $
                                           bareOption WindowStatusCurrentStyle)
                                         (StyExp DefaultStyle))
                               (𝓙 ∘ _E $ bareOption WindowStatusCurrentStyle)
                               (𝓙 ∘ _E $ bareOption WindowStatusStyle)

                               , tmf $ conditional2
                               (And (BVar WindowLastFlag)
                                            (StrNotEq (StrTxt $ toF_SV $ _E $
                                                      bareOption
                                                        WindowStatusLastStyle)
                                                   (StyExp DefaultStyle)))
                               (𝓙 ∘ _E $ bareOption WindowStatusLastStyle)
                               𝓝
                         , tmf show_window_bell_or_activity
                         ]
                 in  tmf $ emptyStyle & rangeStyle ⊩ RangeWindow WindowIndex
                                       & listStyle ⊩ ListFocus
                                       & stylePayload ⊩ text_to_style
               )

             , ( ю [ "#[list=on align=#{status-justify}]#[list=left-marker]<#[list=right-marker]>#[list=on]#{W:#[range=window|#{window_index} #{E:window-status-style}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]#[push-default]#{T:window-status-format}#[pop-default]#[norange default]#{?window_end_flag,,#{window-status-separator}},#[range=window|#{window_index} list=focus #{?#{!=:#{E:window-status-current-style},default},#{E:window-status-current-style},#{E:window-status-style}}#{?#{&&:#{window_last_flag},#{!=:#{E:window-status-last-style},default}},#{E:window-status-last-style},}#{?#{&&:#{window_bell_flag},#{!=:#{E:window-status-bell-style},default}},#{E:window-status-bell-style},#{?#{&&:#{||:#{window_activity_flag},#{window_silence_flag}},#{!=:#{E:window-status-activity-style},default}},#{E:window-status-activity-style},}}]#[push-default]#{T:window-status-current-format}#[pop-default]#[norange list=on default]#{?window_end_flag,,#{window-status-separator}}}"
                   ]
               , let win_stat_last ∷ FormatSpecifier StyleVariable =
                       bareOption WindowStatusLastStyle
                     win_last_style ∷ BoolExpr =
                       And (BVar WindowLastFlag)
                           (StrNotEq (StrTxt ∘ toF_SV $ _E win_stat_last)
                                     (StyExp DefaultStyle))
                     text_to_style ∷ 𝕋 =
                       ю [ toT (ExpandTwice WithoutStrftime
                                            (bareOption WindowStatusStyle))
                         , toText $
                             toF @(FormatSpecifier 𝕋) $
                               conditional (win_last_style∷BoolExpr)
                                           (_E win_stat_last) ()
                         , toText ∘ toF @(FormatSpecifier 𝕋) $
                             show_window_bell_or_activity
                         ]

                 in  TMFL [ tmf $
                              emptyStyle_ & listStyle ⊩ ListOn
                                          & alignStyle ⊩ AlignOpt StatusJustify
                          , tmf $
                              emptyStyle_ & listStyle ⊩ ListLeftMarker "<"
                          , tmf $
                              emptyStyle_ & listStyle ⊩ ListRightMarker ">"
                          , tmf $
                              emptyStyle_ & listStyle ⊩ ListOn

                          , tmf $
                              ForEachWindow
                                (toT ∘ tmf $ [ tmf $
                                     emptyStyle & rangeStyle ⊩ RangeWindow WindowIndex
                                                & stylePayload ⊩ StyleText(text_to_style)
                                 , tmf $
                                     saveDefault (_T $ bareOption WindowStatusFormat)
                                 , tmf $ emptyStyle_ & rangeStyle   ⊩ RangeNone
                                                       & styleDefault ⊢ StyleDefault
                                 , tmf $
                                     conditional2 (BVar WindowEndFlag)
                                                  𝓝 (𝓙 WindowStatusSeparator)
                                  ])
                                (toT ∘ tmf $
                                   let text_to_style' =
                                         ю [ toText ∘ toFormat @(FormatSpecifier 𝕋) $
                                               conditional
                                                 (StrNotEq (StrTxt ∘ toF_SV $ _E $
                                                              bareOption WindowStatusCurrentStyle)
                                                           (StyExp DefaultStyle))
                                                 (_E $ bareOption WindowStatusCurrentStyle)
                                                 (_E $ bareOption WindowStatusStyle)
                                           , let win_stat_last ∷ FormatSpecifier StyleVariable
                                                 win_stat_last =
                                                   bareOption WindowStatusLastStyle
                                                 win_last_style =
                                                   And (BVar WindowLastFlag)
                                                       (StrNotEq (StrTxt ∘ toF_SV $ _E win_stat_last)
                                                                 (StyExp DefaultStyle))
                                             in  toT @(FormatSpecifier 𝕋) $
                                                   conditional (win_last_style∷BoolExpr)
                                                               (_E win_stat_last) ()
                                                                       , toT $ show_window_bell_or_activity
                                           ]

                                   in   [ tmf $ emptyStyle & rangeStyle ⊩ RangeWindow WindowIndex & listStyle ⊩ ListFocus & stylePayload ⊩ StyleText(text_to_style')
                                         , tmf $ saveDefault $ _T (bareOption WindowStatusCurrentFormat)
                                         , tmf $
                                             emptyStyle_ & rangeStyle   ⊩ RangeNone
                                                         & styleDefault ⊢ StyleDefault
                                                         & listStyle    ⊩ ListOn
                                         , tmf $
                                             conditional2
                                               (BVar WindowEndFlag)
                                               𝓝
                                               (𝓙 WindowStatusSeparator)
                                     ]
                                )
                          ]
               )
             ])

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
