{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

import Base1T
import Prelude  ( error, undefined )

-- base --------------------------------

import Data.Function  ( flip )
import Data.List      ( concat, filter, intercalate, splitAt )
import Data.Maybe     ( catMaybes, fromMaybe )
import Data.Ord       ( Ordering( EQ, GT, LT ), compare )
import Data.String    ( IsString( fromString ) )
import Data.Tuple     ( uncurry )
import GHC.Num        ( Num( (*) ) )
import GHC.Real       ( Integral, Ratio( (:%) ), (%), ceiling, quotRem, round )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- env-plus ----------------------------

import Env.Types  ( EnvKey, EnvVal, э, ӭ, ek )

-- fpath -------------------------------

import FPath.AbsFile           ( AbsFile )
import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( File )
import FPath.Parseable         ( readM )

-- lens --------------------------------

import Control.Lens.At  ( ix )

-- log-plus ----------------------------

import Log  ( Log, debugT )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( NoMock ) )

-- mockio-log --------------------------

import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process  ( ꙫ )

-- monadio-plus ------------------------

import MonadIO.Base                   ( getArgs )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.FPath                  ( pResolve )
import MonadIO.Process                ( throwExit )

-- more-unicode ------------------------

import Data.MoreUnicode.Tasty  ( (≣) )

-- mtl ---------------------------------

import Control.Monad.Reader  ( runReaderT )

-- natural -----------------------------

import Natural  ( length )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, help, long, metavar, option
                                    , short, switch, strArgument, value )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReadM, readT )

-- parsec ------------------------------

import Text.Parsec.Prim  ( Parsec )

-- parsec-plus-base --------------------

import ParsecPlusBase  ( Parsecable( parsec, parser ), parse )
import Parsec.Error    ( AsParseError, ParseError )

-- parser-plus -------------------------

import ParserPlus  ( tries )

-- parsers -----------------------------

import qualified  Text.Parser.Token

import Text.Parser.Char         ( CharParsing, char, noneOf )
import Text.Parser.Combinators  ( sepEndBy )
import Text.Parser.Token        ( integer )

-- QuickCheck --------------------------

import Test.QuickCheck.Property  ( (==>) )

-- range -------------------------------

import Data.Range  ( Bound( Bound ), BoundType( Exclusive, Inclusive )
                   , Range( InfiniteRange, LowerBoundRange, SingletonRange
                          , SpanRange, UpperBoundRange )
                   , fromRanges, intersection
                   )

-- stdmain -----------------------------

import StdMain             ( stdMainNoDR )
import StdMain.UsageError  ( UsageParseFPProcIOError )

-- tasty-plus --------------------------

import TastyPlus  ( (≟) )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Arbitrary( arbitrary ),chooseEnum,testProperty )

-- text --------------------------------

import Data.Text  ( pack )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- trifecta ----------------------------

import Text.Trifecta.Parser  ( parseFromFile )

--------------------------------------------------------------------------------

{-| A numeric range intended to select some items from a list;
    -) by range (including a -ve index, meaning "count backwards from the end")
    -) by percentage (again, allowing a -ve, meaning "count backwards from the
       end")
    -) by pieces, e.g., 3 of 5, meaning divide into 5 equal(ish) pieces and
       select the fourth (3 → 0-based).
-}
data IntRange = IntRange (Range ℤ)
            | RangePerCent ℤ -- range as a +ve/-ve %
            | RangePieces ℕ ℕ -- choose the mth of n equal pieces
  deriving (Eq,Show)

parseIntRange'   ∷ Parsec 𝕊 () (Range ℤ)
parseIntRange'   =
  let incl x           = Bound x Inclusive
      span a b        = SpanRange (incl a) (incl b)
  in  tries $ (span ⊳ integer ⊵ char '-' ⋫ integer)
           :| [ LowerBoundRange ∘ incl ⊳ integer ⋪ char '-'
              , UpperBoundRange ∘ incl ⊳ (char '-' ⋫ integer)
              , SingletonRange ⊳ integer
              , const InfiniteRange ⊳ char '-'
              ]
parseIntPerCent ∷ Parsec 𝕊 () ℤ
parseIntPerCent = integer ⋪ char '%'

parseIntRange ∷ Parsec 𝕊 () IntRange
parseIntRange = tries $ (RangePerCent ⊳ parseIntPerCent)
                     :| [ parsePieces, IntRange ⊳ parseIntRange' ]

parseIntRangeHelp ∷ 𝕊
parseIntRangeHelp = ю [ "Select a range value, which is intersected against "
                      , "the list of things; it may be of the following "
                      , "things: (a) a regular range, either (i) n "
                      , "(singleton), (ii) a-, -a, a-b (range, possibly "
                      , "open-ended); these may take a negative integer, which "
                      , "is taken to be an offset from the end of the list "
                      , "(b) a percentage, of the form m%, meaning show the "
                      , "first m% of the list (or -m% meaning show the last "
                      , "m%) (c) a pieces specification, of the form m:n, "
                      , "meaning divide the list into n pieces and select the "
                      , "mth.  All values are 0-based."
                      ]

------------------------------------------------------------

newtype EnvKeyVal = EnvKeyVal { unEnvKeyVal ∷ (EnvKey,EnvVal) }
  deriving Show

instance Printable EnvKeyVal

instance Textual EnvKeyVal where
  textual =
    let parseEnvKey = fromString ⊳ some (noneOf "=") ⋪ char '='
    in  EnvKeyVal ⊳ ((,) ⊳ parseEnvKey ⊵ textual)

envkey ∷ EnvKeyVal → EnvKey
envkey (EnvKeyVal (k,_)) = k

data Options = Options { _envfn     ∷ File
                       , _cmd       ∷ File
                       , _cmdargs   ∷ [𝕋]
                       , _range     ∷ IntRange
                       , _nullLines ∷ 𝔹
                       , _retain    ∷ [EnvKey]
                       , _override  ∷ [EnvKeyVal]
                       }

cmd ∷ Lens' Options File
cmd = lens _cmd (\ o c → o { _cmd = c })

cmdargs ∷ Lens' Options [𝕋]
cmdargs = lens _cmdargs (\ o c → o { _cmdargs = c })

envfn ∷ Lens' Options File
envfn = lens _envfn (\ o efn → o { _envfn = efn })

range ∷ Lens' Options IntRange
range = lens _range (\ o r → o { _range = r })

nullLines ∷ Lens' Options 𝔹
nullLines = lens _nullLines (\ o n → o { _nullLines = n })

retain ∷ Lens' Options [EnvKey]
retain = lens _retain (\ o eks → o { _retain = eks })

override ∷ Lens' Options [EnvKeyVal]
override = lens _override (\ o eks → o { _override = eks })

----------------------------------------

natural ∷ Text.Parser.Token.TokenParsing η ⇒ η ℕ
natural = fromIntegral ⊳ Text.Parser.Token.natural

parsePieces     ∷ Parsec 𝕊 () IntRange
parsePieces     = let pieces m n =
                        if m < n
                        then RangePieces m n
                        else error $ [fmt|%d must be less than %n|] m n
                  in  pieces ⊳ natural ⊵ (char ':' ⋫ natural)

-- helper for simple unit tests

{-| check that @f@, when applied to @input@, results in @expect@.  @show*@ used
    for formatting error messages. -}
makeCheck' ∷ ∀ α ω . (Eq ω, Show ω) ⇒
             (α → 𝕋) → (ω → 𝕋) → (α → ω) → ω → α → TestTree
makeCheck' showInput showExpect f expect input =
  let test_name = [fmt|%t → %t|] (showInput input) (showExpect expect)
  in  testCase test_name $ expect @=? f input

makeCheck2' ∷ ∀ α β ω . (Eq ω, Show ω) ⇒
             (α → β → 𝕋) → (ω → 𝕋) → (α → β → ω) → ω → (α,β) → TestTree
makeCheck2' showInput showExpect f expect input =
  makeCheck' (uncurry showInput) showExpect (uncurry f) expect input

makeCheck2'T ∷ ∀ α β ω . (Eq ω, Show ω) ⇒
              (α → β → 𝕋) → (α → β → ω) → ω → (α,β) → TestTree
makeCheck2'T showInput = makeCheck2' showInput (pack ∘ show)

{-| like @makeCheck'@, but @f@ produces an @Either@, which is expected to be
    a @Right@ value; the @Left@ type of the either must be @Printable@. -}
makeCheckE' ∷ ∀ ε α ω . (Eq ε, Eq ω, Show ω, Show ε, Printable ε) ⇒
             (α → 𝕋) → (ω → 𝕋) → (α → 𝔼 ε ω) → ω → α → TestTree
makeCheckE'  showInput showExpect f expect =
  makeCheck' showInput (either toText showExpect) f (𝕽 expect)

{-| like @makeCheck'E@, but uses @show@ to name the tests -}
makeCheckE ∷ ∀ ε α ω . (Eq ε, Eq ω, Show ω, Show α, Show ε, Printable ε) ⇒
             (α → 𝔼 ε ω) → ω → α → TestTree
makeCheckE = makeCheckE' (pack ∘ show) (pack ∘ show)

{-| like @makeCheck'@, but uses @toText@ to name the tests -}
makeCheck ∷ ∀ α ω . (Eq ω, Show ω, Printable α, Printable ω) ⇒
            (α → ω) → ω → α → TestTree
makeCheck f = makeCheck' toText toText f

parsePiecesTests ∷ TestTree
parsePiecesTests =
  let
    check ∷ IntRange → 𝕊 → TestTree
    check e i = makeCheckE @ParseError (parse parsePieces i) e i
  in
    testGroup "parsePieces" [ check (RangePieces 2 5) "2:5" ]

parseOptions ∷ Parser Options
parseOptions =
  let
    retain_help = intercalate " " [ "retain these keys from the environment"
                                  , "(overrides any settings in the input file)"
                                  ]
    set_help    = intercalate " " [ "set these keys (overrides any settings in"
                                  , "the input file)" ]
  in
    Options ⊳ argument readM (metavar "ENVFILE")
            ⊵ argument readM (metavar "CMD")
            ⊵ many (strArgument (metavar "ARGS"))
            ⊵ option (parsecReadM "INT-RANGE" parseIntRange)
                     (ю [ long  "range"
                        , short 'r'
                        , value (IntRange InfiniteRange)
                        , help  parseIntRangeHelp
                        ])
            ⊵ switch (short '0' ⊕ help "parse input file with null line-ends")
            ⊵ many (option (parsecReadM "ENVKEY" parser)
                           (ю [ short 't', long "retain", help retain_help ]))
--  parsecReadM "" ((,) ⊳ (read ⊳ some digit) ⋪ char '=' ⊵ (pack ⊳ some anyChar))

            ⊵ many (option readT
                   (ю [ short 's', long "set", help set_help ]))

----------------------------------------

{-| Parse a list of pairs, where each pair is delimited by a null character; and
    the key & value are delimited by an '='.  The value may have '=' characters
    in it, but neither key nor value may have a null character.

    We keep this as a list of pairs, rather than a map, to preserve ordering of
    input; that makes comprehension of the results much easier to predict.
-}

parsePairs ∷ ∀ ε α β ρ η . (MonadError ε η, AsParseError ε,
                            Parsecable α, Parsecable β, CharParsing ρ) ⇒
             ℂ → ℂ → ρ (η [(α,β)])
parsePairs assign line_end =
  let
    no_delim          = noneOf [assign,line_end]
    no_line_end       = noneOf [line_end]
    parse_line        = (,) ⊳ many no_delim ⋪ char assign ⊵ many no_line_end
    parsec_pair (x,y) = parsec x x ≫ \ x' → parsec y y ≫ \ y' → return (x',y')
  in
    sequence ⊳ (parsec_pair ⊳⊳ parse_line `sepEndBy` char line_end)

----------------------------------------

{-_ parse a file into a list of pairs -}
parseFile ∷ ∀ ε α β η μ . (MonadIO μ, AsParseError ε, MonadError ε η,
                           Parsecable α, Parsecable β) ⇒
            ℂ → ℂ → File → μ (Maybe (η [(α,β)]))
parseFile assign line_end f =
  parseFromFile (parsePairs assign line_end) (f ⫥ filepath)

----------------------------------------

{-| Convert a (Range ℤ) to an (Range ℕ) by treating any negative values as an
    offset from the end of the range. -}

offset ∷ Integral χ ⇒ ℕ → χ → ℕ
offset n x =
  case x `compare` 0 of
    GT → fromIntegral x -- x is +ve; just use x
    EQ → 0              -- x is 0
    LT →                -- x is -ve
      case abs' x `compare` n of
        LT →   -- x is -ve, but abs x < n
             max 0 (fromIntegral $ fromIntegral @_ @ℤ n + fromIntegral x)
        EQ → 0 -- x is -ve, but abs x ≡ n
        GT → -- x is -ve, abs x > n; n is out-of-range; not sure what to
             -- do with this
             error $ [fmt|not sure what to do with %d|] x

offsetTests ∷ TestTree
offsetTests =
  let
    check ∷ ℕ → ℤ → TestTree
    check exp i = testCase ([fmt|%d → %d|] i exp) $ exp @=? offset 4 i
  in
    testGroup "offset"
              [ check 0 0
              , check 1 1
              , check 2 2
              , check 3 3
              , check 4 4
              , check 5 5
              , check 3 (-1)
              , check 2 (-2)
              , check 1 (-3)
              , check 0 (-4)
              ]

rangeSelect ∷ (Integral β, Show α) ⇒ Range β → [α] → [α]
rangeSelect (fmap (fromIntegral @_ @ℤ) → rng) input =
  let count = length input
      incl ∷ ℕ → Bound ℕ
      incl x = Bound x Inclusive
      (⧏) ∷ Integral β ⇒ [α] → Range β → [α]
      is ⧏ rs =
        let i_r ∷ Range ℕ
            i_r = case count of
              0 → SingletonRange 0
              _ → SpanRange (incl 0) (incl $ count-1)
            (~~) ∷ (Integral χ) ⇒ [α] → χ → 𝕄 α
            xs ~~ n = xs ⩼ ix (fromIntegral $ offset count n)
            rs' = [offset count ⊳ rs]
        in  catMaybes [ is ~~ x | x ← fromRanges $ intersection rs' [i_r] ]
  in input ⧏ rng

rangeSelectTests ∷ TestTree
rangeSelectTests =
  let
    incl ∷ ℤ → Bound ℤ
    incl x = Bound x Inclusive
    check' ∷ 𝕊 → Range ℤ → 𝕊 → TestTree
    check' input r exp = testCase ([fmt|%w|] r) $ exp ≟ rangeSelect r input
    check = check' "abcd"
  in
    testGroup "rangeSelect"
              [ check' "" (SingletonRange 0)            ""
              , check' "" InfiniteRange                 ""
              , check' "" (LowerBoundRange (incl 3))    ""
              , check' "" (LowerBoundRange (incl 0))    ""
              , check' "" (UpperBoundRange (incl 3))    ""
              , check' "" (UpperBoundRange (incl 0))    ""
              , check' "" (SpanRange (incl 1) (incl 4)) ""

              , check InfiniteRange                 "abcd"
              , check (SingletonRange 0)            "a"
              , check (SingletonRange 1)            "b"
              , check (SingletonRange 2)            "c"
              , check (SingletonRange 3)            "d"
              , check (SingletonRange 4)            ""
              , check (SingletonRange (-1))         "d"
              , check (SingletonRange (-2))         "c"
              , check (SingletonRange (-3))         "b"
              , check (SingletonRange (-4))         "a"
              , check (LowerBoundRange (incl 5))    ""
              , check (LowerBoundRange (incl 4))    ""
              , check (LowerBoundRange (incl 3))    "d"
              , check (LowerBoundRange (incl 2))    "cd"
              , check (LowerBoundRange (incl 1))    "bcd"
              , check (LowerBoundRange (incl 0))    "abcd"
              , check (LowerBoundRange (incl (-4))) "abcd"
              , check (LowerBoundRange (incl (-3))) "bcd"
              , check (LowerBoundRange (incl (-2))) "cd"
              , check (LowerBoundRange (incl (-1))) "d"
              , check (UpperBoundRange (incl 5))    "abcd"
              , check (UpperBoundRange (incl 4))    "abcd"
              , check (UpperBoundRange (incl 3))    "abcd"
              , check (UpperBoundRange (incl 2))    "abc"
              , check (UpperBoundRange (incl 1))    "ab"
              , check (UpperBoundRange (incl 0))    "a"
              , check (UpperBoundRange (incl (-4))) "a"
              , check (UpperBoundRange (incl (-3))) "ab"
              , check (UpperBoundRange (incl (-2))) "abc"
              , check (UpperBoundRange (incl (-1))) "abcd"
              , check (SpanRange (incl 0) (incl 4)) "abcd"
              , check (SpanRange (incl 0) (incl 3)) "abcd"
              , check (SpanRange (incl 0) (incl 2)) "abc"
              , check (SpanRange (incl 0) (incl 1)) "ab"
              , check (SpanRange (incl 0) (incl 0)) "a"
              , check (SpanRange (incl 0) (incl (-1))) "abcd"
              , check (SpanRange (incl 0) (incl (-2))) "abc"
              , check (SpanRange (incl 0) (incl (-3))) "ab"
              , check (SpanRange (incl 0) (incl (-4))) "a"
              , check (SpanRange (incl 1) (incl 4)) "bcd"
              , check (SpanRange (incl 1) (incl 3)) "bcd"
              , check (SpanRange (incl 1) (incl 2)) "bc"
              , check (SpanRange (incl 1) (incl 1)) "b"
              , check (SpanRange (incl 1) (incl (-1))) "bcd"
              , check (SpanRange (incl 1) (incl (-2))) "bc"
              , check (SpanRange (incl 1) (incl (-3))) "b"
              ]

----------------------------------------

(⇔) ∷ Ord α ⇒ α → α → Ordering
(⇔) = compare

abs' ∷ Integral χ ⇒ χ → ℕ
abs' y = fromIntegral $ abs y

perCentSelect ∷ (Integral χ, Show χ, Show α) ⇒ χ → [α] → [α]
perCentSelect pc xs = rangeSelect r xs
  {- Given a list of length 4, 50% should be 0-1, -50% should be 2-3
       (obviously).
     Given a list of length 5, 50% should be 0-2, -50% should be 3-4
       (round in favour of the +ve %)
     Given a list of length 4, 20%  should be 0, -80% should be 1-3
       (round to the nearest value)
     Given a list of length 4, 10%  should be empty, -90% should be 0-3
       (round to the nearest value)
  -}

  where
    r = let pc' ∷ ℕ
            bound_type ∷ ℕ → Range ℕ
            incl x = Bound x Inclusive
            excl x = Bound x Exclusive
            (pc',bound_type) =
              case pc ⇔ 0 of
                LT → (fromIntegral $ 100 + pc, \ x → LowerBoundRange (incl x))
                _  → (fromIntegral pc, \ x → UpperBoundRange (excl x))
            bound ∷ Ratio ℕ
            bound = length xs * pc' % 100
            rnd ∷ Ratio ℕ → ℕ
            rnd x@(num :% dem) = case dem of
              1 → num
              2 → ceiling x
              _ → round x
        in  bound_type (rnd bound)

data ArbitraryPC = ArbitraryPC { unArbitraryPC ∷ ℕ }
  deriving Show

toZ ∷ ArbitraryPC → ℤ
toZ = fromIntegral ∘ unArbitraryPC

toNegZ ∷ ArbitraryPC → ℤ
toNegZ = ((-1)*) ∘ fromIntegral ∘ (100-) ∘ unArbitraryPC

instance Arbitrary ArbitraryPC where
  arbitrary = ArbitraryPC ⊳ chooseEnum (0,100)

perCentSelectTests ∷ TestTree
perCentSelectTests =
  let
    check' ∷ 𝕊 → ℤ → 𝕊 → TestTree
    check' input i exp =
      makeCheck2'T [fmt|%03d → %s|] perCentSelect exp (i,input)
    check = check' "abcde"
  in
    testGroup "perCentSelect"
              [ check' ""   0 ""
              , check' ""  50 ""
              , check' "" 100 ""

              , check' "abcd"   50  "ab"
              , check' "abcd" (-50) "cd"
              , check   50  "abc"
              , check (-50) "de"
              , check' "abcd"   20  "a"
              , check' "abcd" (-80) "bcd"
              , check' "abcd"   10  ""
              , check' "abcd" (-90) "abcd"

              , check    0  ""
              , check   20  "a"
              , check   40  "ab"
              , check   60  "abc"
              , check   80  "abcd"
              , check  100  "abcde"
              , check (-80) "bcde"
              , check (-60) "cde"
              , check (-40) "de"
              , check (-20) "e"
              , check   50  "abc"
              , check (-50) "de"

              , check' "a"   43  ""
              , check' "a" (-57) "a"

              , testProperty "split is covering"
                (\ (xs ∷ 𝕊) p →
                    let pcs  = perCentSelect (toZ p) xs
                        pcs' = perCentSelect (toNegZ p) xs
                    in  xs ≣ pcs ⊕ pcs')
              ]


rangePieces ∷ ℕ → [α] → [[α]]
rangePieces 0 _  = error "cannot split into 0 pieces"
rangePieces n xs =
  let l = length xs
      (q,r) = l `quotRem` n
      go ∷ ℕ → [β] → [[β]]
      go i ys | i ≥ (n-1) = [ys]
              | otherwise = let tk = q+ (if i < r then 1 else 0)
                            in  let (d,e) = splitAt (fromIntegral tk) ys
                                in  d : go (i+1) e
  in  go 0 xs

rangePiecesTests ∷ TestTree
rangePiecesTests =
  let
    check' ∷ 𝕊 → ℕ → [𝕊] → TestTree
    check' input i exp = makeCheck2'T [fmt|%03d → %L|] rangePieces exp (i,input)
    check = check' "abcde"
  in
    testGroup "rangePieces"
      [ check 5 ["a","b","c","d","e"]
      , check 4 ["ab","c","d","e"]
      , check 3 ["ab","cd","e"]
      , check 2 ["abc", "de"]
      , check 1 ["abcde"]
      , check 6 ["a","b","c","d","e", "" ]

      , testProperty "split is covering"
        (\ (xs ∷ 𝕊) (unArbitraryPC → p) →
           (p ≢ 0) ==> xs ≣ concat (rangePieces p xs))
      ]

rangeSelect' ∷ Show α ⇒ IntRange → [α] → [α]
rangeSelect' (IntRange        r) xs = rangeSelect r xs
rangeSelect' (RangePerCent  p) xs = perCentSelect p xs
rangeSelect' (RangePieces m n) xs = fromMaybe [] (m ! rangePieces n xs)

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, Printable ε, AsParseError ε,
          AsIOError ε, AsProcExitError ε, AsCreateProcError ε, AsFPathError ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain opts =
  let
    mock = NoMock
  in
    flip runReaderT mock $ do
      let efn = opts ⊣ envfn
      let line_end = if opts ⊣ nullLines then '\0' else '\n'
      parseFile @_ @EnvKey @EnvVal '=' line_end efn ≫ \ case
        𝕹 → throwError ∘ userE $ [fmt|failed to parse '%T'|] efn
        𝕵 (𝕷 e) → throwError e
        𝕵 (𝕽 env) → do
          let override_keys = (opts ⊣ retain) ⊕ (envkey ⊳ (opts ⊣ override))
              env' ∷ [(EnvKey,EnvVal)]
              env' = filter (\ (k,_) → ﬧ (k ∈ override_keys)) $
                       rangeSelect' (opts ⊣ range) env
          cmd' ← pResolve @AbsFile (opts ⊣ cmd)
          (exit_info,()) ← ꙫ (cmd', (opts ⊣ cmdargs), [ӭ [[ek|PATH|]],э (env' ⊕ (unEnvKeyVal ⊳ opts ⊣ override))])
          throwExit (exit_info, ()) ⪼ return 0

----------------------------------------

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "run with a fixed environment as read from a file"
  getArgs ≫ stdMainNoDR progDesc parseOptions (myMain @UsageParseFPProcIOError)

-- tests -----------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "setenv" [ offsetTests, perCentSelectTests, rangeSelectTests
                           , rangePiecesTests, parsePiecesTests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
