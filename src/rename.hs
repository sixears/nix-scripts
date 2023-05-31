{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MonadComprehensions #-}

-- XXX add ability to pass through arguments (as _1, _2, etc.?)
-- XXX add counter (thing incrementing; one per attempted match, one per successful match)
-- XXX add count of arguments
-- XXX add RE fn args pre-check ?
-- XXX Provide Printable of REPlacement; use it in logging (e.g., of std
--     replacement)

{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.List            ( nub )
import Data.List.NonEmpty   ( filter )
import Data.Maybe           ( catMaybes )
import GHC.Exts             ( toList )

-- containers --------------------------

import qualified  Data.Map.Strict  as  Map

-- containers-plus ---------------------

import ContainersPlus.Map  ( AsRepeatedKeyError( _RepeatedKeyError )
                           , RepeatedKeyError {-, fromList -} )

-- env-fpath ---------------------------

import Env.FPath  ( envRcAbsFile )

-- env-plus ----------------------------

import Env.Reader  ( runEnv )

-- fpath -------------------------------

import qualified FPath.Parseable

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.AsFilePath        ( filepath )
import FPath.Dirname           ( dirname )
import FPath.File              ( File, FileAs( _File_ ) )
import FPath.Error.FPathError  ( AsFPathError( _FPathError ) )
import FPath.Parseable         ( readM )
import FPath.RelFile           ( relfile )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog
                          , Severity( Debug, Informational, Notice ) )

-- log-plus ----------------------------

import Log  ( Log, logIO, logIOT )

-- mockio-log --------------------------

import MockIO.Log          ( DoMock( NoMock ), HasDoMock, mkIOLMER )
import MockIO.IOClass      ( HasIOClass, IOClass( IOWrite ) )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Directory  ( mkdir )
import MockIO.File       ( lfexists' )
import MockIO.OpenFile   ( readFileY )

-- monadio-plus ------------------------

import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError( _CreateProcError ) )
import MonadIO.Error.ProcExitError   ( AsProcExitError( _ProcExitError ) )
import MonadIO.FPath                 ( pResolve )
import MonadIO.FStat                 ( FExists( FExists, NoFExists ) )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, eitherReader, flag, help, long
                                    , metavar, short, strOption )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReader, parseNE, textualOption )

-- parsec ------------------------------

import qualified Text.Parsec.Error
import qualified Text.Parsec.Prim

import Text.Parsec.Pos    ( SourceName )
import Text.Parsec.Prim   ( Parsec, Stream )

-- parsec-plus -------------------------

import Parsec.Error  ( AsParseError( _ParseError ), ParseError( ParseError ) )
import ParsecPlus    ( Parsecable( parser, parsec ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char, noneOf, spaces )
import Text.Parser.Combinators  ( eof, optional, try )

-- parser-plus -------------------------

import ParserPlus  ( stringMaybeDQuoted )

-- pcre --------------------------------

import PCRE              ( compRE, replace, replace1, replaceMany )
import PCRE.Error        ( AsREFnError( _REFnError )
                         , AsREGroupError( _REGroupError )
                         , REFnGroupError, REParseError
                         )
import PCRE.REPlacement  ( REPlacement( REPlacement ) )
import PCRE.ReplText     ( ReplText, repltext )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( RE, re )

-- stdmain -----------------------------

import StdMain             ( Overwrite( NoOverwrite, Overwrite )
                           , checkInputFiles, checkOutputFiles, stdMain
                           , throwUsageErrors
                           )
import StdMain.UsageError  ( AsUsageError( _UsageError )
                           , UsageParseFPProcIOError )

-- text --------------------------------

import Data.Text     ( pack, unlines )
import Data.Text.IO  ( putStrLn )

-- unix --------------------------------

import qualified  System.Posix.Files

--------------------------------------------------------------------------------

data MkDirs = MkDirs | NoMkDirs
  deriving (Eq,Show)

------------------------------------------------------------

data Quiet = NoQuiet | Quiet
  deriving (Eq,Show)

------------------------------------------------------------

data ReplacementOpt = ROptRepl   REPlacement
                    | ROptREName 𝕋
                    | ROptFile1  File
                    | ROptFileN  File

------------------------------------------------------------

data Options = Options { _repl_opt ∷ ReplacementOpt
                       , _inputs   ∷ NonEmpty File
                       , _mkdirs   ∷ MkDirs
                       , _overwr   ∷ Overwrite
                       , _quiet    ∷ Quiet
                       }

--------------------

inputs ∷ Lens' Options (NonEmpty File)
inputs = lens _inputs (\ o is → o { _inputs = is })

--------------------

repl_opt ∷ Lens' Options ReplacementOpt
repl_opt = lens _repl_opt (\ o rp → o { _repl_opt = rp })

--------------------

mkdirs ∷ Lens' Options MkDirs
mkdirs = lens _mkdirs (\ o m → o { _mkdirs = m })

--------------------

overwr ∷ Lens' Options Overwrite
overwr = lens _overwr (\ o w → o { _overwr = w })

--------------------

quiet ∷ Lens' Options Quiet
quiet = lens _quiet (\ o q → o { _quiet = q })

------------------------------------------------------------

parseReplText ∷ Parser ReplText
parseReplText = argument parsecReader (metavar "REPLACEMENT")

parseRE ∷ Parser RE
parseRE = argument (eitherReader (first toString ∘ compRE @REParseError ∘ pack))
                   (metavar "REGEX")

parseREPlacement ∷ Parser REPlacement
parseREPlacement = REPlacement ⊳ parseRE ⊵ parseReplText

parseReplacementOpt ∷ Parser ReplacementOpt
parseReplacementOpt =
    ROptRepl ⊳ parseREPlacement
  ∤ ROptREName ⊳ (pack ⊳ strOption (ю [ short 'n', long "name", metavar "REName"
                                      , help "select this named RE from re file"
                                      ]))
  ∤ ROptFile1 ⊳ (textualOption (ю [ short 'f', long "file1"
                                  , help "use 1 RE from this file" ]))
  ∤ ROptFileN ⊳ (textualOption (ю [ short 'F', long "file-many"
                                  , help "use many REs from this file" ]))

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ parseReplacementOpt
          ⊵ parseNE (argument readM (metavar "FILENAME"))
          ⊵ flag NoMkDirs MkDirs (ю [ short 'M', long "mkdirs"
                                    , help "make missing directories" ])
          ⊵ flag NoOverwrite Overwrite (ю [ short 'O', long "overwrite"
                                          , help "overwrite extant files" ])
          ⊵ flag NoQuiet Quiet (ю [ short 'q', long "no-output"
                                  , help "don't output file moves" ])

------------------------------------------------------------

-- XXX monadio-plus
rename' ∷ ∀ ε γ δ μ . (MonadIO μ, HasCallStack, FileAs γ, FileAs δ,
                       AsIOError ε, MonadError ε μ, HasCallStack) ⇒
          γ → δ → μ ()
rename' (review _File_ → from) (review _File_ → to) =
  liftIO $ System.Posix.Files.rename (from ⫥ filepath) (to ⫥ filepath)

----------------------------------------

-- XXX mockios-plus
rename ∷ ∀ ε γ δ ω μ .
         (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω, FileAs γ,
          FileAs δ, Printable γ, Printable δ) ⇒
         Severity → γ → δ → DoMock → μ ()
rename sev from to =
  let msg = [fmt|renam '%T' → '%T'|] from to
   in mkIOLMER sev IOWrite msg 𝕹 () (rename' from to)

----------------------------------------

{- | A regex file contains a list of regular expressions, with their
     replacements.  It is line-oriented.

     Each line must be either:
     -) 3 (posibly multiple) tab-separated columns, being name,regex,replacement.
        The name must be a simple identifier ([[:alpha:]]([\w_]*).
        The names must be unique.
     -) A sequence of space/tab characters
     -) A comment, beginning with '#'.
 -}

pe ∷ AsParseError ε ⇒ Text.Parsec.Error.ParseError → ε
pe e = _ParseError # ParseError e callStack

parse ∷ ∀ ε α σ τ η . (AsParseError ε, MonadError ε η, Stream σ Identity τ) ⇒
        Parsec σ () α → SourceName → σ → η α
parse p s t = either (throwError ∘ pe) return $ Text.Parsec.Prim.parse p s t

parseRegexLine ∷ ∀ ε σ η .
                 (Stream σ Identity ℂ, Show σ, AsParseError ε, MonadError ε η) ⇒
                 SourceName → σ → η (𝕄 (𝕋, REPlacement))
parseRegexLine =
  let spc = spaces ⋪ optional (char '#' ⋪ many (noneOf "\n"))
      namedREPl ∷ Stream σ' Identity ℂ ⇒ Parsec σ' ξ (𝕋, REPlacement)
      namedREPl = (,) ⊳ (pack ⊳ stringMaybeDQuoted ⋪ many (char '\t')) ⊵ parser
      parseLine ∷ Stream σ Identity ℂ ⇒ Parsec σ () (𝕄 (𝕋,REPlacement))
      parseLine = try (𝕵 ⊳ (namedREPl) ⋪ spc) ∤ (pure 𝕹 ⊳ spc)
  in
    parse (parseLine ⋪ eof)

parseRegexLineTests ∷ TestTree
parseRegexLineTests =
  let checkN s =
        testCase s $ 𝕽 𝕹 @=? parseRegexLine @ParseError "test" (pack s)
      check ex s =
        testCase s $ 𝕽 (𝕵 ex) @=? parseRegexLine @ParseError "test" (pack s)
   in testGroup "parseRegexLine"
        [ checkN "", checkN "\t", checkN "\t  \t", checkN "  \t  "
        , check ("rep1_0",rep1_0) (ю [ "rep1_0\t\t${iggy}(fo+)${pop}(.ar)\t\""
                                     , ">>${pop}<< (${1}) [${0}]\"" ])
        , check ("rep3_3",rep3_3) (ю [ "rep3_3\t\"(?<=/)foo\\\\.*(.{3})\"\t\t"
                                     , "\"quux.${.title 1}/\"  # c" ])
        ]

----------------------------------------

-- XXX use parsecFileUTF8 ?
readRegexFile ∷ (MonadIO μ,
                 AsParseError ε, AsIOError ε, AsRepeatedKeyError 𝕋 ε,
                 Printable ε, MonadError ε μ, HasCallStack,
                 Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                AbsFile → μ (Map.Map 𝕋 REPlacement)

readRegexFile fn = do
  let msg = [fmt|reading regexen from %T|]
  ls ← readFileY @_ @[𝕋] Informational (𝕵 $ msg) (return "") fn NoMock
  result ← case ls of
    𝕹     → return Map.empty
    𝕵 ls' → fromList ∘ catMaybes ⊳ forM ls' (parseRegexLine (toString fn))
  forM_ (toList result)
        (\ (k,v) → logIO Debug def $ [fmtT|read RE: %t:\t%w|] k v)
  return result

----------------------------------------

-- XXX Clean this up, including logging useful things
renameFile ∷ ∀ ε ω μ . (MonadIO μ,
                        HasDoMock ω, HasIOClass ω, Default ω,
                        MonadLog (Log ω) μ,
                        AsFPathError ε, AsREFnError ε, AsREGroupError ε,
                        AsIOError ε, AsParseError ε, AsRepeatedKeyError 𝕋 ε,
                        Printable ε, HasCallStack, MonadError ε μ) ⇒
             Options → AbsFile → μ (AbsFile,AbsFile)
renameFile opts fn = do
  let simple_replace replacement = do
        logIOT Informational $ [fmt|checking %T against %w|] fn replacement
        replace replacement (toText fn) ≫ \ case
          𝕹   → logIOT Notice ([fmt|no replacement: %T|] fn) ⪼ return (fn, fn)
          𝕵 f → do (fn,) ⊳ FPath.Parseable.parse @AbsFile f

      log_regex_read f c = do
        n ← pResolve f
        res ← readRegexFile n
        logIOT Informational $ [fmt|checking %T against %s RE from %T|] fn c n
        return res

  logIO Debug def $ [fmtT|processing '%T'|] fn
  case opts ⊣ repl_opt of

    ROptRepl replacement → simple_replace replacement

    ROptREName n         → do
      -- XXX should allow -f here to mean "select regex from this file"
      regexen ← default_regexen_fn ≫ readRegexFile
      let no_such_re = [fmt|no such regex '%T' found in '%T'|] n fn
       in case n `Map.lookup` regexen of
            𝕵 replacement → simple_replace replacement
            𝕹             → throwError $ userE no_such_re

    ROptFile1 f          → do
      res ← log_regex_read f "1"
      replace1 (Map.toList res) (toText fn) ≫ \ case
        𝕹 → return (fn, fn)
        𝕵 (x,g) → do logIOT Debug $ [fmt|[1] %T → %t (%w)|] fn g x
                     (fn,) ⊳ FPath.Parseable.parse @AbsFile g

    ROptFileN f          → do
      res ← log_regex_read f "*"
      replaceMany (Map.toList res) (toText fn) ≫ \ case
        ([], _) → return (fn, fn)
        (xs,g) → do logIOT Debug $ [fmt|[*] %T → %t %w|] fn g xs
                    (fn,) ⊳ FPath.Parseable.parse @AbsFile g

----------------------------------------

default_regexen_fn ∷ ∀ ε μ .
                     (MonadIO μ, AsIOError ε, AsFPathError ε,
                      HasCallStack, MonadError ε μ) ⇒
                     μ AbsFile
default_regexen_fn =
  runEnv (envRcAbsFile "RENAME_REGEXEN" [relfile|.rename/default-regexen.txt|])

----------------------------------------

-- A note on -v messages:
--   -) x → y is issued on stdout unless --quiet is given
--   -) default (Warning)
--   -) (Notice) show non-renamed files
--   -) (Informational) show replacement checks about to happen
--   -) (Debug)
myMain ∷ ∀ ε .
         (HasCallStack, Printable ε, AsUsageError ε, AsIOError ε,AsParseError ε,
          AsProcExitError ε, AsCreateProcError ε, AsFPathError ε, AsREFnError ε,
          AsRepeatedKeyError 𝕋 ε, AsREGroupError ε) ⇒
         [𝕊] → DoMock → Options
       → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain args do_mock opts = do
  logIOT Debug $ [fmt|ARGS: %L|] args
  ins ∷ NonEmpty AbsFile ← sequence $ pResolve ⊳ opts ⊣ inputs

  input_errors ← checkInputFiles (toList ins)

  outputs ← filter (\ (i,o) → i ≢ o) ⊳ sequence (renameFile opts ⊳ ins)

  let outdirs   = nub $ [ f ⊣ dirname | (_,f) ← outputs ]
      d_exist d = lfexists' Informational FExists d NoMock
  outdir_es ∷ [(AbsDir,FExists)] ← forM outdirs $ \ d → (d,) ⊳ d_exist d
  let make_dirs = nub [ d | (d,e) ← outdir_es
                          , NoFExists ≡ e, MkDirs ≡ opts ⊣ mkdirs ]
  usage_errors ← checkOutputFiles (snd ⊳ outputs) make_dirs
                                  (opts ⊣ overwr)
  throwUsageErrors do_mock "not continuing with rename in presence of errors"
                   (input_errors ⊕ usage_errors)

  forM_ make_dirs (\ d → mkdir Notice d 0750 do_mock)
  forM_ outputs $ \ (infn,outfn) → when (infn ≢ outfn) $ do
    when (NoQuiet ≡ opts ⊣ quiet)$ liftIO (putStrLn $ [fmt|%T → %T|] infn outfn)
    rename Notice infn outfn do_mock

  return $ if outputs ≡ [] then 1 else 0

----------------------------------------

data RenameError = RE_UFPPIO_ERROR UsageParseFPProcIOError
                 | RE_REFNG_ERROR  REFnGroupError
                 | RE_RK_ERROR     (RepeatedKeyError 𝕋)

_RE_UFPPIO_ERROR ∷ Prism' RenameError UsageParseFPProcIOError
_RE_UFPPIO_ERROR = prism' (\ e → RE_UFPPIO_ERROR e)
                          (\ case RE_UFPPIO_ERROR e → 𝕵 e; _ → 𝕹)

_RE_REFNG_ERROR ∷ Prism' RenameError REFnGroupError
_RE_REFNG_ERROR = prism' (\ e → RE_REFNG_ERROR e)
                         (\ case RE_REFNG_ERROR e → 𝕵 e; _ → 𝕹)

_RE_RK_ERROR ∷ Prism' RenameError (RepeatedKeyError 𝕋)
_RE_RK_ERROR = prism' (\ e → RE_RK_ERROR e) (\ case RE_RK_ERROR e → 𝕵 e; _ → 𝕹)

----------------------------------------

instance Exception RenameError

--------------------

instance Show RenameError where
  show (RE_UFPPIO_ERROR e) = show e
  show (RE_REFNG_ERROR  e) = show e
  show (RE_RK_ERROR     e) = show e

--------------------

instance Printable RenameError where
  print (RE_UFPPIO_ERROR e) = print e
  print (RE_REFNG_ERROR  e) = print e
  print (RE_RK_ERROR     e) = print e

--------------------

instance HasCallstack RenameError where
  callstack =
    let
      getter (RE_UFPPIO_ERROR e) = e ⊣ callstack
      getter (RE_REFNG_ERROR  e) = e ⊣ callstack
      getter (RE_RK_ERROR     e) = e ⊣ callstack
      setter (RE_UFPPIO_ERROR e) cs =
        RE_UFPPIO_ERROR (e & callstack ⊢ cs)
      setter (RE_REFNG_ERROR  e) cs =
        RE_REFNG_ERROR (e & callstack ⊢ cs)
      setter (RE_RK_ERROR     e) cs =
        RE_RK_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

----------------------------------------

instance AsREGroupError RenameError where
  _REGroupError = _RE_REFNG_ERROR ∘ _REGroupError

instance AsREFnError RenameError where
  _REFnError = _RE_REFNG_ERROR ∘ _REFnError

instance AsFPathError RenameError where
  _FPathError  = _RE_UFPPIO_ERROR ∘ _FPathError

instance AsCreateProcError RenameError where
  _CreateProcError  = _RE_UFPPIO_ERROR ∘ _CreateProcError

instance AsIOError RenameError where
  _IOError = _RE_UFPPIO_ERROR ∘ _IOError

instance AsProcExitError RenameError where
  _ProcExitError  = _RE_UFPPIO_ERROR ∘ _ProcExitError

instance AsParseError RenameError where
  _ParseError = _RE_UFPPIO_ERROR ∘ _ParseError

instance AsUsageError RenameError where
  _UsageError  = _RE_UFPPIO_ERROR ∘ _UsageError

instance AsRepeatedKeyError 𝕋 RenameError where
  _RepeatedKeyError  = _RE_RK_ERROR ∘ _RepeatedKeyError

------------------------------------------------------------

main ∷ IO ()
main = do
  let progDesc = unlines [ "rename files per a regex."
                         , "Note that the regex always matches against an"
                         , "absolute filename; but only the matched portion is"
                         , "replaced.  Therefore it is common to use a positive"
                         , "look-behind assertion such as (?<=/) to anchor the"
                         , "match to the beginning of a directory; or even"
                         , "(?<=/)(?=[^/]+$) to match only the filename portion"
                         , "of each file."
                         ]
  getArgs ≫ (\ args → stdMain progDesc parseOptions (myMain @RenameError args) args)

--------------------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

re1 ∷ RE
re1 = [re|${iggy}(fo+)${pop}(.ar)|]
re3 ∷ RE
re3 = [re|(?<=/)foo\.*(.{3})|]

repl0 ∷ ReplText -- ">>${pop}<< (${1}) [${0}]"
repl0 = [repltext|>>${pop}<< (${1}) [${0}]|]

repl3 ∷ ReplText
𝕽 repl3 = parsec @ReplText @Parsec.Error.ParseError
                 ("repl3"∷𝕋) ("quux.${.title 1}/"∷𝕋)

rep1_0 ∷ REPlacement -- s/${iggy}(fo+)${pop}(.ar)/>>${pop}<< (${1}) [${0}]/
rep1_0 = REPlacement re1 repl0

rep3_3 ∷ REPlacement -- s/(?<=/)foo\.*(.{3})$/quux.${.title 1}\//
rep3_3 = REPlacement re3 repl3

----------------------------------------

tests ∷ TestTree
tests = testGroup "parseReplacementText" [ parseRegexLineTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
