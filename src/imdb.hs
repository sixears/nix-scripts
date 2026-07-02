-- IMDB to Obsidian Haskell Script
-- Uses ImageMagick (`magick`) for image resizing via command line

-- XXX add logging timestamps

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import Base1

-- putStrLn => log, or error?
import Prelude  ( Double, div, error, filter, map, mod, null, toEnum, truncate )

import qualified Data.ByteString      as BSS
import qualified Network.HTTP.Simple  as HTTP
import qualified System.Directory     as Dir
import qualified Control.Monad        as Monad
import qualified Data.Yaml            as Yaml
import qualified Data.Text            as T

-- aeson -------------------------------

import qualified Data.Aeson           as Aeson

import Data.Aeson        ( FromJSON, ToJSON, (.:), (.:?), defaultOptions,
                           fieldLabelModifier, omitNothingFields, genericToJSON,
                           withObject, withScientific, withText
                         )
import Data.Aeson.Types  ( Object, parseFail )

-- base --------------------------------

import Data.List     ( any, dropWhileEnd, nub, span )
import Data.Maybe    ( listToMaybe )
import GHC.Generics  ( Generic )
import System.IO     ( Handle, SeekMode( AbsoluteSeek ), hFileSize, hSeek )
import Text.Read     ( read )

-- bytestring --------------------------

import Data.ByteString  ( ByteString, hGet, uncons )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadCatch )

-- extra -------------------------------

import Data.List.Extra  ( takeWhileEnd )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Dirname           ( dirname )
import FPath.Dir               ( DirAs )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( FileAs )
import FPath.FileLike          ( (⊙) )
import FPath.Parseable         ( __parse__ )
import FPath.PathComponent     ( PathComponent, pc )
import FPath.RelDir            ( reldir )

-- fstat -------------------------------

import FStat  ( FileType( Directory ), ftype )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log, errT, infoT, noticeT, warnT )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Debug, Informational ) )

-- mockio-log --------------------------

import MockIO.IOClass      ( HasIOClass )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Directory          ( mkdir )
import MockIO.DoMock             ( DoMock( NoMock ), HasDoMock( doMock ) )
import MockIO.File               ( FExists( FExists, NoFExists ), fexists, unlink )
import MockIO.FStat              ( stat )
import MockIO.OpenFile           ( FileOpenMode( FileR ), HEncoding( Binary ),
                                   appendFile, readFile, withFile, writeFile )
import MockIO.Process            ( ꙩ )
import MockIO.Process.MLCmdSpec  ( ToMLCmdSpec )

-- monaderror-io -----------------------

import MonadError           ( eitherME )
import MonadError.IO.Error  ( throwUserError )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.FPath                  ( getCwd )
import MonadIO.NamedHandle            ( HasNamedHandle, handle )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- modern-uri --------------------------

import Text.URI       ( QueryParam( QueryParam ), RText, RTextLabel( PathPiece ), URI,
                        mkPathPiece, mkURI, render, renderStr )
import Text.URI.Lens  ( uriPath, uriQuery )
import Text.URI.QQ    ( pathPiece, queryKey, queryValue, uri )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks, runReaderT )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNEConversions  ( FromSeqNonEmpty, fromSeqNE )

-- parser-plus -------------------------

import ParserPlus  ( digits )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, help, long, metavar, short )

-- optparse-plus -----------------------

import OptParsePlus  ( textualArgument, textualOption )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, anyChar, string )
import Text.Parser.Combinators  ( choice, optional )

-- scientific --------------------------

import Data.Scientific  ( isInteger, toRealFloat )

-- stdmain -----------------------------

import StdMain  ( stdMainSimple )

-- text --------------------------------

import Data.Text                 ( breakOn, dropWhile, isInfixOf, isPrefixOf, unlines )
import Data.Text.Encoding        ( encodeUtf8 )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- yaml --------------------------------

import Data.Yaml  ( decodeEither', encode )

--------------------------------------------------------------------------------

newtype Year = Year { unYear ∷ Word8 } deriving Show -- offset from 1900

--------------------

instance Printable Year where
  print = P.text ∘ [fmt|%d|] ∘ (1900+) ∘ fromIntegral @_ @Word16 ∘ unYear

--------------------

instance ToJSON Year where toJSON = Aeson.String ∘ toText

--------------------

instance FromJSON Year where
  parseJSON = withScientific "Year" $ \ s →
    if isInteger s
    then let y ∷ Word16 = truncate s
         in if y < 1900
       -- XXX have fmt handle Scientifics
            then parseFail $ [fmt|year '%T' < 1900|] (toRealFloat @Double s)
            else if y > 2155
                 then parseFail $ [fmt|year '%T' > 2155|] (toRealFloat @Double s)
                 else pure $ Year (fromIntegral $ y-1900)
    else parseFail $ [fmt|year '%T' is not an integer|] (toRealFloat @Double s)

------------------------------------------------------------

-- | Data types for IMDB responses

data TitleResponse = TitleResponse { _trTitle       ∷ Title
                                   , startYear      ∷ 𝕄 Year
                                   , runtimeSeconds ∷ 𝕄 Word16
                                   , plot           ∷ 𝕄 𝕋
                                   , interests      ∷ 𝕄 [Interest]
                                   , stars          ∷ 𝕄 [IMDBPerson]
                                   , directors      ∷ 𝕄 [IMDBPerson]
                                   }
  deriving Show

----------

instance FromJSON TitleResponse where
  parseJSON = withObject "TitleResponse" $ \ v → do
    TitleResponse ⊳ v .:  "primaryTitle"
                  ⊵ v .:? "startYear"
                  ⊵ v .:? "runtimeSeconds"
                  ⊵ v .:? "plot"
                  ⊵ v .:? "interests"
                  ⊵ v .:? "stars"
                  ⊵ v .:? "directors"

--------------------

instance HasTitle TitleResponse where
  title = lens _trTitle (\ tr t → tr { _trTitle = t })

------------------------------------------------------------

newtype Interest = Interest { interestName ∷ 𝕋 } deriving Show

--------------------

instance FromJSON Interest where
  parseJSON = withObject "Interest" $ \ v → Interest <$> v .: "name"

------------------------------------------------------------

newtype IMDBPerson = IMDBPerson { displayName ∷ 𝕋 } deriving Show

--------------------

instance FromJSON IMDBPerson where
  parseJSON = withObject "Person" $ \ v → IMDBPerson <$> v .: "displayName"

------------------------------------------------------------

newtype Rating = Rating { unRating ∷ 𝕋 }  deriving Show

--------------------

instance Printable Rating where
  print = P.text ∘ unRating

--------------------

instance FromJSON Rating where
  -- we parse everything here, there's too many different ratings in different
  -- countries; but at least it's its own type
  parseJSON = withText "Rating" $ pure ∘ Rating

--------------------

instance ToJSON Rating where
  toJSON (Rating t) = Aeson.String t

------------------------------------------------------------

class HasCountry α where
  country ∷ Lens' α Country

------------------------------------------------------------

class HasRating α where
  rating ∷ Lens' α Rating

------------------------------------------------------------

data Certificate = Certificate { _country ∷ Country, _rating ∷ Rating }
  deriving Show

----------

instance HasCountry Certificate where
  country = lens _country (\ c y → c { _country = y })

----------

instance HasRating Certificate where
  rating = lens _rating (\ c r → c { _rating = r })

------------------------------------------------------------

class HasCertificates α where
  certificates ∷ Lens' α [Certificate]

------------------------------------------------------------

newtype CertificateResponse = CertificateResponse { _certificates ∷ [Certificate]}
  deriving Show

--------------------

instance HasCertificates CertificateResponse where
  certificates = lens _certificates (\ _ cs → CertificateResponse cs)

--------------------

instance FromJSON CertificateResponse where
  parseJSON =
    withObject "CertificateResponse" $ \ v →
      CertificateResponse <$> v .: "certificates"

------------------------------------------------------------

instance FromJSON Certificate where
  parseJSON =
    withObject "Certificate" $ \ v →
      Certificate <$> v .: "country" <*> v .: "rating"

------------------------------------------------------------

class HasCountryCode α where
  countryCode ∷ Lens' α 𝕋

------------------------------------------------------------

newtype Country = Country { _code ∷ 𝕋 } deriving Show

----------

instance HasCountryCode Country where
  countryCode = lens _code (\ _ c → Country { _code = c })

--------------------

instance FromJSON Country where
  parseJSON = withObject "Country" $ \ v → Country <$> v .: "code"

------------------------------------------------------------

newtype ImageResponse = ImageResponse { images ∷ [Image] } deriving Show

--------------------

instance FromJSON ImageResponse where
  parseJSON = withObject "ImageResponse" $ \ v → ImageResponse <$> v .: "images"

------------------------------------------------------------

class HasQueryParams α where
  queryParams ∷ Lens' α [QueryParam]

------------------------------------------------------------

newtype MyURI = MyURI { unMyURI ∷ URI } deriving Show

myURI ∷ Lens' MyURI URI
myURI = lens unMyURI (\ _ u → MyURI u)

instance FromJSON MyURI where
  parseJSON = withText "MyURI" $ \ t → either (parseFail ∘ show) (pure ∘ MyURI) $ mkURI t

instance Printable MyURI where
  print = P.text ∘ render ∘ unMyURI

instance HasQueryParams MyURI where
  queryParams = myURI ∘ uriQuery

------------------------------------------------------------

data Image = Image { imageType ∷ 𝕋, url ∷ MyURI } deriving Show

instance FromJSON Image where
  parseJSON = withObject "Image" $ \ v → Image <$> v .: "type" <*> (v .: "url")

------------------------------------------------------------

newtype MDInternalLink = MDInternalLink { unMDInternalLink ∷ PathComponent }

instance ToJSON MDInternalLink where
  toJSON = Aeson.String ⊳ mdInternalLink ∘ unMDInternalLink

------------------------------------------------------------

newtype Duration = Duration { _durationInSeconds ∷ Word16 }

------------------------------------------------------------

newtype Title = Title { _unTitle ∷ 𝕋 }  deriving  (FromJSON, Show, ToJSON)

----------

class HasTitle α where
  title ∷ Lens' α Title

----------

instance Printable Title where print = P.text ∘ _unTitle

------------------------------------------------------------

data FrontMatter = FrontMatter { imdb          ∷ IMDB_ID
                               , _fmTitle      ∷ Title
                               , cover         ∷ MDInternalLink
                               , ukCertificate ∷ 𝕄 Rating
                               , summary       ∷ 𝕋
                               , year          ∷ 𝕄 Year
                               , duration      ∷ 𝕋
                               , interests'    ∷ 𝕄 [𝕋]
                               , stars'        ∷ 𝕄 [𝕋]
                               , directors'    ∷ 𝕄 [𝕋]
                               }
  deriving Generic

----------

instance ToJSON FrontMatter where
  toJSON = let modifier "ukCertificate" = "UK Certificate"
               modifier "_fmTitle" = "title"
               modifier t = dropWhileEnd (≡'\'') t
           in  genericToJSON defaultOptions { fieldLabelModifier = modifier
                                            , omitNothingFields = 𝓣 }

----------

instance Printable FrontMatter where
  print = P.utf8 ∘ encode

--------------------

instance HasTitle FrontMatter where
  title = lens _fmTitle (\ fm t → fm { _fmTitle = t })

------------------------------------------------------------

-- | Person type for family members
data Person = Abi | Xander | JJ | Mum  deriving  (Show, Eq)

--------------------

instance Printable Person where print = P.string ∘ show

--------------------

instance Textual Person where
  textual = choice [ string "Mum"       ⋫ pure Mum
                   , string "mum"       ⋫ pure Mum
                   , string "Heather"   ⋫ pure Mum
                   , string "heather"   ⋫ pure Mum
                   , string "Hx"        ⋫ pure Mum
                   , string "hx"        ⋫ pure Mum
                   , string "Abigail"   ⋫ pure Abi
                   , string "abigail"   ⋫ pure Abi
                   , string "Abi"       ⋫ pure Abi
                   , string "abi"       ⋫ pure Abi
                   , string "Ax"        ⋫ pure Abi
                   , string "ax"        ⋫ pure Abi
                   , string "Alexander" ⋫ pure Xander
                   , string "alexander" ⋫ pure Xander
                   , string "Xander"    ⋫ pure Xander
                   , string "xander"    ⋫ pure Xander
                   , string "Xax"       ⋫ pure Xander
                   , string "xax"       ⋫ pure Xander
                   , string "Jonathan"  ⋫ pure JJ
                   , string "jonathan"  ⋫ pure JJ
                   , string "JJ"        ⋫ pure JJ
                   , string "jj"        ⋫ pure JJ
                   ]


--------------------

-- | Get the display name for a person
personName ∷ Person → 𝕋
personName Abi    = "Abi"
personName Xander = "Xander"
personName JJ     = "JJ"
personName Mum    = "Mum"

--------------------

personComponent ∷ Person → PathComponent
personComponent = __parse__ ∘ personName

--------------------

-- | Get the prefix for a person
personPrefix ∷ Person → 𝕋
personPrefix Abi    = "ax"
personPrefix Xander = "xax"
personPrefix JJ     = "jj"
personPrefix Mum    = "hx"

--------------------

-- | Parse a string to a Person
parsePerson ∷ 𝕋 → 𝕄 Person
parsePerson "Abi"    = 𝓙 Abi
parsePerson "Xander" = 𝓙 Xander
parsePerson "JJ"     = 𝓙 JJ
parsePerson "Mum"    = 𝓙 Mum
parsePerson _        = 𝓝

------------------------------------------------------------

class ToPathPiece α where
  toPathPiece ∷ α → RText 'PathPiece
  ҩ ∷ α → RText 'PathPiece
  ҩ = toPathPiece

instance ToPathPiece (RText 'PathPiece) where toPathPiece = id

------------------------------------------------------------

newtype IMDB_ID = IMDB_ID ℕ  deriving  Show

--------------------

instance Printable IMDB_ID where
  print (IMDB_ID i) = P.text $ [fmt|tt%07d|] i

--------------------

instance ToJSON IMDB_ID where
  toJSON = Aeson.String ∘ toText

--------------------

instance Textual IMDB_ID where
  -- this is so that, we can parse, e.g.,
  -- https://www.imdb.com/title/tt20234774/parentalguide/?ref_=tt_ov_pg#certificates
  -- on the cmdline
  textual =
    IMDB_ID ⊳ (read ⊳ (optional imdbTitlePrefix ⋫ string "tt" ⋫ digits ⋪ many anyChar))

--------------------

instance ToPathPiece IMDB_ID where
  toPathPiece  = either (error ∘ show) id ∘ mkPathPiece ∘ toText

------------------------------------------------------------

-- | Command line options
data Options = Options { tts :: [IMDB_ID], people :: [Person], seen :: [Person] }
  deriving Show

----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ some (textualArgument (metavar "IMDB ID"))
          ⊵ nub ⊳ many (textualOption (ю [ short 'w', long "wants", long "want"
                                          , help "wants to see" ]))
          ⊵ nub ⊳ many (textualOption (ю [ short 'h', long "has-seen", long "seen"
                                          , help "has seen" ]))

------------------------------------------------------------

{-| things we can append to a URI -}
class UAppend α where
  uAppend ∷ MyURI → α → MyURI
  (‡) ∷ MyURI → α → MyURI
  (‡) = uAppend

instance UAppend [RText 'PathPiece] where
  uAppend uri_ pieces = uri_ & (myURI ∘ uriPath) ⊧ (◇ pieces)

instance UAppend (RText 'PathPiece) where
  uAppend uri_ piece = uAppend uri_ [piece]

------------------------------------------------------------

fromPC ∷ (Element α ~ PathComponent, FromSeqNonEmpty α) => PathComponent → α
fromPC = fromSeqNE ∘ pure

----------------------------------------

-- | IMDB API base URL
imdbApiBase ∷ MyURI
imdbApiBase = MyURI [uri|https://api.imdbapi.dev/titles|]

-- | IMDB common interactive lookup prefix
imdbTitlePrefix ∷ CharParsing η => η 𝕊
imdbTitlePrefix = string "https://www.imdb.com/title/"

----------------------------------------

parseRequest ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) => MyURI → μ HTTP.Request
parseRequest = eitherME (userE ∘ show) ∘ HTTP.parseRequest ∘ renderStr ∘ unMyURI

----------------------------------------

fetchResponse ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) => MyURI → μ ByteString
fetchResponse uri_ = do
  response ← parseRequest uri_ ≫ HTTP.httpBS
  let status = HTTP.getResponseStatusCode response
  if status == 200
  then return $ HTTP.getResponseBody response
  else throwUserError $ "HTTP error: " ◇ show status

----------------------------------------

fetchJSON ∷ ∀ ε a μ . (MonadIO μ, AsIOError ε, MonadError ε μ, FromJSON a) =>
            MyURI → μ (𝕄 a)
fetchJSON uri_ = do
  (Aeson.eitherDecode ∘ BSS.fromStrict) ⊳ fetchResponse uri_ ≫ \ case
    𝓛 err    → throwUserError $ "Error decoding JSON: " ◇ err
    𝓡 result → return $ 𝓙 result

----------------------------------------

-- | Sanitize title for filename
titleFilename ∷ Title → 𝕄 Year → PathComponent
titleFilename ttle year =
  let name = case breakOn " " $ T.replace "/" "-" $ T.replace ":" "-" (toText ttle) of
               ("The", rest) → dropWhile (≡' ') rest ◇ "," ◇ "The"
               ("A",   rest) → dropWhile (≡' ') rest ◇ "," ◇ "A"
               ("An",  rest) → dropWhile (≡' ') rest ◇ "," ◇ "An"
               (ini,   rest) → ini ◇ rest
      year_text = "" ⧐ ([fmt|  (%T)|] ⊳ year)
  in  __parse__ $ name ◇ year_text

----------------------------------------

-- | Format duration from seconds
formatDuration ∷ 𝕄 Word16 → 𝕋
formatDuration (𝓙 seconds) =
  let hours = seconds `div` 3600
      minutes = (seconds `mod` 3600) `div` 60
  in T.pack $ show hours ◇ "h" ◇ show minutes ◇ "m"
formatDuration 𝓝 = "N/A"

----------------------------------------

{-| execute an external process, don't redirect out/err, nothing on stdin, expect 0 exit -}
ꙭ ∷ ∀ ε δ α μ . (MonadIO μ, ToMLCmdSpec α (), MonadLog (Log MockIOClass) μ,
                 MonadReader δ μ, HasDoMock δ,
                 AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
                 Printable ε, MonadError ε μ) =>
    α → μ ()

ꙭ = snd ⩺ ꙩ

----------------------------------------

-- | Download and resize an image using ImageMagick's `magick` command
-- downloadAndResizeImage ∷ 𝕋 → FilePath → IO ()
downloadAndResizeImage ∷ ∀ ε ρ μ .
                         (MonadIO μ, HasDoMock ρ, MonadReader ρ μ,
                          MonadLog (Log MockIOClass) μ,
                          AsFPathError ε, AsIOError ε,AsCreateProcError ε,AsProcExitError ε,
                          Printable ε, MonadError ε μ) =>
                         MyURI → AbsFile → μ ()

downloadAndResizeImage image_uri target_path = do
  do_mock ← asks (view doMock)
  -- Download the image to a temporary file
  let temp_file_path = target_path ⊙ [pc|tmp|]
  body ← fetchResponse image_uri
  writeFile Informational 𝓝 (𝓙 0o644) temp_file_path body do_mock

  -- Use ImageMagick to resize the image
  ꙭ ([absfile|/run/current-system/sw/bin/magick|],
     [toText temp_file_path, "-resize", "600x400>", toText target_path])

  -- Remove the temporary file
  unlink Informational temp_file_path do_mock

----------------------------------------

writeMD ∷ ∀ ε α ρ μ .
          (MonadIO μ, Printable α, MonadLog (Log MockIOClass) μ, MonadCatch μ,
           HasDoMock ρ, MonadReader ρ μ, AsIOError ε, Printable ε, MonadError ε μ) =>
          𝕄 α → [𝕋] → AbsFile → μ ()
writeMD yaml_m lines fn =
  let content = maybe "" [fmtT|---\n%T---\n|] yaml_m ◇ unlines lines
  in  asks (view doMock) ≫ writeFile Informational 𝓝 (𝓙 0o644) fn content

----------------------------------------

mdInternalLink ∷ PathComponent → 𝕋
mdInternalLink = [fmt|[[%T]]|]

----------------------------------------

writeMovie ∷ ∀ ε ρ μ .
             (MonadIO μ, MonadLog (Log MockIOClass) μ, MonadCatch μ,
              HasDoMock ρ, MonadReader ρ μ, AsIOError ε, Printable ε, MonadError ε μ) =>
             IMDB_ID → PathComponent → 𝕄 Rating → TitleResponse → AbsFile → μ ()
writeMovie tt sanitized_title uk_cert title_response fn = do
  let fm = FrontMatter
        { imdb          = tt
        , _fmTitle      = title_response ⊣ title
        , cover         = MDInternalLink $ sanitized_title ⊙ [pc|jpg|]
        , ukCertificate = uk_cert
        , summary       = ""    ⧐ plot title_response
        , year          = startYear title_response
        , duration      = formatDuration (runtimeSeconds title_response)
        , interests'    = map interestName ⊳ interests title_response
        , stars'        = map displayName  ⊳ stars     title_response
        , directors'    = map displayName  ⊳ directors title_response
        }

  writeMD (𝓙 fm) [] fn

----------------------------------------

readLastByte ∷ Handle → IO (𝕄 Word8)
readLastByte h = do
  size ← hFileSize h
  if size ≤ 0
  then return 𝓝
  else do hSeek h AbsoluteSeek (size - 1)
          bs ← hGet h 1
          return $ fst ⊳ uncons bs

--------------------

readLastByte' ∷ ∀ ε δ μ . (MonadIO μ, HasNamedHandle δ, AsIOError ε, MonadError ε μ) =>
                δ → μ (𝕄 ℂ)
readLastByte' h = asIOError $ (toEnum ∘ fromIntegral) ⊳⊳ readLastByte (h ⊣ handle)

--------------------

getLastByte ∷ ∀ ε γ ω μ . (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, Printable ε,
                           HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) =>
              γ → μ (𝕄 ℂ)
getLastByte fn = withFile Informational 𝓝 Binary FileR (return 𝓝) fn readLastByte' NoMock

----------------------------------------

{-| Ensure that a file ends in a newline, *if it exists and is a file*.
    If it doesn't exist: do nothing.
    If it exists, but is empty: do nothing.
    If it is not a file (or appending does not work): error.
-}
ensureTrailingNewline ∷ ∀ ε γ ρ ω μ .
                        (MonadIO μ, FileAs γ, AsIOError ε, Printable ε, MonadError ε μ,
                         HasDoMock ρ, MonadReader ρ μ,
                         HasDoMock ω,HasIOClass ω,Default ω,MonadLog (Log ω) μ) =>
                        γ → μ ()
ensureTrailingNewline fn = do
  do_mock ← asks (view doMock)
  fexists Debug FExists fn NoMock ≫ \ case
    NoFExists → return ()
    FExists → getLastByte fn ≫ \ case
      𝓝      → return ()
      𝓙 '\n' → return ()
      𝓙 _    → appendFile Informational 𝓝 𝓝 fn ("\n"∷𝕋) do_mock

----------------------------------------

{-| Parse a pandoc-markdown file, that is, a file with an optional set of YAML attributes
    at the top.

    If the file is prefixed with a "---" line, then the top is parsed as YAML, until any
    latter "---" line.  The second "---", and any subsequent text, is returned as lines.(&&)

    If the file is not prefixed with a "---" line, then it's all returned as lines.
-}
parseMD ∷ ∀ ε γ ω μ . (MonadIO μ, FileAs γ, AsIOError ε, Printable ε, MonadError ε μ,
                       HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) =>
          γ → μ (𝕄 Object, [𝕋])
parseMD fn = do
  readFile Informational 𝓝 (return []) fn NoMock ≫ \ case
    ("---" : xs) → let (header, rest) = span (≢ "---") xs
                   in  case decodeEither' @Object (encodeUtf8 $ unlines header) of
                         𝓡 o → return (𝓙 o,rest)
                         𝓛 e → throwUserError $
                                 [fmtT|failed to decode header in %T (%w)|] fn e
    xs           → return (𝓝, xs)

----------------------------------------

{-| A bit like `mkpath`, but only one level.  Ensure that a given directory exists,
    creating it if necessary (but not creating parents). -}
ensureDir ∷ ∀ ε δ ρ ω μ . (MonadIO μ, DirAs δ, AsIOError ε, Printable ε, MonadError ε μ,
                           HasDoMock ρ, MonadReader ρ μ,
                           MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) =>
            δ → μ ()
ensureDir d = do
  do_mock ← asks (view doMock)
  ftype ⊳⊳ stat Informational 𝓝 d NoMock ≫ \ case
    𝓝           → mkdir Informational d 0o755 do_mock
    𝓙 Directory → return ()
    𝓙 ft        → throwUserError ([fmtT|not a directory: %T (got a %w)|] d ft)

----------------------------------------

appendText ∷ ∀ ε ρ ω μ . (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                          HasDoMock ρ, MonadReader ρ μ,
                          Default ω,HasDoMock ω,HasIOClass ω, MonadLog (Log ω) μ) =>
             AbsFile → 𝕋 → μ ()
appendText file_name text =
  asks (view doMock) ≫ appendFile Informational 𝓝 (𝓙 0o644) file_name text

----------------------------------------

{-| Ensure that a given wiki link is present in a file, adding it at the end if necessary
    (see `appendInternalLink`); creating the file if it doesn't exist. -}
ensureInternalLink ∷ ∀ ε ρ ω μ . (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                                  HasDoMock ρ, MonadReader ρ μ,
                                  Default ω,HasDoMock ω,HasIOClass ω, MonadLog (Log ω) μ) =>
                     PathComponent → AbsFile → μ ()
ensureInternalLink link fn = do
  ensureDir (fn ⊣ dirname)
  (_, lines) ← parseMD fn
  let text = mdInternalLink link
  case filter (text `isInfixOf`) lines of
    (_:_) → return ()
    []    → do
      let last_para = takeWhileEnd (≢"") lines
      ensureTrailingNewline fn
      when (any ("#" `isPrefixOf`) last_para) $ appendText fn "\n"
      appendText fn $ text ◇ "\n"

----------------------------------------

gbCert ∷ HasCertificates α => α → 𝕄 Rating
gbCert response =
  let filtGB = filter $ (≡"GB") ∘ view (country ∘ countryCode)
  in  listToMaybe $ map (view rating) $ filtGB (response ⊣ certificates)

----------------------------------------

gbRating ∷ ∀ ε α ω μ . (MonadIO μ, ToPathPiece α, MonadLog (Log ω) μ, Default ω,
                        AsIOError ε, MonadError ε μ) =>
           α → Title → μ (𝕄 Rating)

gbRating tt ttle = do
  let certificate_url = imdbApiBase ‡ [toPathPiece tt, [pathPiece|certificates|]]
  let fetch_certificate_response = fetchJSON @_ @CertificateResponse
  gb_rating ← fetch_certificate_response certificate_url ⊲ (join ∘ (gbCert ⊳))
  let cert_notice ∷ 𝕄 Rating → Title → 𝕋
      cert_notice = maybe [fmt|No UK Certificate found for %T|]
                          [fmt|UK Certificate '%T' for %T|]
  noticeT $ cert_notice gb_rating ttle
  return gb_rating

----------------------------------------

fetchImages ∷ ∀ ε α ρ μ .
              (MonadIO μ, MonadLog (Log MockIOClass) μ, ToPathPiece α,
               AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
               Printable ε, MonadError ε μ, HasDoMock ρ, MonadReader ρ μ) =>
              α → AbsFile → μ ()

fetchImages tt image_target_path = do
  let uri' = imdbApiBase ‡ [toPathPiece tt, [pathPiece|images|]] & queryParams ⊢ params
               where params = [QueryParam [queryKey|types|] [queryValue|poster|]]
  fetchJSON uri' ≫ \ case
    𝓙 imageResponse → do
      let posterImages = images imageResponse
      if null posterImages
        then infoT "No images found"
        else do
          infoT $ [fmt|Writing %T…|] image_target_path
          case head posterImages of
            𝓝    → infoT "no image found"
            𝓙 pI → downloadAndResizeImage (url pI) image_target_path
    𝓝 → warnT "Failed to fetch images"

----------------------------------------

-- | Process a single title
-- processTitle ∷ 𝕋 → Options → IO ()
processTitle ∷ ∀ ε ρ μ .
               (MonadIO μ, MonadLog (Log MockIOClass) μ, MonadCatch μ,
                HasDoMock ρ, MonadReader ρ μ,
                AsFPathError ε, AsIOError ε, AsCreateProcError ε, AsProcExitError ε, Printable ε, MonadError ε μ) =>
               AbsDir → Options → IMDB_ID → μ ()
processTitle info_dir opts tt = do
  let title_uri = imdbApiBase ‡ ҩ tt
  infoT $ [fmt|trying uri: %T|] title_uri
  maybeTitleResponse ← fetchJSON title_uri
  case maybeTitleResponse of
    𝓝 → errT $ [fmt|Failed to fetch title: %T|] tt
    𝓙 title_response → do
      let ttle              = title_response ⊣ title
          file_title        = titleFilename ttle (startYear title_response)
          movies_dir        = info_dir ⫻ [reldir|movies/|]
          md_fname          = fromPC (file_title ⊙ [pc|md|])
          jpg_fname         = fromPC (file_title ⊙ [pc|jpg|])
          target_path       = movies_dir ⫻ md_fname
          attachment_dir    = movies_dir ⫻ [reldir|_attachments/|]
          image_target_path = attachment_dir ⫻ jpg_fname
      infoT $ [fmt|Fetched title (%T): %T|] tt ttle
      -- check if the file already exists
         -- XXX use something better than Dir, e.g., MockIO
      exists ← liftIO $ Dir.doesFileExist (toString target_path)
      if exists
        then infoT $ [fmt|Already exists: %T (%T)|] target_path tt
        else do
          infoT $ [fmt|Found title: %T|] ttle

          -- Create attachments directory if it doesn't exist
          -- XXX use something better than Dir, e.g., MockIO
          liftIO $ Dir.createDirectoryIfMissing 𝓣 (toString attachment_dir)

          -- Fetch and process images
          fetchImages tt image_target_path

          -- Fetch certificate
          gb_rating ← gbRating tt ttle
          writeMovie tt file_title gb_rating title_response target_path

          let people_dir     = info_dir ⫻ [reldir|people/|]
              person_dir p   = people_dir ⫻ fromList [personComponent p]
              person_fn bf p = person_dir p ⫻ __parse__ (bf (personPrefix p))

          forM_ (people opts) $
            ensureInternalLink file_title ∘ person_fn [fmtT|%t-wants-to-see.md|]

          forM_ (seen opts) $
            ensureInternalLink file_title ∘ person_fn [fmtT|%t-has-seen.md|]

----------------------------------------

doMain ∷ ∀ ε μ .
         (MonadIO μ, MonadLog (Log MockIOClass) μ, MonadCatch μ,
          AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε, Printable ε,
          MonadError ε μ) =>
         DoMock → Options → μ ()
doMain do_mock opts = do
  cwd ← getCwd
  if null (tts opts)
  then throwUserError @_ @𝕋 "no titles provided"
  else do
    -- Check if movies directory exists
    moviesDirExists ← liftIO $ Dir.doesDirectoryExist "movies"
    if not moviesDirExists
      then throwUserError @_ @𝕋 "run this in an obsidian movies-info dir"
      else Monad.forM_ (tts opts) $ flip runReaderT do_mock ∘ processTitle cwd opts

----------------------------------------

main ∷ IO ()
main = let progDesc ∷ 𝕋 = "add a new film to the obsidian movies library"
       in  stdMainSimple progDesc parseOptions doMain

-- that's all, folks! ----------------------------------------------------------
