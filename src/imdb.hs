-- IMDB to Obsidian Haskell Script
-- Uses ImageMagick (`magick`) for image resizing via command line

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
import Prelude  ( FilePath, Int, div, error, filter, map, mod, null, putStrLn )

import qualified Data.ByteString      as BSS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO         as TIO
import qualified Network.HTTP.Simple  as HTTP
import qualified System.Directory     as Dir
import qualified System.FilePath      as FP
import qualified System.Process       as Proc
import qualified Data.Maybe           as Maybe
import qualified Control.Monad        as Monad
import qualified System.Exit          as Exit
import qualified Data.Yaml            as Yaml
import qualified Data.Text            as T

-- aeson -------------------------------

import qualified Data.Aeson           as Aeson

import Data.Aeson        ( FromJSON, ToJSON, (.:), (.:?), defaultOptions,fieldLabelModifier,
                           omitNothingFields, genericToJSON, withObject, withText )
import Data.Aeson.Types  ( parseFail )

-- base --------------------------------

import Data.List     ( dropWhileEnd, nub )
import GHC.Generics  ( Generic )
import Text.Read     ( read )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.FileLike          ( (⊙) )
import FPath.Parseable         ( __parse__ )
import FPath.PathComponent     ( PathComponent, pc )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( RelFile )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational ) )

-- mockio ------------------------------

import MockIO.DoMock  ( HasDoMock( doMock ) )

-- mockio-log --------------------------

import MockIO.Log          ( HasDoMock, mkIOLMER )
import MockIO.IOClass      ( HasIOClass, IOClass( IORead, IOWrite ) )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.DoMock             ( DoMock )
import MockIO.File               ( unlink )
import MockIO.OpenFile           ( writeFile )
import MockIO.Process            ( ꙩ )
import MockIO.Process.MLCmdSpec  ( ToMLCmdSpec )

-- monaderror-io -----------------------

import MonadError           ( eitherME, fromRight )
import MonadError.IO.Error  ( IOError, throwUserError )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.FPath                  ( getCwd )
import MonadIO.Error.ProcExitError    ( AsProcExitError )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- modern-uri --------------------------

import Text.URI       ( RText, RTextLabel( PathPiece ), URI,
                        mkPathPiece, mkURI, render, renderStr )
import Text.URI.Lens  ( uriPath )
import Text.URI.QQ    ( pathPiece, uri )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, ask, asks, runReaderT )

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

-- stdmain -----------------------------

import StdMain             ( stdMainSimple )
import StdMain.UsageError  ( UsageParseFPProcIOError )

-- text --------------------------------

import Data.Text                 ( breakOn, dropWhile )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

-- | IMDB API base URL
imdbApiBase ∷ URI
imdbApiBase = [uri|https://api.imdbapi.dev/titles|]

-- | IMDB common interactive lookup prefix
imdbTitlePrefix ∷ CharParsing η => η 𝕊
imdbTitlePrefix = string "https://www.imdb.com/title/"

------------------------------------------------------------

-- | Data types for IMDB responses

data TitleResponse = TitleResponse { primaryTitle   ∷ 𝕋
                                   , startYear      ∷ 𝕄 Int
                                   , runtimeSeconds ∷ 𝕄 Int
                                   , plot           ∷ 𝕄 𝕋
                                   , interests      ∷ 𝕄 [Interest]
                                   , stars          ∷ 𝕄 [Person']
                                   , directors      ∷ 𝕄 [Person']
                                   }
  deriving Show

--------------------

instance FromJSON TitleResponse where
  parseJSON = withObject "TitleResponse" $ \ v → do
    TitleResponse ⊳ v .:  "primaryTitle"
                  ⊵ v .:? "startYear"
                  ⊵ v .:? "runtimeSeconds"
                  ⊵ v .:? "plot"
                  ⊵ v .:? "interests"
                  ⊵ v .:? "stars"
                  ⊵ v .:? "directors"

------------------------------------------------------------

data Interest = Interest { interestName ∷ 𝕋 } deriving Show

--------------------

instance FromJSON Interest where
  parseJSON = withObject "Interest" $ \ v → Interest <$> v .: "name"

------------------------------------------------------------

data Person' = Person' { displayName ∷ 𝕋 } deriving Show

--------------------

instance FromJSON Person' where
  parseJSON = withObject "Person" $ \ v → Person' <$> v .: "displayName"

------------------------------------------------------------

data CertificateResponse = CertificateResponse { certificates ∷ [Certificate] }
  deriving Show

--------------------

instance FromJSON CertificateResponse where
  parseJSON =
    withObject "CertificateResponse" $ \ v → CertificateResponse <$> v .: "certificates"

------------------------------------------------------------

data Certificate = Certificate { country ∷ Country , rating ∷ 𝕋 } deriving Show

--------------------

instance FromJSON Certificate where
  parseJSON =
    withObject "Certificate" $ \ v → Certificate <$> v .: "country" <*> v .: "rating"

------------------------------------------------------------

data Country = Country { code ∷ 𝕋 } deriving Show

--------------------

instance FromJSON Country where
  parseJSON = withObject "Country" $ \ v → Country <$> v .: "code"

------------------------------------------------------------

data ImageResponse = ImageResponse { images ∷ [Image] } deriving Show

--------------------

instance FromJSON ImageResponse where
  parseJSON = withObject "ImageResponse" $ \ v → ImageResponse <$> v .: "images"

------------------------------------------------------------

data Image = Image { imageType ∷ 𝕋, url ∷ URI } deriving Show

instance FromJSON URI where
  parseJSON = withText "URI" $ \ t → either (parseFail ∘ show) pure $ mkURI t

instance FromJSON Image where
  parseJSON = withObject "Image" $ \ v → Image <$> v .: "type" <*> (v .: "url")

------------------------------------------------------------

data FrontMatter = FrontMatter { imdb          ∷ 𝕋
                               , title         ∷ 𝕋
                               , cover         ∷ 𝕋
                               , ukCertificate ∷ 𝕋
                               , summary       ∷ 𝕋
                               , year          ∷ 𝕋
                               , duration      ∷ 𝕋
                               , interests'    ∷ 𝕄 [𝕋]
                               , stars'        ∷ 𝕄 [𝕋]
                               , directors'    ∷ 𝕄 [𝕋]
                               }
  deriving Generic

instance ToJSON FrontMatter where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropWhileEnd (≡'\'')
                                        , omitNothingFields = 𝓣 }

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

class    ToPathPiece α                  where toPathPiece ∷ α → RText 'PathPiece
instance ToPathPiece (RText 'PathPiece) where toPathPiece = id

------------------------------------------------------------

data IMDB_ID = IMDB_ID ℕ  deriving  Show

--------------------

instance Printable IMDB_ID where
  print (IMDB_ID i) = P.text $ [fmt|tt%07d|] i

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
          ⊵ nub ⊳ (many (textualOption (ю [ short 'w', long "wants", long "want"
                                          , help "wants to see" ])))
          ⊵ nub ⊳ (many (textualOption (ю [ short 'h', long "has-seen", long "seen"
                                          , help "has seen" ])))

------------------------------------------------------------

parseRequest ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) => 𝕋 → μ HTTP.Request
parseRequest url = eitherME (userE ∘ show) $ HTTP.parseRequest $ T.unpack url

----------------------------------------

parseRequest' ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) => URI → μ HTTP.Request
parseRequest' = eitherME (userE ∘ show) ∘ HTTP.parseRequest ∘ renderStr

----------------------------------------

fetchResponse ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) => 𝕋 → μ ByteString
fetchResponse url = do
  response ← parseRequest url ≫ HTTP.httpBS
  let status = HTTP.getResponseStatusCode response
  if status == 200
  then return $ HTTP.getResponseBody response
  else throwUserError $ "HTTP error: " ◇ show status

----------------------------------------

fetchResponse' ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) => URI → μ ByteString
fetchResponse' uri = do
  response ← parseRequest' uri ≫ HTTP.httpBS
  let status = HTTP.getResponseStatusCode response
  if status == 200
  then return $ HTTP.getResponseBody response
  else throwUserError $ "HTTP error: " ◇ show status

----------------------------------------

fetchJson ∷ ∀ ε a μ . (MonadIO μ, AsIOError ε, MonadError ε μ, FromJSON a) => 𝕋 → μ (𝕄 a)
fetchJson url = do
  (Aeson.eitherDecode ∘ BSS.fromStrict) ⊳ fetchResponse url ≫ \ case
    𝓛 err    → throwUserError $ "Error decoding JSON: " ◇ err
    𝓡 result → return $ 𝓙 result

----------------------------------------

fetchJson' ∷ ∀ ε a μ . (MonadIO μ, AsIOError ε, MonadError ε μ, FromJSON a) => URI → μ (𝕄 a)
fetchJson' uri = do
  (Aeson.eitherDecode ∘ BSS.fromStrict) ⊳ fetchResponse' uri ≫ \ case
    𝓛 err    → throwUserError $ "Error decoding JSON: " ◇ err
    𝓡 result → return $ 𝓙 result

----------------------------------------

-- | Sanitize title for filename
titleFilename ∷ 𝕋 → 𝕄 Int → PathComponent
titleFilename title year =
  let name = case breakOn " " $ T.replace "/" "-" $ T.replace ":" "-" title of
               ("The", rest) → dropWhile (≡' ') rest ◇ "," ◇ "The"
               ("A",   rest) → dropWhile (≡' ') rest ◇ "," ◇ "A"
               ("An",  rest) → dropWhile (≡' ') rest ◇ "," ◇ "An"
               (ini,   rest) → ini ◇ rest
      year_text = "" ⧐ ([fmt|  [%d]|] ⊳ year)
  in  __parse__ $ name ◇ year_text

----------------------------------------

-- | Format duration from seconds
formatDuration ∷ 𝕄 Int → 𝕋
formatDuration (𝓙 seconds) =
  let hours = seconds `div` 3600
      minutes = (seconds `mod` 3600) `div` 60
  in T.pack $ show hours ◇ "h" ◇ show minutes ◇ "m"
formatDuration 𝓝 = "N/A"

----------------------------------------

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
                         URI → AbsFile → μ ()

downloadAndResizeImage image_uri target_path = do
  do_mock ← asks (view doMock)
  -- Download the image to a temporary file
  let temp_file_path = target_path ⊙ [pc|tmp|]
  body ← fetchResponse' image_uri
  writeFile Informational 𝓝 (𝓙 0o644) temp_file_path body do_mock

  -- Use ImageMagick to resize the image
  ꙭ ([absfile|/run/current-system/sw/bin/magick|],
     [toText temp_file_path, "-resize", "600x400>", toText target_path])

  -- Remove the temporary file
  unlink Informational temp_file_path do_mock

----------------------------------------

writeMarkdownFile ∷ MonadIO μ => 𝕋 → 𝕋 → 𝕄 𝕋 → TitleResponse → FilePath → μ ()
writeMarkdownFile tt sanitizedTitle ukCert titleResponse targetPath = do
  let fm = FrontMatter
        { imdb          = tt
        , title         = primaryTitle titleResponse
        , cover         = T.concat ["[[", sanitizedTitle, ".jpg]]"]
        , ukCertificate = "N/A" ⧐ ukCert
        , summary       = ""    ⧐ plot titleResponse
        , year          = maybe "N/A" (T.pack . show) (startYear titleResponse)
        , duration      = formatDuration (runtimeSeconds titleResponse)
        , interests'    = map interestName ⊳ interests titleResponse
        , stars'        = map displayName  ⊳ stars     titleResponse
        , directors'    = map displayName  ⊳ directors titleResponse
        }

      yamlContent = "---\n" ◇ decodeUtf8With lenientDecode (Yaml.encode fm) ◇ "---\n"

  liftIO $ TIO.writeFile targetPath yamlContent


----------------------------------------

fromPC ∷ (Element α ~ PathComponent, FromSeqNonEmpty α) => PathComponent → α
fromPC = fromSeqNE ∘ pure


-- | Process a single title
-- processTitle ∷ 𝕋 → Options → IO ()
processTitle ∷ ∀ ε ρ μ .
               (MonadIO μ, MonadLog (Log MockIOClass) μ,
                HasDoMock ρ, MonadReader ρ μ,
                AsFPathError ε, AsIOError ε, AsCreateProcError ε, AsProcExitError ε, Printable ε, MonadError ε μ) =>
               AbsDir → IMDB_ID → Options → μ ()
processTitle info_dir tt opts = do
  let title_uri = imdbApiBase & uriPath ⊧ (◇ [toPathPiece tt])
  liftIO $ putStrLn $ "trying url: " ◇ renderStr title_uri
  -- XXX this should fail with, e.g., https://api.imdbapi.dev/titles/tt107206
  maybeTitleResponse ← fetchJson' title_uri
  case maybeTitleResponse of
    𝓝 → liftIO $ putStrLn $ "Failed to fetch title: " ◇ toString tt
    𝓙 titleResponse → do
      let sanitized_title   = titleFilename (primaryTitle titleResponse) (startYear titleResponse)
          movies_dir        = info_dir ⫻ [reldir|movies/|]
          md_fname          = fromPC (sanitized_title ⊙ [pc|md|])
          jpg_fname         = fromPC (sanitized_title ⊙ [pc|jpg|])
          -- XXX lose typesig?
          target_path       ∷ AbsFile
          target_path       = movies_dir ⫻ md_fname
          attachment_dir    = movies_dir ⫻ [reldir|_attachments/|]
          image_target_path = attachment_dir ⫻ jpg_fname
          tt_pp             = toPathPiece tt
      -- check if the file already exists
      liftIO $ putStrLn $ "Fetched title: " ◇ toString tt
          -- XXX use something better than Dir, e.g., MockIO
      exists ← liftIO $ Dir.doesFileExist (toString target_path)
      if exists
        then liftIO $ putStrLn $ "Already exists: " ◇ toString target_path ◇ " (" ◇ toString tt ◇ ")"
        else do
          liftIO $ putStrLn $ "Found title: " ◇ T.unpack (primaryTitle titleResponse)

          -- Create attachments directory if it doesn't exist
          -- XXX use something better than Dir, e.g., MockIO
          liftIO $ Dir.createDirectoryIfMissing 𝓣 (toString attachment_dir)

          -- Fetch and process images
          let imagesUrl = imdbApiBase & uriPath ⊧ (◇ [tt_pp, [pathPiece|images|]])
          maybeImageResponse ← fetchJson' imagesUrl
          case maybeImageResponse of
            𝓙 imageResponse → do
              let posterImages = filter (\ image → (imageType image) == "poster") (images imageResponse)
              if null posterImages
                then liftIO $ putStrLn "No images found"
                else do
                  liftIO $ putStrLn $ "Writing " ◇ (toString image_target_path) ◇ "..."
                  case head posterImages of
                    𝓝    → liftIO $ putStrLn "no image found"
                    𝓙 pI → downloadAndResizeImage (url pI) image_target_path
            _ → liftIO $ putStrLn "Failed to fetch images"

          -- Fetch certificate
          let certificate_url = imdbApiBase & uriPath ⊧ (◇ [tt_pp, [pathPiece|certificates|]])
          maybeCertificateResponse ← fetchJson' certificate_url
          let ukCertificate = case maybeCertificateResponse of
                𝓙 certificateResponse →
                  Maybe.listToMaybe $ map rating $ filter (\ certificate → code (country certificate) == "GB") (certificates certificateResponse)
                𝓝 → 𝓝

          -- XXX writeMarkdownFile to not take a string for a filename
          writeMarkdownFile (toText tt) (toText sanitized_title) ukCertificate titleResponse (toString target_path)

          -- Update people files
          Monad.forM_ (people opts) $ \ p → do
            let pp = T.unpack (personPrefix p)
                personDir = FP.combine "people" (T.unpack (personName p))
            liftIO $ Dir.createDirectoryIfMissing 𝓣 personDir
            let personFilePath = FP.combine personDir $ pp ◇ "-wants-to-see.md"
            liftIO $ TIO.appendFile personFilePath $ T.concat ["[[", (primaryTitle titleResponse), "]]\n"]

          Monad.forM_ (seen opts) $ \ p → do
            let pp = T.unpack (personPrefix p)
                personDir = FP.combine "people" (T.unpack (personName p))
            liftIO $ Dir.createDirectoryIfMissing 𝓣 personDir
            let personFilePath = FP.combine personDir $ pp ◇ "-has-seen.md"
            liftIO $ TIO.appendFile personFilePath $ T.concat ["[[", (primaryTitle titleResponse), "]]\n"]


----------------------------------------

doMain ∷ ∀ ε μ .
         (MonadIO μ, MonadLog (Log MockIOClass) μ,
          AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ) =>
         DoMock → Options → μ ()
-- XXX DoMock; percolate it through
doMain doMock opts = do
  cwd ← getCwd
  if null (tts opts)
  then throwUserError @_ @𝕋 "no titles provided"
  else do
    -- Check if movies directory exists
    moviesDirExists ← liftIO $ Dir.doesDirectoryExist "movies"
    if not moviesDirExists
      then throwUserError @_ @𝕋 "run this in an obsidian movies-info dir"
      else Monad.forM_ (tts opts) $ \ tt → ѥ (flip runReaderT doMock $ processTitle @UsageParseFPProcIOError cwd tt opts) ≫ \ case
                                      𝓛 e → liftIO $ Exit.exitFailure -- XXX REASON/error
                                      𝓡 r → return r

----------------------------------------

main ∷ IO ()
main = let progDesc ∷ 𝕋 = "add a new film to the obsidian movies library"
       in  stdMainSimple progDesc parseOptions doMain

-- that's all, folks! ----------------------------------------------------------
