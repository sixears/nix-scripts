-- IMDB to Obsidian Haskell Script
-- Uses ImageMagick (`magick`) for image resizing via command line

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Base1

-- putStrLn => log, or error?
import Prelude  ( FilePath, Int, all, div, drop, error, filter, map, mod, null, putStrLn )

import qualified Data.ByteString      as BSS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO         as TIO
import qualified Network.HTTP.Simple  as HTTP
import qualified System.Directory     as Dir
import qualified System.FilePath      as FP
import qualified System.Environment   as Env
import qualified System.Process       as Proc
import qualified Data.Char            as Char
import qualified Data.List            as List
import qualified Data.Maybe           as Maybe
import qualified Control.Monad        as Monad
import qualified System.Exit          as Exit
import qualified Data.Yaml                    as Yaml
import qualified Data.Text                    as T

-- aeson -------------------------------

import qualified Data.Aeson           as Aeson

import Data.Aeson  ( FromJSON, ToJSON, (.:), (.:?),
                     defaultOptions, omitNothingFields, genericToJSON, withObject )

-- base --------------------------------

import Data.Char     ( isDigit )
import Data.List     ( isPrefixOf, nub, partition, zip )
import Data.Maybe    ( catMaybes )
import GHC.Generics  ( Generic )
import Text.Read     ( read )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual ), fromText,toText )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError, FPathIOError )

-- monaderror-io -----------------------

import MonadError           ( eitherME )
import MonadError.IO.Error  ( IOError, throwUserError )

-- monadio-plus ------------------------

import MonadIO.FPath  ( getCwd )

-- parser-plus -------------------------

import ParserPlus  ( digits )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, help, long, metavar, readerError, short, strArgument )

-- optparse-plus -----------------------

import OptParsePlus  ( textualArgument, textualOption )

-- parsers -----------------------------

import Text.Parser.Char         ( string )
import Text.Parser.Combinators  ( choice )

-- stdmain -----------------------------

import StdMain  ( stdMainSimple )

-- text --------------------------------

import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

-- | IMDB API base URL
imdbApiBase ∷ 𝕋
imdbApiBase = "https://api.imdbapi.dev/titles/"

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

data Image = Image { imageType ∷ 𝕋, url ∷ 𝕋 } deriving Show

instance FromJSON Image where
  parseJSON = withObject "Image" $ \ v → do Image <$> v .: "type" <*> v .: "url"

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
  toJSON = genericToJSON defaultOptions { omitNothingFields = 𝓣 }

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

data IMDB_ID = IMDB_ID ℕ  deriving  Show

--------------------

instance Printable IMDB_ID where
  print (IMDB_ID i) = P.string $ "tt" ◇ show i

--------------------

instance Textual IMDB_ID where
  textual = IMDB_ID ⊳ (read ⊳ (string "tt" ⋫ digits))

------------------------------------------------------------

-- | Command line options
data Options = Options { tts    :: [IMDB_ID]
                       , people :: [Person]
                       , seen   :: [Person]
                       }
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

fetchResponse ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) => 𝕋 → μ ByteString
fetchResponse url = do
  response ← parseRequest url ≫ HTTP.httpBS
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

-- | Sanitize title for filename
sanitizeTitle ∷ 𝕋 → 𝕋
sanitizeTitle title = T.replace "/" "-" $ T.replace ":" "-" title

----------------------------------------

-- | Format duration from seconds
formatDuration ∷ 𝕄 Int → 𝕋
formatDuration (𝓙 seconds) =
  let hours = seconds `div` 3600
      minutes = (seconds `mod` 3600) `div` 60
  in T.pack $ show hours ◇ "h" ◇ show minutes ◇ "m"
formatDuration 𝓝 = "N/A"

----------------------------------------

-- | Download and resize an image using ImageMagick's `magick` command
downloadAndResizeImage ∷ 𝕋 → FilePath → IO ()
downloadAndResizeImage imageUrl targetPath = do
  -- Download the image to a temporary file
  request ← HTTP.parseRequest $ T.unpack imageUrl
  response ← HTTP.httpBS request
  let body = HTTP.getResponseBody response
  let tempFilePath = targetPath ◇ ".tmp"
  BSL.writeFile tempFilePath $ BSS.fromStrict body

  -- Use ImageMagick to resize the image
  Proc.callProcess "magick" [tempFilePath, "-resize", "600x400>", targetPath]

  -- Remove the temporary file
  Dir.removeFile tempFilePath

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

-- | Process a single title
-- processTitle ∷ 𝕋 → Options → IO ()
processTitle ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) => 𝕋 → Options → μ ()
processTitle tt opts = do
  let titleUrl = T.concat [imdbApiBase, tt]
  maybeTitleResponse ← fetchJson titleUrl
  case maybeTitleResponse of
    𝓝 → liftIO $ putStrLn $ "Failed to fetch title: " ◇ T.unpack tt
    𝓙 titleResponse → do
      let sanitizedTitle = sanitizeTitle (primaryTitle titleResponse)
          targetPath = FP.combine "movies" $ T.unpack sanitizedTitle ◇ ".md"
          attachmentDir = FP.combine "movies" "_attachments"
          imageTargetPath = FP.combine attachmentDir $ T.unpack sanitizedTitle ◇ ".jpg"

      -- Check if the file already exists
      exists ← liftIO $ Dir.doesFileExist targetPath
      if exists
        then liftIO $ putStrLn $ "Already exists: " ◇ targetPath ◇ " (" ◇ T.unpack tt ◇ ")"
        else do
          liftIO $ putStrLn $ "Found title: " ◇ T.unpack (primaryTitle titleResponse)

          -- Create attachments directory if it doesn't exist
          liftIO $ Dir.createDirectoryIfMissing 𝓣 attachmentDir

          -- Fetch and process images
          let imagesUrl = T.concat [imdbApiBase, tt, "/images"]
          maybeImageResponse ← fetchJson imagesUrl
          case maybeImageResponse of
            𝓙 imageResponse → do
              let posterImages = filter (\ image → (imageType image) == "poster") (images imageResponse)
              if null posterImages
                then liftIO $ putStrLn "No images found"
                else do
                  liftIO $ putStrLn $ "Writing " ◇ imageTargetPath ◇ "..."
                  case head posterImages of
                    𝓝    → liftIO $ putStrLn "no image found"
                    𝓙 pI → liftIO $ downloadAndResizeImage (url pI) imageTargetPath
            _ → liftIO $ putStrLn "Failed to fetch images"

          -- Fetch certificate
          let certificateUrl = T.concat [imdbApiBase, tt, "/certificates"]
          maybeCertificateResponse ← fetchJson certificateUrl
          let ukCertificate = case maybeCertificateResponse of
                𝓙 certificateResponse →
                  Maybe.listToMaybe $ map rating $ filter (\ certificate → code (country certificate) == "GB") (certificates certificateResponse)
                𝓝 → 𝓝

          writeMarkdownFile tt sanitizedTitle ukCertificate titleResponse targetPath

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

-- | Parse command line arguments
parseArgs ∷ [String] → 𝔼 [𝕋] Options
parseArgs args =
  let (tts, peopleArgs) = List.partition (\ arg → "tt" `List.isPrefixOf` arg && all Char.isDigit (drop 2 arg)) args
      (people, seen) = List.partition (\ arg → "+" `List.isPrefixOf` arg) peopleArgs
      rawPeople = map (T.pack . drop 1) people
      rawSeen = map (T.pack . drop 1) seen
      parsedPeople = map parsePerson rawPeople
      parsedSeen = map parsePerson rawSeen
      unknownPeople = [name | (name, 𝓝) ← zip rawPeople parsedPeople]
      unknownSeen = [name | (name, 𝓝) ← zip rawSeen parsedSeen]
      allUnknown = unknownPeople ◇ unknownSeen
  in if null allUnknown
     then Right $ Options { tts = catMaybes $ map (fromText ∘ T.pack) tts
                          , people = Maybe.catMaybes parsedPeople
                          , seen = Maybe.catMaybes parsedSeen
                          }
     else Left allUnknown

----------------------------------------

{-
-- | Parses command-line arguments.
--   - TT IDs: @tt1234567@ (must start with "tt" followed by digits)
--   - People to add: @-John@ (prefix with '-')
--   - People seen: @+Jane@ (prefix with '+')
--   Fails with a list of any unrecognized person names.
parseOptions :: Parser Options
parseOptions = do
  args <- many (strArgument (metavar "ARG"))
  let (tts, rest) = partition (\arg -> "tt" `isPrefixOf` arg && all isDigit (drop 2 arg)) args
      (people, seen) = partition (\arg -> "-" `isPrefixOf` arg) rest
      rawPeople = map (T.pack . drop 1) people
      rawSeen = map (T.pack . drop 1) seen
      parsedPeople = map parsePerson rawPeople
      parsedSeen = map parsePerson rawSeen
      unknownPeople = [name | (name, Nothing) <- zip rawPeople parsedPeople]
      unknownSeen = [name | (name, Nothing) <- zip rawSeen parsedSeen]
      allUnknown = unknownPeople ◇ unknownSeen
  if null allUnknown
    then pure $ Options (map T.pack tts) (catMaybes parsedPeople) (catMaybes parsedSeen)
--    else readerError (show allUnknown)
    else error $ "Unknown person(s): " ◇ T.unpack (T.intercalate ", " allUnknown)
-}

----------------------------------------

doMain ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) => Options → μ ()
doMain opts = do
  cwd ← getCwd
  if null (tts opts)
  then throwUserError @_ @𝕋 "no titles provided"
  else do
    -- Check if movies directory exists
    moviesDirExists ← liftIO $ Dir.doesDirectoryExist "movies"
    if not moviesDirExists
      then throwUserError @_ @𝕋 "run this in an obsidian movies-info dir"
      else Monad.forM_ (tts opts) $ \ tt → ѥ (processTitle @IOError (toText tt) opts) ≫ \ case
                                      𝓛 e → liftIO $ Exit.exitFailure -- XXX REASON/error
                                      𝓡 r → return r

----------------------------------------

-- | Main function
main' ∷ IO ()
main' = do
  args ← Env.getArgs
  if null args
    then putStrLn "usage: imdb <tt...>"
    else do
      case parseArgs args of
         𝓛 unknown → do putStrLn "Error: Unknown person names:"
                        mapM_ (\name → TIO.putStrLn (T.concat ["  ", name])) unknown
                        Exit.exitFailure
         𝓡 opts → ѥ (doMain @FPathIOError opts) ≫ \ case
                     𝓡 () → return ()
                     𝓛 e  → liftIO $ putStrLn (show e) ⪼ Exit.exitFailure


doMain' doMock opts = doMain opts

main ∷ IO ()
main = let progDesc ∷ 𝕋 = "add a new film to the obsidian movies library"
        in  stdMainSimple progDesc parseOptions doMain'

-- that's all, folks! ----------------------------------------------------------
