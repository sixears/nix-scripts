-- IMDB to Obsidian Haskell Script
-- Uses ImageMagick (`magick`) for image resizing via command line

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Base1  hiding  ( head )

import Prelude  ( Bool( True ), FilePath, (++), Int, all, div, drop, filter, head, lookup, map, mod, null, putStrLn )


import qualified Data.Aeson           as A
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

-- aeson -------------------------------

import Data.Aeson  ( FromJSON, withObject )

-- base --------------------------------

-- text --------------------------------

import qualified  Data.Text  as  T

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
                                   , stars          ∷ 𝕄 [Person]
                                   , directors      ∷ 𝕄 [Person]
                                   }
  deriving Show

--------------------

instance FromJSON TitleResponse where
  parseJSON = withObject "TitleResponse" $ \ v → do
    TitleResponse ⊳ v A..:  "primaryTitle"
                  ⊵ v A..:? "startYear"
                  ⊵ v A..:? "runtimeSeconds"
                  ⊵ v A..:? "plot"
                  ⊵ v A..:? "interests"
                  ⊵ v A..:? "stars"
                  ⊵ v A..:? "directors"

------------------------------------------------------------

data Interest = Interest { interestName ∷ 𝕋 } deriving Show

--------------------

instance FromJSON Interest where
  parseJSON = withObject "Interest" $ \ v → Interest <$> v A..: "name"

------------------------------------------------------------

data Person = Person { displayName ∷ 𝕋 } deriving Show

--------------------

instance A.FromJSON Person where
  parseJSON = withObject "Person" $ \ v → Person <$> v A..: "displayName"

------------------------------------------------------------

data CertificateResponse = CertificateResponse { certificates ∷ [Certificate] }
  deriving Show

--------------------

instance FromJSON CertificateResponse where
  parseJSON =
    withObject "CertificateResponse" $ \ v → CertificateResponse <$> v A..: "certificates"

------------------------------------------------------------

data Certificate = Certificate { country ∷ Country , rating ∷ 𝕋 } deriving Show

--------------------

instance A.FromJSON Certificate where
  parseJSON =
    withObject "Certificate" $ \ v → Certificate <$> v A..: "country" <*> v A..: "rating"

------------------------------------------------------------

data Country = Country { code ∷ 𝕋 } deriving Show

--------------------

instance A.FromJSON Country where
  parseJSON = withObject "Country" $ \ v → Country <$> v A..: "code"

------------------------------------------------------------

data ImageResponse = ImageResponse { images ∷ [Image] } deriving Show

--------------------

instance A.FromJSON ImageResponse where
  parseJSON = withObject "ImageResponse" $ \ v → ImageResponse <$> v A..: "images"

------------------------------------------------------------

data Image = Image { imageType ∷ 𝕋 , url ∷ 𝕋 } deriving Show

instance FromJSON Image where
  parseJSON = withObject "Image" $ \ v → do Image <$> v A..: "type" <*> v A..: "url"

------------------------------------------------------------

-- | Command line options
data Options = Options { tts    :: [𝕋]
                       , people :: [𝕋]
                       , seen   :: [𝕋]
                       }
  deriving Show

------------------------------------------------------------

-- | Fetch JSON from a URL
fetchJson ∷ A.FromJSON a => 𝕋 → IO (𝕄 a)
fetchJson url = do
  request ← HTTP.parseRequest $ T.unpack url
  response ← HTTP.httpBS request
  let status = HTTP.getResponseStatusCode response
  if status == 200
    then do
      let body = HTTP.getResponseBody response
      case A.eitherDecode $ BSS.fromStrict body of
        Left err → do
          putStrLn $ "Error decoding JSON: " ++ err
          return Nothing
        Right result → return $ Just result
    else do
      putStrLn $ "HTTP error: " ++ show status
      return Nothing

----------------------------------------

-- | Sanitize title for filename
sanitizeTitle ∷ 𝕋 → 𝕋
sanitizeTitle title =
  let replacedColons = T.replace ":" "-" title
      replacedSlashes = T.replace "/" "-" replacedColons
  in replacedSlashes

----------------------------------------

-- | Format duration from seconds
formatDuration ∷ 𝕄 Int → 𝕋
formatDuration (Just seconds) =
  let hours = seconds `div` 3600
      minutes = (seconds `mod` 3600) `div` 60
  in T.pack $ show hours ++ "h" ++ show minutes ++ "m"
formatDuration Nothing = "N/A"

----------------------------------------

-- | Write a property to a file
writeProperty ∷ FilePath → 𝕋 → 𝕋 → IO ()
writeProperty filePath key value = do
  TIO.appendFile filePath $ T.concat [key, ": ", value, "\n"]

----------------------------------------

-- | Write a list of properties to a file
writeProperties ∷ FilePath → 𝕋 → [𝕋] → IO ()
writeProperties filePath key values = do
  TIO.appendFile filePath $ T.concat [key, ":\n"]
  Monad.forM_ values $ \ value → do
    TIO.appendFile filePath $ T.concat ["  - ", value, "\n"]

----------------------------------------

-- | Download and resize an image using ImageMagick's `magick` command
downloadAndResizeImage ∷ 𝕋 → FilePath → IO ()
downloadAndResizeImage imageUrl targetPath = do
  -- Download the image to a temporary file
  request ← HTTP.parseRequest $ T.unpack imageUrl
  response ← HTTP.httpBS request
  let body = HTTP.getResponseBody response
  let tempFilePath = targetPath ++ ".tmp"
  BSL.writeFile tempFilePath $ BSS.fromStrict body

  -- Use ImageMagick to resize the image
--  let magickCmd = Proc.proc "magick" [tempFilePath, "-resize", "600x400>", targetPath]
--  _ ← Proc.callCommand $ Proc.showCommandForUser magickCmd []
  Proc.callProcess "magick" [tempFilePath, "-resize", "600x400>", targetPath]

  -- Remove the temporary file
  Dir.removeFile tempFilePath

----------------------------------------

-- | Process a single title
--processTitle ∷ 𝕋 → [𝕋] → [𝕋] → IO ()
--processTitle tt people seen = do
processTitle ∷ 𝕋 → Options → IO ()
processTitle tt opts = do
  let titleUrl = T.concat [imdbApiBase, tt]
  maybeTitleResponse ← fetchJson titleUrl
  case maybeTitleResponse of
    Nothing → putStrLn $ "Failed to fetch title: " ++ T.unpack tt
    Just titleResponse → do
      let sanitizedTitle = sanitizeTitle (primaryTitle titleResponse)
          targetPath = FP.combine "movies" $ T.unpack sanitizedTitle ++ ".md"
          attachmentDir = FP.combine "movies" "_attachments"
          imageTargetPath = FP.combine attachmentDir $ T.unpack sanitizedTitle ++ ".jpg"

      -- Check if the file already exists
      exists ← Dir.doesFileExist targetPath
      if exists
        then putStrLn $ "Already exists: " ++ targetPath ++ " (" ++ T.unpack tt ++ ")"
        else do
          putStrLn $ "Found title: " ++ T.unpack (primaryTitle titleResponse)

          -- Create attachments directory if it doesn't exist
          Dir.createDirectoryIfMissing True attachmentDir

          -- Fetch and process images
          let imagesUrl = T.concat [imdbApiBase, tt, "/images"]
          maybeImageResponse ← fetchJson imagesUrl
          case maybeImageResponse of
            Just imageResponse → do
              let posterImages = filter (\ image → (imageType image) == "poster") (images imageResponse)
              if null posterImages
                then putStrLn "No images found"
                else do
                  putStrLn $ "Writing " ++ imageTargetPath ++ "..."
                  downloadAndResizeImage (url $ head posterImages) imageTargetPath
            _ → putStrLn "Failed to fetch images"

          -- Fetch certificate
          let certificateUrl = T.concat [imdbApiBase, tt, "/certificates"]
          maybeCertificateResponse ← fetchJson certificateUrl
          let ukCertificate = case maybeCertificateResponse of
                Just certificateResponse →
                  Maybe.listToMaybe $ map rating $ filter (\ certificate → code (country certificate) == "GB") (certificates certificateResponse)
                Nothing → Nothing

          -- Write the markdown file
          TIO.writeFile targetPath "---\n"
          writeProperty targetPath "imdb" tt
          writeProperty targetPath "title" (T.concat ["\"", (primaryTitle titleResponse), "\""])
          writeProperty targetPath "cover" (T.concat ["\"[[", sanitizedTitle, ".jpg]]\""])
          writeProperty targetPath "UK Certificate" ("N/A" ⧐ ukCertificate)
          writeProperty targetPath "summary" (T.concat ["\"", "" ⧐ plot titleResponse, "\""])
          writeProperty targetPath "year" (T.pack $ Maybe.maybe "N/A" show (startYear titleResponse))
          writeProperty targetPath "duration" (formatDuration (runtimeSeconds titleResponse))

          case (interests titleResponse) of
            Just is → writeProperties targetPath "interests" (map interestName is)
            Nothing → return ()
          case (stars titleResponse) of
            Just ss → writeProperties targetPath "stars" (map displayName ss)
            Nothing → return ()
          case (directors titleResponse) of
            Just ds → writeProperties targetPath "directors" (map displayName ds)
            Nothing → return ()

          TIO.appendFile targetPath "---\n"

          -- Update people files
          let personMap = [("Abi", "ax"), ("Xander", "xax"), ("JJ", "jj"), ("Mum", "hx")]
          Monad.forM_ (people opts) $ \ p → do
            let pp = "" ⧐ lookup (T.unpack p) personMap
            if null pp
              then putStrLn $ "No pp for '" ++ T.unpack p ++ "'"
              else do
                let personDir = FP.combine "people" (T.unpack p)
                Dir.createDirectoryIfMissing True personDir
                let personFilePath = FP.combine personDir $ pp ++ "-wants-to-see.md"
                TIO.appendFile personFilePath $ T.concat ["[[", (primaryTitle titleResponse), "]]\n"]

          Monad.forM_ (seen opts) $ \ p → do
            let pp = "" ⧐ lookup (T.unpack p) personMap
            if null pp
              then putStrLn $ "No pp for '" ++ T.unpack p ++ "'"
              else do
                let personDir = FP.combine "people" (T.unpack p)
                Dir.createDirectoryIfMissing True personDir
                let personFilePath = FP.combine personDir "has-seen.md"
                TIO.appendFile personFilePath $ T.concat ["[[", (primaryTitle titleResponse), "]]\n"]

----------------------------------------

-- | Parse command line arguments
parseArgs ∷ [String] → Options -- ([𝕋], [𝕋], [𝕋])
parseArgs args =
  let (tts, peopleArgs) = List.partition (\ arg → "tt" `List.isPrefixOf` arg && all Char.isDigit (drop 2 arg)) args
      (people, seen) = List.partition (\ arg → "+" `List.isPrefixOf` arg) peopleArgs
      parsedPeople = map (T.pack . drop 1) people
      parsedSeen = map (T.pack . drop 1) seen
--  in (map T.pack tts, parsedPeople, parsedSeen)
  in Options { tts = map T.pack tts
             , people = parsedPeople
             , seen = parsedSeen
             }
----------------------------------------

-- | Main function
main ∷ IO ()
main = do
  args ← Env.getArgs
  if null args
    then putStrLn "usage: imdb <tt...>"
    else do
      let opts = parseArgs args
      if null (tts opts)
        then putStrLn "no titles provided"
        else do
          -- Check if movies directory exists
          moviesDirExists ← Dir.doesDirectoryExist "movies"
          if not moviesDirExists
            then putStrLn "run this in an obsidian movies-info dir"
            else Monad.forM_ (tts opts) $ \ tt → processTitle tt opts

-- that's all, folks! ----------------------------------------------------------
