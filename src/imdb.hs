-- IMDB to Obsidian Haskell Script
-- Uses ImageMagick (`magick`) for image resizing via command line

-- XXX add logging timestamps

-- curl --request GET --url 'https://api.themoviedb.org/3/movie/0717151/credits'
-- curl 'https://api.themoviedb.org/3/movie/0717151?api_key=...&append_to_response=credits,images

-- [martyn:movies-info:0]$ curl --request GET --url 'https://api.themoviedb.org/3/movie/717151?api_key=...&append_to_response=videos,credits' | jq | less

-- https://api.themoviedb.org/3/configuration?api_key=...
-- curl 'https://image.tmdb.org/t/p/w500/6bcrhJQLoLNsYX4UGTOMQkCiACA.jpg?api_key=...'

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE ViewPatterns       #-}

module Main where

import Base1

import Prelude  ( Float, divMod, error, filter, floor, null, toEnum )

-- aeson -------------------------------

import Data.Aeson        qualified as Aeson
import Data.Aeson.Types  qualified as AesonTypes

import Data.Aeson         ( FromJSON, ToJSON( toJSON ), (.:), (.:?), (.!=),
                            defaultOptions, fieldLabelModifier, genericToJSON,
                            omitNothingFields, parseJSON,
                            withArray, withObject, withScientific, withText
                          )
import Data.Aeson.Types   ( Object, parseFail )

-- base --------------------------------

import Data.Char           qualified as  Char
import Data.List.NonEmpty  qualified as  NonEmpty

import Control.Monad.Fail  ( fail )
import Data.Bool           ( bool )
import Data.Char           ( isDigit )
import Data.List           ( and, any, dropWhileEnd, nub, or, sort, sortBy, span,
                             splitAt, take, uncons )
import Data.List.NonEmpty  ( nonEmpty )
import Data.Maybe          ( catMaybes )
import Data.Ord            ( compare, comparing )
import Data.Type.Equality  ( type (~) )
import GHC.Exts            ( IsString )
import GHC.Generics        ( Generic )
import System.IO           ( Handle, SeekMode( AbsoluteSeek ), hFileSize,hSeek )
import System.IO.Error     ( IOErrorType, alreadyExistsErrorType, mkIOError )
import Text.Read           ( read )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode  ( (≠) )
import Prelude.Unicode  ( ℚ )

-- bytestring --------------------------

import Data.ByteString  qualified as  BSS
import Data.ByteString  ( ByteString, hGet )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- duration ----------------------------

import Duration  ( Duration( MINS ) )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadCatch )

-- extra -------------------------------

import Data.List.Extra  ( takeWhileEnd )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Basename          ( basename )
import FPath.Dirname           ( dirname )
import FPath.Dir               ( DirAs )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( FileAs )
import FPath.FileLike          ( (⊙) )
import FPath.FPath             ( FPathAs )
import FPath.Parseable         ( __parse__ )
import FPath.PathComponent     ( PathComponent, pc )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( relfile )

-- fstat -------------------------------

import FStat  ( FileType( Directory ), ftype )

-- http-conduit ------------------------

import qualified Network.HTTP.Simple  as  HTTP

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log, debugT, infoT, warnT )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Debug ) )

-- mockio-log --------------------------

import MockIO.IOClass      ( HasIOClass )
import MockIO.Log          ( debugIO, infoIO )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Directory          ( mkdir )
import MockIO.DoMock             ( DoMock( DoMock, NoMock ), HasDoMock( doMock ))
import MockIO.File               ( FExists( FExists, NoFExists ),
                                   fexists, unlink )
import MockIO.FStat              ( stat )
import MockIO.OpenFile           ( FileOpenMode( FileR ), HEncoding( Binary ),
                                   appendFile, readFile, withFile, writeFile )
import MockIO.Process            ( ꙩ )
import MockIO.Process.MLCmdSpec  ( ToMLCmdSpec )

-- monaderror-io -----------------------

import MonadError           ( eitherME )
import MonadError.IO.Error  ( AsIOError( _IOErr ), throwUserError )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.FPath                  ( getCwd )
import MonadIO.NamedHandle            ( HasNamedHandle, handle )
import MonadIO.User                   ( homePath )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- modern-uri --------------------------

import Text.URI  qualified as  URI

import Text.URI       ( QueryParam( QueryParam ), RText,
                        RTextLabel( PathPiece, QueryValue ), URI,
                        mkPathPiece, mkURI, render, renderStr )
import Text.URI.Lens  ( unRText, uriPath, uriTrailingSlash, uriQuery )
import Text.URI.QQ    ( pathPiece, queryKey, queryValue, uri )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks, runReaderT )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE  qualified as  SeqNE
import NonEmptyContainers.SeqNEConversions  ( FromSeqNonEmpty,
                                              fromSeqNE, toSeqNE )

-- parser-plus -------------------------

import ParserPlus  ( digits )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, flag, help, long, metavar, short )

-- optparse-plus -----------------------

import OptParsePlus  ( textualArgument, textualOption )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, anyChar, string )
import Text.Parser.Combinators  ( choice, optional )

-- scientific --------------------------

import Data.Scientific  ( floatingOrInteger )

-- stdmain -----------------------------

import StdMain  ( stdMainSimple )

-- text --------------------------------

import Data.Text  qualified as  T

import Data.Text.Encoding  ( encodeUtf8 )

-- text-printer ------------------------

import Text.Printer  qualified as  P

-- time --------------------------------

import Data.Time.Calendar.OrdinalDate  ( Day, Year, toOrdinalDate )
import Data.Time.Clock                 ( utctDay )
import Data.Time.Format                ( defaultTimeLocale, parseTimeM )
import Data.Time.Format.ISO8601        ( iso8601Show )

-- vector ------------------------------

import Data.Vector  ( Vector )

-- yaml --------------------------------

import Data.Yaml  ( decodeEither', encode )

--------------------------------------------------------------------------------

parseArray ∷ FromJSON α => 𝕊 → ([α] → β) → Aeson.Value → AesonTypes.Parser β
parseArray nme tpe = withArray nme $ \ v → tpe ⊳ mapM Aeson.parseJSON (toList v)

------------------------------------------------------------

newtype APIReadToken = APIReadToken { unAPIReadToken ∷ BSS.ByteString }

----------

class HasAPIReadToken α where
  apiReadToken ∷ Lens' α APIReadToken

------------------------------------------------------------

class ToPathPiece α where
  toPathPiece ∷ α → RText PathPiece
  ҩ ∷ α → RText PathPiece
  ҩ = toPathPiece

instance ToPathPiece (RText PathPiece) where toPathPiece = id

------------------------------------------------------------

{-| Does the URI have a trailing slash? -}
data TrailingSlash = TrailingSlash | NoTrailingSlash  deriving  (Eq,Show)

------------------------------------------------------------

newtype MyURI = MyURI { unMyURI ∷ URI } deriving Show

myURI ∷ Lens' MyURI URI
myURI = lens unMyURI (\ _ u → MyURI u)

instance FromJSON MyURI where
  parseJSON = withText "MyURI" $ \ t → 
    either (parseFail ∘ show) (pure ∘ MyURI) $ mkURI t

instance Printable      MyURI  where  print = P.text ∘ render ∘ unMyURI
instance HasQueryParams MyURI  where  queryParams = myURI ∘ uriQuery

----------------------------------------

trailingSlash ∷ Lens' MyURI TrailingSlash
trailingSlash =
  lens (\ (MyURI uri_)    → let is_t_s = (fst ⊳ URI.uriPath uri_ ⧏ 𝓕)
                            in  bool NoTrailingSlash TrailingSlash is_t_s)
       (\ (MyURI uri_) ts → let is_t_s = ts ≡ TrailingSlash
                            in  MyURI $ uri_ & uriTrailingSlash ⊢ is_t_s)

------------------------------------------------------------

{-| things we can append to a URI -}
class UAppend α where
  uAppend ∷ MyURI → α → MyURI
  (‡) ∷ MyURI → α → MyURI
  (‡) = uAppend

instance UAppend [RText 'PathPiece] where
  uAppend (MyURI uri_) pieces =

    let trailing_slash = fst ⊳ URI.uriPath uri_ ⧏ 𝓕
    in  MyURI $ uri_ & ({- myURI ∘ -} uriPath) ⊧ (◇ pieces)
                     & ({- myURI ∘ -} uriTrailingSlash) ⊢ trailing_slash

instance UAppend (RText 'PathPiece) where
  uAppend uri_ piece = uAppend uri_ [piece]

------------------------------------------------------------

newtype ImageWidth = ImageWidth Word16  deriving (Eq,FromJSON,Ord,Printable,Show)
instance ToPathPiece ImageWidth  where
  toPathPiece (ImageWidth w) =
    either (error ∘ show) id ∘ mkPathPiece ∘ ("w" ◇) $ toText w

instance UAppend ImageWidth where uAppend uri_ iw = uAppend uri_ (toPathPiece iw)

----------------------------------------

class    HasImageWidth α           where  imageWidth ∷ Lens' α ImageWidth
instance HasImageWidth ImageWidth  where  imageWidth = lens id (flip const)

----------------------------------------

class    AsImageWidth α          where  _ImageWidth ∷ Prism' α ImageWidth
instance AsImageWidth ImageWidth where  _ImageWidth = prism' id 𝓙

------------------------------------------------------------

newtype ImageHeight = ImageHeight Word16 deriving(Eq,FromJSON,Ord,Printable,Show)

----------------------------------------

class HasImageHeightMay α  where  imageHeightMay ∷ Lens' α (𝕄 ImageHeight)
instance HasImageHeightMay (𝕄 ImageHeight) where
  imageHeightMay = lens id (flip const)

------------------------------------------------------------

data ImageSize = ImgWidth ImageWidth | ImgHeight ImageHeight | ImgSizeOriginal
  deriving (Eq,Show)

----------

instance FromJSON ImageSize where
  parseJSON = withText "ImageSize" $ \ v →
    case T.uncons v of
      𝓙 ('w', sz) | T.all isDigit sz →
                    pure $ ImgWidth  (ImageWidth $ read $ T.unpack sz)
      𝓙 ('h', sz) | T.all isDigit sz →
                    pure $ ImgHeight (ImageHeight $ read $ T.unpack sz)
      𝓙 ('o', "riginal")             → pure $ ImgSizeOriginal
      𝓙 _                            → fail $ [fmt|unparse image size: %t|] v
      𝓝                              → fail $ [fmt|empty image size|]


----------

instance AsImageWidth ImageSize where
  _ImageWidth = prism' ImgWidth (\ case (ImgWidth iw) → 𝓙 iw; _ → 𝓝)

------------------------------------------------------------

newtype  PosterSizes = PosterSizes { unPosterSizes ∷ [ImageSize] }  deriving Show
instance FromJSON PosterSizes where
  parseJSON = parseArray "PosterSizes" PosterSizes

----------------------------------------

class    HasPosterSizes α            where  posterSizes ∷ Lens' α PosterSizes
instance HasPosterSizes PosterSizes  where  posterSizes = lens id (flip const)

------------------------------------------------------------

data Configuration =
  Configuration { _cfgPosterSizes ∷ PosterSizes, _cfgImageSecureBase ∷ MyURI }
  deriving Show

----------

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \ v → do
    images ← v .: "images"
    Configuration ⊳ images .: "poster_sizes"
                  ⊵ images .: "secure_base_url"


----------

instance HasPosterSizes Configuration where
  posterSizes = lens _cfgPosterSizes (\ cfg pss → cfg { _cfgPosterSizes = pss })

--------------------

imageSecureBase ∷ Lens' Configuration MyURI
imageSecureBase = lens _cfgImageSecureBase
                       (\ cfg isb → cfg { _cfgImageSecureBase = isb })

------------------------------------------------------------

data Context = Context { _doMock ∷ DoMock, _apiReadToken ∷ APIReadToken }

----------

instance HasDoMock Context where
  doMock = lens _doMock (\ c dm → c { _doMock = dm })

----------

instance HasAPIReadToken Context where
  apiReadToken = lens _apiReadToken (\ c rt → c { _apiReadToken = rt })

------------------------------------------------------------

newtype Date = Date { unDate ∷ Day }  deriving  (Eq,Ord,Show)

----------

instance FromJSON Date where
  parseJSON = withText "Date" $ \ t →
    case T.unsnoc t of
      𝓙 (_,'Z') → case parseTimeM 𝓣 defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack t) of
                    𝓙 day → return $ Date $ utctDay day
                    𝓝     → fail $ [fmt|Failed to parse date: %t|] t
      _         → case parseTimeM 𝓣 defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
                    𝓙 day → return $ Date day
                    𝓝     → fail $ [fmt|Failed to parse date: %t|] t

--------------------

class    HasDate α     where  date ∷ Lens' α Date
instance HasDate Date  where  date = lens id (flip const)

------------------------------------------------------------

class ToYear α where
  year ∷ α → Year

instance ToYear Date where
  year (Date d) = fst $ toOrdinalDate d

------------------------------------------------------------

newtype Release = Release { unRelease ∷ Date }  deriving  (Eq,FromJSON,Show)

----------

instance HasDate Release where
  date = lens unRelease (\ _ d → Release d)

----------

instance ToYear Release where
  year = year ∘ unRelease

----------

instance ToJSON Release where
  toJSON = Aeson.String ∘ T.pack ∘ iso8601Show ∘ unDate ∘ unRelease

----------------------------------------

class    HasRelease α        where  release ∷ Lens' α Release
instance HasRelease Release  where  release = lens id (flip const)

----------------------------------------

class ToReleaseYear α where
  releaseYear ∷ α → Year

----------

instance ToReleaseYear Release where
  releaseYear = year ∘ view release

------------------------------------------------------------

class ToPathComponent α where
  pathComponent ∷ α → PathComponent

------------------------------------------------------------

class    HasDuration α         where  duration ∷ Lens' α Duration
instance HasDuration Duration  where  duration = lens id (flip const)

------------------------------------------------------------

newtype Runtime = Runtime { unRuntime ∷ Duration }  deriving  Show

----------

instance HasDuration Runtime where
  duration = lens unRuntime (\ _ d → Runtime d)

----------------------------------------

class    HasRuntime α        where  runtime ∷ Lens' α Runtime
instance HasRuntime Runtime  where  runtime = lens id (flip const)

------------------------------------------------------------

newtype Overview = Overview { unOverview ∷ 𝕋 }  deriving  Show

----------------------------------------

class HasOverview α where
  overview ∷ Lens' α Overview

------------------------------------------------------------

newtype Genre = Genre { genreName ∷ 𝕋 } deriving Show

--------------------

instance FromJSON Genre where
  parseJSON = withObject "Genre" $ \ v → Genre <$> v .: "name"

--------------------

instance ToJSON Genre where
  toJSON = Aeson.String ∘ genreName

----------------------------------------

class    HasGenre α     where  genre ∷ Lens' α Genre
instance HasGenre Genre where  genre = lens id (flip const)

----------------------------------------

class HasGenres α  where  genres ∷ Lens' α [Genre]

------------------------------------------------------------

newtype Job = Job { unJob ∷ 𝕋 }  deriving  (Eq,FromJSON,Show)

----------------------------------------

class    HasJob α    where  job ∷ Lens' α Job
instance HasJob Job  where  job = lens id (flip const)

------------------------------------------------------------

newtype Name = Name { unName ∷ 𝕋 }  deriving  (Eq,FromJSON,IsString,Show)

----------------------------------------

class    HasName α     where  name ∷ Lens' α Name
instance HasName Name  where  name = lens id (flip const)

------------------------------------------------------------

data TMDBPerson = TMDBPerson { _tpName ∷ Name, _tpJob ∷ Job } deriving Show

----------

instance FromJSON TMDBPerson where
  parseJSON = withObject "Person" $ \ v →
    TMDBPerson ⊳ v .: "name"
               ⊵ v .: "job"

----------

instance ToJSON TMDBPerson where toJSON = Aeson.String ∘ unName ∘ _tpName

----------

instance HasJob TMDBPerson where job = lens _tpJob (\ tp j → tp { _tpJob = j })

------------------------------------------------------------

data TMDBCast = TMDBCast { _tcName ∷ 𝕋, _tcCastID ∷ ℕ, _tcOrder ∷ ℕ }
  deriving Show

----------

instance FromJSON TMDBCast where
  parseJSON = withObject "Person" $ \ v →
    TMDBCast ⊳ v .: "name"
             ⊵ v .: "cast_id"
             ⊵ v .: "order"

----------

instance ToJSON TMDBCast where
  toJSON = Aeson.String ∘ _tcName

------------------------------------------------------------

newtype Cast = Cast [TMDBCast]  deriving  Show

----------

instance FromJSON Cast where
  parseJSON = parseArray "Cast" Cast

----------

instance ToJSON Cast where
  toJSON (Cast cst) = Aeson.Array (fromList $ toJSON ⊳ cst)

----------------------------------------

takeCast ∷ ℕ → Cast → Cast
takeCast n (Cast cs) = Cast $ take (fromIntegral n) cs

----------------------------------------

class    HasCast α     where  cast ∷ Lens' α Cast
instance HasCast Cast  where  cast = lens id (flip const)

------------------------------------------------------------

newtype Directors = Directors [TMDBPerson]  deriving  Show

----------

instance FromJSON Directors where
  parseJSON = parseArray "Directors" Directors

----------

instance ToJSON Directors where
  toJSON (Directors cst) = Aeson.Array (fromList $ toJSON ⊳ cst)

----------------------------------------

class    HasDirectors α          where  directors ∷ Lens' α Directors
instance HasDirectors Directors  where  directors = lens id (flip const)

------------------------------------------------------------

data Language = Language_NONE | Language_EN | Language_ISO639_alpha2 𝕋
  deriving  (Eq,Show)

----------

instance Ord Language where
  Language_EN   <= _ = 𝓣
  _ <= Language_EN   = 𝓕
  Language_NONE <= _ = 𝓣
  _ <= Language_NONE = 𝓕
  (Language_ISO639_alpha2 x) <= (Language_ISO639_alpha2 y) = x <= y

----------

instance Printable Language where
  print Language_NONE              = P.text "-"
  print Language_EN                = P.text "en"
  print (Language_ISO639_alpha2 t) = P.text $ T.toLower t

----------

instance FromJSON Language where
  parseJSON = withText "Language" $ \ t → pure $ case T.toLower t of
                                            "en" → Language_EN
                                            ""   → Language_NONE
                                            _    → Language_ISO639_alpha2 t

----------------------------------------

class    HasLanguage α         where  language ∷ Lens' α Language
instance HasLanguage Language  where  language = lens id (flip const)

------------------------------------------------------------

data Country = Country_NONE | Country_GB | Country_US | Country_ISO3166_alpha2 𝕋
  deriving (Eq,Show)

----------

instance Ord Country where
  Country_GB   <= _ = 𝓣
  _ <= Country_GB   = 𝓕
  Country_US   <= _ = 𝓣
  _ <= Country_US   = 𝓕
  Country_NONE <= _ = 𝓣
  _ <= Country_NONE = 𝓕
  (Country_ISO3166_alpha2 x) <= (Country_ISO3166_alpha2 y) = x <= y

----------

instance Printable Country where
  print Country_NONE               = P.text "-"
  print Country_GB                 = P.text "GB"
  print Country_US                 = P.text "US"
  print (Country_ISO3166_alpha2 t) = P.text t

----------

instance FromJSON Country where
  parseJSON = withText "Country" $ \ t → pure $ case T.toLower t of
                                            "gb" → Country_GB
                                            "uk" → Country_GB
                                            "us" → Country_US
                                            _    → Country_ISO3166_alpha2 t

----------------------------------------

class    HasCountry α        where  country ∷ Lens' α Country
instance HasCountry Country  where  country = lens id (flip const)

------------------------------------------------------------

newtype VoteAvg = VoteAvg ℚ  deriving  (Eq,FromJSON,Ord,Show)

instance Printable VoteAvg where print (VoteAvg q) = P.text $ [fmt|%3.2f|] q

----------------------------------------

class    HasVoteAvg α        where  voteAvg ∷ Lens' α VoteAvg
instance HasVoteAvg VoteAvg  where  voteAvg = lens id (flip const)

------------------------------------------------------------

newtype VoteCount = VoteCount Word32  deriving  (Eq,FromJSON,Ord,Printable,Show)

------------------------------------------------------------

newtype ImagePath = ImagePath { unImagePath ∷ RText PathPiece }  deriving  Show

----------

instance UAppend ImagePath where
  uAppend uri_ ipath = uAppend uri_ (unImagePath ipath)

----------

instance Printable ImagePath where
  print ipath = P.text $ (unImagePath ipath) ⊣ unRText

----------

instance ToPathComponent ImagePath where pathComponent = __parse__

----------------------------------------

class     HasImagePath α          where  imagePath ∷ Lens' α ImagePath
instance  HasImagePath ImagePath  where  imagePath = lens id (flip const)

------------------------------------------------------------

data Image = Image { _imgImagePath ∷ ImagePath
                   , _imgLanguage  ∷ Language
                   , _imgCountry   ∷ Country
                   , _imgWidth     ∷ ImageWidth
                   , _imgHeight    ∷ ImageHeight
                   , _imgVoteAvg   ∷ VoteAvg
                   , _imgVoteCnt   ∷ VoteCount
                   }
           deriving  Show

----------

instance ToPathComponent Image
  where pathComponent img = pathComponent (img ⊣ imagePath)

----------

instance HasImageWidth Image where
  imageWidth = lens _imgWidth (\ i iw → i { _imgWidth = iw })

----------

instance FromJSON Image where
  parseJSON = withObject "Image" $ \ v → do
    let parse_path_piece ∷ 𝕋 → AesonTypes.Parser ImagePath
        parse_path_piece pp =
          let fail_msg e = [fmt|failed to parse path piece: %t (%s)|] pp e
          in  case T.uncons pp of
                𝓙 ('/',pp') → case eitherME show $ mkPathPiece pp' of
                                𝓡 p → return $ ImagePath p
                                𝓛 e → fail $ fail_msg e
                _           → fail $ fail_msg "no leading '/'"
    Image ⊳ (v .: "file_path" ≫ parse_path_piece)
          ⊵ v .:? "iso_639_1"  .!= Language_NONE
          ⊵ v .:? "iso_3166_1" .!= Country_NONE
          ⊵ v .:  "width"
          ⊵ v .:  "height"
          ⊵ v .:  "vote_average"
          ⊵ v .:  "vote_count"

----------

instance HasImagePath Image where
  imagePath = lens _imgImagePath (\ i p → i { _imgImagePath = p })

----------

instance HasLanguage Image where
  language = lens _imgLanguage (\ i l → i { _imgLanguage = l })

----------

instance HasCountry Image where
  country = lens _imgCountry (\ i c → i { _imgCountry = c })

----------

instance HasVoteAvg Image where
  voteAvg = lens _imgVoteAvg (\ i v → i { _imgVoteAvg = v })

----------

instance Printable Image where
  print i =
    P.text $
      [fmt|%t %t/%t %4Tx%-4T %T (%T)|] (unImagePath (i ⊣ imagePath) ⊣ unRText)
                                       (toText $ i ⊣ language)
                                       (toText $ i ⊣ country)
                                       (i ⊣ imageWidth) (_imgHeight i)
                                       (_imgVoteAvg i) (_imgVoteCnt i)

------------------------------------------------------------



newtype Posters = Posters { unPosters ∷ [Image] }  deriving  Show
instance FromJSON Posters  where  parseJSON = parseArray "Posters" Posters

----------------------------------------

class    HasPosters α        where  posters ∷ Lens' α Posters
instance HasPosters Posters  where  posters = lens id (flip const)

------------------------------------------------------------

data ReleaseType = ReleaseType_Premiere
                 | ReleaseType_Theatrical_Limited
                 | ReleaseType_Theatrical
                 | ReleaseType_Digital
                 | ReleaseType_Physical
                 | ReleaseType_TV
  deriving (Eq,Show)

----------

instance FromJSON ReleaseType where
  parseJSON = withScientific "ReleaseType" $ \ v →
    case floatingOrInteger @Float @Word8 v of
      𝓡 1 → pure ReleaseType_Premiere
      𝓡 2 → pure ReleaseType_Theatrical_Limited
      𝓡 3 → pure ReleaseType_Theatrical
      𝓡 4 → pure ReleaseType_Digital
      𝓡 5 → pure ReleaseType_Physical
      𝓡 6 → pure ReleaseType_TV
      𝓡 i →
        fail $ [fmt|unknown release type code %d (%T)|] i tmdbReleaseTypeDocURI
      𝓛 i →
        fail $ [fmt|floating-point release type code %f (%T)|]
                 i tmdbReleaseTypeDocURI

----------

{- ordering set for preference of certification -}
instance Ord ReleaseType where
  compare = comparing toRank
            where toRank ReleaseType_Digital             = 0 ∷ Word8
                  toRank ReleaseType_Physical            = 1
                  toRank ReleaseType_TV                  = 2
                  toRank ReleaseType_Theatrical          = 3
                  toRank ReleaseType_Theatrical_Limited  = 4
                  toRank ReleaseType_Premiere            = 5

------------------------------------------------------------

newtype Certificate = Certificate 𝕋  deriving  (Eq,FromJSON,Show,ToJSON)

----------------------------------------

class    HasCertificate α           where certificate ∷ Lens' α Certificate
instance HasCertificate Certificate where certificate = lens id (flip const)

------------------------------------------------------------

data ReleaseDatum = ReleaseDatum { _rdCertification ∷ Certificate
                                 , _rdLanguage      ∷ Language
                                 , _rdReleaseDate   ∷ Date
                                 , _rdType          ∷ ReleaseType
                                 }
  deriving (Eq,Show)

----------

instance FromJSON ReleaseDatum where
  parseJSON = withObject "ReleaseDatum" $ \ v →
    ReleaseDatum ⊳ v .: "certification" ⊵ v .: "iso_639_1" ⊵ v .: "release_date" ⊵ v .: "type"

----------

instance Ord ReleaseDatum where
  compare = comparing (\ rd → (_rdType rd,_rdReleaseDate rd))

----------

instance HasLanguage ReleaseDatum where
  language = lens _rdLanguage (\ rd l → rd { _rdLanguage = l })

----------------------------------------

instance HasCertificate ReleaseDatum where
  certificate = lens _rdCertification (\ rd c → rd { _rdCertification = c })

----------------------------------------

type ReleaseData = [ReleaseDatum]

class    HasReleaseDates α             where releaseDates ∷ Lens' α ReleaseData
instance HasReleaseDates ReleaseData where releaseDates = lens id (flip const)

------------------------------------------------------------

data ReleaseDateResult = ReleaseDateResult { _rdrCountry      ∷ Country
                                           , _rdrReleaseDates ∷ ReleaseData }
  deriving (Eq,Show)

----------

instance Ord ReleaseDateResult where compare = comparing (view country)

----------

instance FromJSON ReleaseDateResult where
  parseJSON = withObject "ReleaseDateResult" $ \ v →
    ReleaseDateResult ⊳ v .: "iso_3166_1" ⊵ v .: "release_dates"

----------

instance HasCountry ReleaseDateResult where
  country = lens _rdrCountry (\ rdr c → rdr { _rdrCountry = c })

----------

instance HasReleaseDates ReleaseDateResult where
  releaseDates = lens _rdrReleaseDates (\ rdr rds → rdr { _rdrReleaseDates=rds })

------------------------------------------------------------

class HasReleaseDateResults α where
  releaseDateResults ∷ Lens' α [ReleaseDateResult]

----------

instance HasReleaseDateResults [ReleaseDateResult] where
  releaseDateResults = lens id (flip const)

------------------------------------------------------------

-- | Data types for IMDB responses

data TitleResponse = TitleResponse { _trTitle     ∷ Title
                                   , _trRelease   ∷ Release
                                   , _trRuntime   ∷ Runtime
                                   , _trOverview  ∷ Overview
                                   , _trGenres    ∷ [Genre]
                                   , _trCast      ∷ Cast
                                   , _trDirectors ∷ Directors
                                   , _trPosters   ∷ Posters
                                   , _trReleases  ∷ [ReleaseDateResult]
                                   }
  deriving Show

----------

instance FromJSON TitleResponse where
  parseJSON = withObject "TitleResponse" $ \ v → do
    let getDirectors ∷ [Aeson.Value] → AesonTypes.Parser Directors
        getDirectors c = let filt p = (T.toLower $ unJob (p ⊣ job)) ≡ "director"
                         in  Directors ⊳ filter filt ⊳ mapM Aeson.parseJSON c

    credits       ← v .: "credits"
    crew          ← credits .: "crew"
    images        ← v .: "images"
    release_dates ← v .: "release_dates"

    TitleResponse ⊳ v .:  "title"
                  ⊵ v .:  "release_date"
                  ⊵ Runtime ∘ MINS ⊳ v .: "runtime"
                  ⊵ Overview       ⊳ v .: "overview"
                  ⊵ v .: "genres"
                  ⊵ credits .: "cast"
                  ⊵ getDirectors crew
                  ⊵ images .: "posters"
                  ⊵ release_dates .: "results"

----------

instance HasTitle TitleResponse where
  title = lens _trTitle (\ tr t → tr { _trTitle = t })

----------

instance HasRelease TitleResponse where
  release = lens _trRelease (\ tr r → tr { _trRelease = r })

----------

instance ToReleaseYear TitleResponse where
  releaseYear = year ∘ view release

----------

instance HasRuntime TitleResponse where
  runtime = lens _trRuntime (\ tr r → tr { _trRuntime = r })

----------

instance HasOverview TitleResponse where
  overview = lens _trOverview (\ tr o → tr { _trOverview = o })

----------

instance HasGenres TitleResponse where
  genres = lens _trGenres (\ tr o → tr { _trGenres = o })

----------

instance HasCast TitleResponse where
  cast = lens _trCast (\ tr c → tr { _trCast = c })

----------

instance HasDirectors TitleResponse where
  directors = lens _trDirectors (\ tr ds → tr { _trDirectors = ds })

----------

instance HasPosters TitleResponse where
  posters = lens _trPosters (\ tr ps → tr { _trPosters = ps })

----------

instance HasReleaseDateResults TitleResponse where
  releaseDateResults = lens _trReleases (\ tr rdrs → tr { _trReleases = rdrs })

--------------------

instance ToPathComponent TitleResponse where
  pathComponent r = titleFilename (r ⊣ title) (𝓙 ∘ year $ r ⊣ release ∘ date)

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

class HasRating α where
  rating ∷ Lens' α Rating

------------------------------------------------------------

class HasQueryParams α where
  queryParams ∷ Lens' α [QueryParam]

------------------------------------------------------------

newtype MDInternalLink = MDInternalLink { unMDInternalLink ∷ PathComponent }

instance ToJSON MDInternalLink where
  toJSON = Aeson.String ⊳ mdInternalLink ∘ unMDInternalLink

------------------------------------------------------------

newtype Title = Title { _unTitle ∷ 𝕋 }  deriving  (FromJSON, Show, ToJSON)

----------

class HasTitle α where
  title ∷ Lens' α Title

----------

instance Printable Title where print = P.text ∘ _unTitle

------------------------------------------------------------

data FrontMatter = FrontMatter { _fmTMDB_ID     ∷ TMDB_ID
                               , _fmTitle       ∷ Title
                               , _fmCover       ∷ 𝕄 MDInternalLink
                               , _fmCertificate ∷ 𝕄 Certificate
                               , _fmOverview    ∷ 𝕋
                               , _fmRelease     ∷ Release
                               , _fmYear        ∷ Year
                               , _fmDuration    ∷ 𝕋
                               , _fmGenres      ∷ [Genre]
                               , _fmCast        ∷ Cast
                               , _fmDirectors   ∷ Directors
                               }
  deriving Generic

----------

instance ToJSON FrontMatter where
  toJSON = let
               drop_fm t = case splitAt 3 t of
                             ("_fm", t') → case uncons t' of
                                             𝓙 (c,t'') → Char.toLower c : t''
                                             𝓝         → t'
                             _           → t
               mdfy "tMDB_ID" = "tmdb"
               mdfy t         = dropWhileEnd (≡'\'') t
               opts = defaultOptions { fieldLabelModifier = mdfy ∘ drop_fm
                                     , omitNothingFields = 𝓣 }
           in  genericToJSON opts

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

newtype TMDB_ID = TMDB_ID ℕ  deriving  Show

--------------------

instance Printable TMDB_ID where
  print (TMDB_ID i) = P.text $ [fmt|%07d|] i

--------------------

instance ToJSON TMDB_ID where
  toJSON (TMDB_ID i) = Aeson.Number (fromIntegral i)

--------------------

--------------------

instance Textual TMDB_ID where
  -- this is so that, we can parse, e.g.,
  -- https://api.themoviedb.org/3/movie/717151...
  -- on the cmdline
  textual =
    TMDB_ID ⊳ (read ⊳ (optional tmdbTitlePrefix⋫ string ""⋫ digits ⋪many anyChar))

--------------------

instance ToPathPiece TMDB_ID where
  toPathPiece  = either (error ∘ show) id ∘ mkPathPiece ∘ toText

------------------------------------------------------------

data Overwrite  = Overwrite | NoOverwrite deriving (Eq,Show)

------------------------------------------------------------

data EType = Movie | TV deriving Show

------------------------------------------------------------

-- | Command line options
data Options = Options { tts        ∷ [TMDB_ID]
                       , people     ∷ [Person]
                       , seen       ∷ [Person]
                       , _overwrite ∷ Overwrite
                       , _eType     ∷ EType
                       }
  deriving Show

--------------------

overwrite ∷ Lens' Options Overwrite
overwrite = lens _overwrite (\ o w → o { _overwrite = w })

--------------------

etype ∷ Lens' Options EType
etype = lens _eType (\ o e → o { _eType = e })

----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ some (textualArgument (metavar "TMDB ID"))
          ⊵ nub ⊳ many (textualOption (ю [ short 'w', long "wants", long "want"
                                          , help "wants to see" ]))
          ⊵ nub ⊳ many (textualOption (ю [ short 'h', long "has-seen",
                                           long "seen"
                                         , help "has seen" ]))
          ⊵ flag NoOverwrite Overwrite (ю [ short 'O', long "overwrite"
                                          , help "overwrite existing entry" ])
          ⊵ flag Movie TV (ю [ long "tv", help "TV, not movies" ])

------------------------------------------------------------

fromPC ∷ (Element α ~ PathComponent, FromSeqNonEmpty α) => PathComponent → α
fromPC = fromSeqNE ∘ pure

----------------------------------------

newtype ApiKey = ApiKey { unApiKey ∷ RText 'QueryValue }

minImageWidth ∷ ImageWidth
minImageWidth = ImageWidth 500

imageWidthFilt ∷ HasImageWidth ξ => ξ → 𝔹
imageWidthFilt i = (i ⊣ imageWidth) ≥ minImageWidth

{-| where to look for info about release types -}
tmdbReleaseTypeDocURI ∷ MyURI
tmdbReleaseTypeDocURI =
  MyURI [uri|https://developer.themoviedb.org/reference/movie-release-dates|]

-- | TMDB API base URL
tmdbApiBase ∷ MyURI
-- tmdbApiBase = MyURI [uri|https://api.themoviedb.org/3|]
tmdbApiBase = MyURI [uri|https://api.themoviedb.org/3|]

tmdbURIConfiguration ∷ MyURI
tmdbURIConfiguration = tmdbApiBase‡([pathPiece|configuration|]∷RText 'PathPiece)

-- | TMDB API base URL
tmdbApiTypeBase ∷ EType → MyURI
tmdbApiTypeBase Movie = MyURI [uri|https://api.themoviedb.org/3/movie|]
tmdbApiTypeBase TV    = MyURI [uri|https://api.themoviedb.org/3/tv|]

-- | IMDB API base URL
imdbApiBase ∷ MyURI
imdbApiBase = MyURI [uri|https://api.imdbapi.dev/titles|]

-- | IMDB common interactive lookup prefix
imdbTitlePrefix ∷ CharParsing η => η 𝕊
imdbTitlePrefix = string "https://www.imdb.com/title/"

-- | IMDB common interactive lookup prefix
tmdbTitlePrefix ∷ CharParsing η => η 𝕊
tmdbTitlePrefix = (◇) ⊳ string "https://www.themoviedb.org/"
                      ⊵ choice [ string "tv", string "movie" ]

----------------------------------------

{-| read a file, which should have a single line of text; excluding lines that
    are may have some or none whitespace possibly preceding a comment that is
    denoted by a leading '#' -}
readTokenFile ∷ ∀ ε ω μ .
                (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                 MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) =>
                AbsFile → DoMock → μ 𝕋
readTokenFile fn do_mock =
  let filter_blank_comment =
        let is_blank_comment t = case T.uncons (T.stripStart t) of
                                   𝓝          → 𝓕
                                   𝓙 ('#', _) → 𝓕
                                   _          → 𝓣
        in  filter is_blank_comment
  in  filter_blank_comment ⊳ (readFile Debug 𝓝 (return []) fn do_mock)≫ \ case
    []         → throwUserError $ [fmtT|no usable lines found in %T|] fn
    [one_line] → return one_line
    _          → throwUserError $ [fmtT|too many lines found in %T|] fn


----------------------------------------

parseRequest ∷ ∀ ε ρ ω μ . (MonadIO μ, Default ω, HasIOClass ω, HasDoMock ω,
                            MonadLog (Log ω) μ,
                            HasAPIReadToken ρ, MonadReader ρ μ,
                            AsIOError ε, Printable ε, MonadError ε μ) =>
               MyURI → μ HTTP.Request

parseRequest uri_ = do
  api_read_token ← unAPIReadToken ⊳ asks (view apiReadToken)
  r ← eitherME (userE ∘ show) $ HTTP.parseRequest ∘ renderStr $ unMyURI uri_
  return $ HTTP.addRequestHeader "Authorization" ("Bearer " ◇ api_read_token) r

----------------------------------------

fetchResponse ∷ ∀ ε ρ μ .
                (MonadIO μ, AsIOError ε, MonadError ε μ, Printable ε,
                 HasAPIReadToken ρ, MonadReader ρ μ,
                 MonadLog (Log MockIOClass) μ) =>
                MyURI → DoMock → μ ByteString
fetchResponse uri_ do_mock = do
  debugIO do_mock $ [fmtT|fetching URI: %T|] uri_
  case do_mock of
    DoMock → return ""
    NoMock → do
      response ← parseRequest uri_ ≫ HTTP.httpBS
      let status = HTTP.getResponseStatusCode response
      debugT $ [fmt|got status %d for URI: %T|] status uri_
      if status == 200
      then return $ HTTP.getResponseBody response
      else throwUserError $ "HTTP error: " ◇ show status

----------------------------------------

fetchJSON ∷ ∀ ε α ρ μ .
            (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ, FromJSON α,
             HasAPIReadToken ρ, MonadReader ρ μ,
             MonadLog (Log MockIOClass) μ) =>
            MyURI → μ α
fetchJSON uri_ = do
  (Aeson.eitherDecode ∘ BSS.fromStrict) ⊳ fetchResponse uri_ NoMock ≫ \ case
    𝓛 err    → throwUserError $ "Error decoding JSON: " ◇ err
    𝓡 result → return $ result

----------------------------------------

-- | Sanitize title for filename
titleFilename ∷ Title → 𝕄 Year → PathComponent
titleFilename ttle y =
  let nme = let breakReplace = T.breakOn " "∘T.replace "/" "-"∘T.replace ":" "-"
            in  case breakReplace (toText ttle) of
                  ("The", rest) → T.dropWhile (≡' ') rest ◇ "," ◇ "The"
                  ("A",   rest) → T.dropWhile (≡' ') rest ◇ "," ◇ "A"
                  ("An",  rest) → T.dropWhile (≡' ') rest ◇ "," ◇ "An"
                  (ini,   rest) → ini ◇ rest
      year_text = "" ⧐ ([fmt|  (%T)|] ⊳ y)
  in  __parse__ $ nme ◇ year_text

----------------------------------------

-- | Format duration as 1h00m
formatDuration ∷ Duration → 𝕋
formatDuration (MINS mins) =
  let (hours∷Word8,minutes) = floor mins `divMod` 60
  in T.pack $ show hours ◇ "h" ◇ show minutes ◇ "m"

----------------------------------------

{-| execute an external process, don't redirect out/err, nothing on stdin,
    expect 0 exit -}
ꙭ ∷ ∀ ε δ α μ . (MonadIO μ, ToMLCmdSpec α (), MonadLog (Log MockIOClass) μ,
                 MonadReader δ μ, HasDoMock δ,
                 AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                 AsProcExitError ε, Printable ε,
                 MonadError ε μ) =>
    α → μ ()

ꙭ = snd ⩺ ꙩ

----------------------------------------

-- | Download and resize an image using ImageMagick's `magick` command
-- downloadAndResizeImage ∷ 𝕋 → FilePath → IO ()
downloadAndResizeImage ∷ ∀ ε ρ μ .
                         (MonadIO μ, HasDoMock ρ, MonadReader ρ μ,
                          MonadLog (Log MockIOClass) μ, HasAPIReadToken ρ,
                          AsFPathError ε, AsIOError ε,
                          AsCreateProcError ε,AsProcExitError ε,
                          Printable ε, MonadError ε μ) =>
                         MyURI → AbsFile → μ ()

downloadAndResizeImage image_uri target_path = do
  do_mock ← asks (view doMock)
  -- Download the image to a temporary file
  let temp_file_path = target_path ⊙ [pc|tmp|]
  body ← fetchResponse image_uri do_mock
  writeFile Debug 𝓝 (𝓙 0o644) temp_file_path body do_mock

  -- Use ImageMagick to resize the image
  ꙭ ([absfile|/run/current-system/sw/bin/magick|],
     [toText temp_file_path, "-resize", "600x400>", toText target_path])

  -- Remove the temporary file
  unlink Debug temp_file_path do_mock

----------------------------------------

writeMD ∷ ∀ ε α ρ μ .
          (MonadIO μ, Printable α, MonadLog (Log MockIOClass) μ, MonadCatch μ,
           HasDoMock ρ,MonadReader ρ μ,AsIOError ε,Printable ε,MonadError ε μ)=>
          𝕄 α → [𝕋] → AbsFile → μ ()
writeMD yaml_m lines fn = do
  let content = maybe "" [fmtT|---\n%T---\n|] yaml_m ◇ T.unlines lines
  infoT $ [fmt|content: %t|] content
  asks (view doMock) ≫ writeFile Debug 𝓝 (𝓙 0o644) fn content

----------------------------------------

mdInternalLink ∷ PathComponent → 𝕋
mdInternalLink = [fmt|[[%T]]|]

----------------------------------------

writeMovie ∷ ∀ ε ρ μ .
             (MonadIO μ, MonadLog (Log MockIOClass) μ, MonadCatch μ,
              HasDoMock ρ, MonadReader ρ μ, AsIOError ε, Printable ε,
              MonadError ε μ) =>
             TMDB_ID → 𝕄 Certificate → TitleResponse → 𝕄 AbsFile → AbsFile → μ ()
writeMovie tt cert title_response poster_fn_y fn = do
  let fm = FrontMatter
        { _fmTMDB_ID     = tt
        , _fmTitle       = title_response ⊣ title
        , _fmCertificate = cert
        , _fmOverview    = unOverview $ title_response ⊣ overview
        , _fmYear        = releaseYear title_response
        , _fmRelease     = title_response ⊣ release
        , _fmDuration    = formatDuration (title_response ⊣ runtime ∘ duration)
        , _fmGenres      = title_response ⊣ genres
        , _fmCast        = takeCast 5 $ title_response ⊣ cast
        , _fmDirectors   = title_response ⊣ directors
        , _fmCover       =
            MDInternalLink ∘ SeqNE.last ∘ toSeqNE ∘ basename ⊳ poster_fn_y
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
          return $ fst ⊳ BSS.uncons bs

--------------------

readLastByte' ∷ ∀ ε δ μ .
                (MonadIO μ, HasNamedHandle δ, AsIOError ε, MonadError ε μ) =>
                δ → μ (𝕄 ℂ)
readLastByte' h =
  asIOError $ (toEnum ∘ fromIntegral) ⊳⊳ readLastByte (h ⊣ handle)

--------------------

getLastByte ∷ ∀ ε γ ω μ .
              (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, Printable ε,
               HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) =>
              γ → μ (𝕄 ℂ)
getLastByte fn =
  withFile Debug 𝓝 Binary FileR (return 𝓝) fn readLastByte' NoMock

----------------------------------------

{-| Ensure that a file ends in a newline, *if it exists and is a file*.
    If it doesn't exist: do nothing.
    If it exists, but is empty: do nothing.
    If it is not a file (or appending does not work): error.
-}
ensureTrailingNewline ∷ ∀ ε γ ρ ω μ .
                        (MonadIO μ, FileAs γ, AsIOError ε, Printable ε,
                         MonadError ε μ,
                         HasDoMock ρ, MonadReader ρ μ,
                         HasDoMock ω, HasIOClass ω, Default ω,
                         MonadLog (Log ω) μ) =>
                        γ → μ ()
ensureTrailingNewline fn = do
  do_mock ← asks (view doMock)
  fexists Debug FExists fn NoMock ≫ \ case
    NoFExists → return ()
    FExists → getLastByte fn ≫ \ case
      𝓝      → return ()
      𝓙 '\n' → return ()
      𝓙 _    → appendFile Debug 𝓝 𝓝 fn ("\n"∷𝕋) do_mock

----------------------------------------

{-| Parse a pandoc-markdown file, that is, a file with an optional set of YAML
    attributes at the top.

    If the file is prefixed with a "---" line, then the top is parsed as YAML,
    until any latter "---" line.  The second "---", and any subsequent text, is
    returned as lines.(&&)

    If the file is not prefixed with a "---" line, then it's all returned as
    lines.
-}
parseMD ∷ ∀ ε γ ω μ .
          (MonadIO μ, FileAs γ, AsIOError ε, Printable ε, MonadError ε μ,
           HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) =>
          γ → μ (𝕄 Object, [𝕋])
parseMD fn = do
  readFile Debug 𝓝 (return []) fn NoMock ≫ \ case
    ("---" : xs) → let (header, rest) = span (≢ "---") xs
                   in  case decodeEither' (encodeUtf8 $ T.unlines header) of
                         𝓡 o → return (𝓙 o,rest)
                         𝓛 e → throwUserError $
                                 [fmtT|failed to decode header in %T (%w)|] fn e
    xs           → return (𝓝, xs)

----------------------------------------

{-| A bit like `mkpath`, but only one level.  Ensure that a given directory
    exists, creating it if necessary (but not creating parents). -}
ensureDir ∷ ∀ ε δ ρ ω μ . (MonadIO μ, DirAs δ, AsIOError ε, Printable ε,
                           MonadError ε μ,
                           HasDoMock ρ, MonadReader ρ μ,
                           MonadLog (Log ω) μ, Default ω, HasIOClass ω,
                           HasDoMock ω) =>
            δ → μ ()
ensureDir d = do
  do_mock ← asks (view doMock)
  ftype ⊳⊳ stat Debug 𝓝 d NoMock ≫ \ case
    𝓝           → mkdir Debug d 0o755 do_mock
    𝓙 Directory → return ()
    𝓙 ft        → throwUserError ([fmtT|not a directory: %T (got a %w)|] d ft)

----------------------------------------

appendText ∷ ∀ ε ρ ω μ . (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                          HasDoMock ρ, MonadReader ρ μ,
                          Default ω, HasDoMock ω, HasIOClass ω,
                          MonadLog (Log ω) μ) =>
             AbsFile → 𝕋 → μ ()
appendText file_name text =
  asks (view doMock) ≫ appendFile Debug 𝓝 (𝓙 0o644) file_name text

----------------------------------------

{-| Ensure that a given wiki link is present in a file, adding it at the end if
    necessary (see `appendInternalLink`); creating the file if it doesn't exist.
-}
ensureInternalLink ∷ ∀ ε ρ ω μ .
                     (MonadIO μ, AsIOError ε, Printable ε, MonadError ε μ,
                      HasDoMock ρ, MonadReader ρ μ,
                      Default ω,HasDoMock ω,HasIOClass ω, MonadLog (Log ω) μ) =>
                     PathComponent → AbsFile → μ ()
ensureInternalLink link fn = do
  ensureDir (fn ⊣ dirname)
  (_, lines) ← parseMD fn
  let text = mdInternalLink link
  case filter (text `T.isInfixOf`) lines of
    (_:_) → return ()
    []    → do
      let last_para = takeWhileEnd (≢"") lines
      ensureTrailingNewline fn
      when (any ("#" `T.isPrefixOf`) last_para) $ appendText fn "\n"
      appendText fn $ text ◇ "\n"

----------------------------------------

fetchImage ∷ ∀ ε ρ μ .
             (MonadIO μ, MonadLog (Log MockIOClass) μ,
              AsIOError ε, AsFPathError ε, AsCreateProcError ε,
              AsProcExitError ε, Printable ε,
              HasAPIReadToken ρ,MonadError ε μ,HasDoMock ρ,MonadReader ρ μ) =>
             Configuration → ImageWidth → PathComponent → AbsDir → Image
           → μ AbsFile

fetchImage cfg image_width file_title movies_dir img = do
  let jpg_fname         = fromPC (file_title ⊙ pathComponent img)
      attachment_dir    = movies_dir ⫻ [reldir|_attachments/|]
      image_target_path ∷ AbsFile = attachment_dir ⫻ jpg_fname
      image_uri         =
        (cfg ⊣ imageSecureBase) ‡ image_width
                                ‡ (img ⊣ imagePath)
                                & trailingSlash ⊢ NoTrailingSlash

  debugT $ [fmt|%T|] img
  debugT $ [fmt|%T → %T|] image_uri image_target_path

  fexists Debug NoFExists image_target_path NoMock ≫ \ case
    FExists   → infoT $ [fmtT|not re-downloading path: %T|] image_target_path
    NoFExists → do
      do_mock ← asks (view doMock)
      infoIO do_mock $ [fmtT|downloading image to: %T|] image_target_path
      when (do_mock ≡ NoMock) $ do
        body ← fetchResponse image_uri do_mock
        writeFile Debug 𝓝 (𝓙 0o644) image_target_path body do_mock
  return image_target_path

----------------------------------------

throwIOErr ∷ ∀ ε γ α μ . (FPathAs γ, AsIOError ε, MonadError ε μ) =>
             IOErrorType → 𝕋 → 𝕄 Handle → 𝕄 γ → μ α
throwIOErr t s h p =
  throwError $ _IOErr # mkIOError t (toString s) h (toString ⊳ p)

throwAlreadyExists ∷ ∀ ε τ γ α μ .
                     (Printable τ, FPathAs γ, AsIOError ε, MonadError ε μ) =>
                     τ → γ -> μ α
throwAlreadyExists s = throwIOErr alreadyExistsErrorType (toText s) 𝓝 ∘ 𝓙

----------------------------------------

writePeopleLinks ∷ ∀ ε ω ρ μ .
                   (MonadIO μ, HasDoMock ρ, MonadReader ρ μ,
                    Default ω, HasDoMock ω, HasIOClass ω, MonadLog (Log ω) μ,
                    AsIOError ε, Printable ε, MonadError ε μ) =>
                   PathComponent → AbsDir → Options → μ ()
writePeopleLinks file_title info_dir opts = do
  let people_dir     = info_dir ⫻ [reldir|people/|]
      person_dir p   = people_dir ⫻ fromList [personComponent p]
      person_fn bf p = person_dir p ⫻ __parse__ (bf (personPrefix p))

  forM_ (people opts) $
    ensureInternalLink file_title ∘ person_fn [fmtT|%t-wants-to-see.md|]

  forM_ (seen opts) $
    ensureInternalLink file_title ∘ person_fn [fmtT|%t-has-seen.md|]



----------------------------------------

{-| languages I work with, in preference order -}
myLanguages ∷ Vector Language
myLanguages = fromList [Language_EN, Language_NONE]

----------------------------------------

languageFilt ∷ HasLanguage ξ => ξ → 𝔹
languageFilt l = (l ⊣ language) ∈ myLanguages

----------------------------------------

{-| countries I work with, in preference order -}
myCountries ∷ Vector Country
myCountries = fromList [Country_GB, Country_US, Country_NONE]

----------------------------------------

countryFilt ∷ HasCountry ξ => ξ → 𝔹
countryFilt c = (c ⊣ country) ∈ myCountries

----------------------------------------

processTitle ∷ ∀ ε ρ μ .
               (MonadIO μ, MonadLog (Log MockIOClass) μ, MonadCatch μ,
                HasDoMock ρ, HasAPIReadToken ρ, MonadReader ρ μ,
                AsFPathError ε, AsIOError ε, AsCreateProcError ε,
                AsProcExitError ε, Printable ε, MonadError ε μ) =>
               Configuration → AbsDir → Options → ImageWidth → TMDB_ID → μ ()
processTitle cfg info_dir opts image_width tt = do
  let params    = pure $ QueryParam [queryKey|append_to_response|]
                                    [queryValue|credits,images,release_dates|]
      title_uri = tmdbApiTypeBase (opts ⊣ etype) ‡ ҩ tt & queryParams ⊢ params
  title_response ← fetchJSON title_uri
  infoT $ [fmt|fetched title (%T): %T|] tt (title_response ⊣ title)
  debugT $ [fmt|title_response: %w|] title_response

  let ttle             = title_response ⊣ title
      file_title       = pathComponent title_response
      movies_dir       = info_dir ⫻ [reldir|movies/|]
      md_fname         = fromPC (pathComponent title_response ⊙ [pc|md|])
      target_path      = movies_dir ⫻ md_fname
      pfilt       p    = and [ languageFilt p, countryFilt p, imageWidthFilt p ]
      pcmp        x y  = compare (x⊣language,x⊣country,y⊣voteAvg,y⊣imageWidth)
                                 (y⊣language,y⊣country,x⊣voteAvg,x⊣imageWidth)
      my_posters       =
        sortBy pcmp ∘ filter pfilt ∘ unPosters $ title_response ⊣ posters
      rfilt       r    = and [countryFilt r,or(languageFilt ⊳ r ⊣ releaseDates)]
  my_release ← case sort ∘ filter rfilt $ title_response ⊣ releaseDateResults of
                 []    → throwUserError $ [fmtT|%T: no releases found|] ttle
                 (x:_) → return x

  cert ← case sort ∘ filter languageFilt $ my_release ⊣ releaseDates of
    []    → do warnT$ [fmt|%T: release has no release dates! %w|] ttle my_release
               return 𝓝
    (x:_) → return ∘ 𝓙 $ x ⊣ certificate

  when (opts ⊣ overwrite ≠ Overwrite) $
    ((≡ FExists) ⊳ fexists Debug NoFExists target_path NoMock) ≫ flip when
      (throwAlreadyExists tt target_path)

  infoT $ [fmt|writing file %T (%T)|] target_path ttle

  let fetch_image = fetchImage cfg image_width file_title movies_dir
  poster_fn_y ← case nonEmpty my_posters of
                  𝓝     → return 𝓝
                  𝓙 m_p → 𝓙 ∘ NonEmpty.head ⊳ forM m_p fetch_image

  debugT $ [fmt|poster_fn_y: %w|] poster_fn_y

  writeMovie tt cert title_response poster_fn_y target_path
  writePeopleLinks file_title info_dir opts

----------------------------------------

addMovies ∷ AbsDir → AbsDir
addMovies = (⫻[reldir|movies/|])

----------------------------------------

checkMoviesDir ∷ ∀ ε μ .
         (MonadIO μ, MonadLog (Log MockIOClass) μ,
          AsIOError ε,
          Printable ε, MonadError ε μ) =>
         AbsDir → μ ()
checkMoviesDir (addMovies → d) = do
  let isDir st = ftype st ≡ Directory
  stat Debug 𝓝 d NoMock ≫ \ case
    𝓙 st | isDir st → return ()
    𝓙 st        → throwUserError $ [fmtT|not a directory: %T (got %T)|] d st
    𝓝           →
      throwUserError $ [fmtT|no such dir: %T; run this in a movies-info dir|] d

----------------------------------------

{-| Find from the `Configuration` the least wide `ImageWidth` that is larger than
    the given `ImageWidth` -}
getLargerImageWidth ∷ ∀ ε δ η . (HasPosterSizes δ,AsIOError ε,MonadError ε η) =>
                      ImageWidth → δ → η ImageWidth
getLargerImageWidth iw cfg =
  let poster_sizes = unPosterSizes $ cfg ⊣ posterSizes
  in  case sort ∘ filter (≥ iw) ∘ catMaybes $ (⩼ _ImageWidth) ⊳ poster_sizes of
        (ps:_) → return ps
        []     → throwUserError $ [fmtT|no poster sizes found > %w: %w|] iw
                                  (cfg ⊣ posterSizes)

----------------------------------------

doMain ∷ ∀ ε μ .
         (MonadIO μ, MonadLog (Log MockIOClass) μ, MonadCatch μ,
          AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
          Printable ε, MonadError ε μ) =>
         DoMock → Options → μ ()
doMain do_mock opts = do
  cwd ← getCwd
  checkMoviesDir cwd

  api_read_token_fn ← homePath [relfile|.tmdb-api.read-access-token|]
  api_read_token ← APIReadToken ∘ encodeUtf8 ⊳
                   readTokenFile api_read_token_fn NoMock
  if null (tts opts)
  then throwUserError @_ @𝕋 "no titles provided"
  else
    let ctxt = Context { _doMock = do_mock, _apiReadToken = api_read_token }
    in  flip runReaderT ctxt $ do
      cfg ∷ Configuration ← fetchJSON tmdbURIConfiguration
      debugT $ [fmt|configuration: %w|] cfg
      image_width ← getLargerImageWidth minImageWidth cfg
      debugT $ [fmt|image_width: %w|] image_width
      forM_ (tts opts) $ processTitle cfg cwd opts image_width

----------------------------------------

main ∷ IO ()
main = let progDesc ∷ 𝕋 = "add a new film to the obsidian movies library"
       in  stdMainSimple progDesc parseOptions doMain

-- that's all, folks! ----------------------------------------------------------
