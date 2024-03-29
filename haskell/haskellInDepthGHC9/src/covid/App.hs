module App
  ( parseByteStream,
    parseZipppedCsv,
  )
where

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import CovidData
import CovidStats
import CsvParser
import Data.Attoparsec.ByteString.Streaming qualified as AB
import Data.Function
import Data.List (intercalate)
import Data.Map qualified as M
import Streaming
import Streaming.ByteString (ByteStream)
import Streaming.ByteString.Char8 qualified as SB
import Streaming.Prelude qualified as S
import Streaming.Zip qualified as S

parseGroup :: Monad m => Stream (Of CountryData) m a -> m (Of (Maybe CountryData) a)
parseGroup = S.next >=> either (pure . (Nothing :>)) doParse
  where
    doParse (cdat, rest) =
      rest
        & S.map _days
        & S.foldMap id
        & (S.mapOf (Just . withDaysAndTotals cdat) <$>)

parseZipppedCsv :: (MonadResource m, MonadThrow m) => FilePath -> m ContinentStats
parseZipppedCsv =
  SB.readFile
    >>> S.gunzip
    >>> parseByteStream

parseByteStream :: forall m. (MonadIO m, MonadThrow m) => ByteStream m () -> m ContinentStats
parseByteStream str = do
  (cstats :> ps) <- doParse
  case ps of
    (Right ()) -> return cstats
    (Left ((s, e), _)) -> throwM $ userError $ intercalate "," s <> e
  where
    doParse =
      str
        & AB.parsed maybeCountryData
        & S.catMaybes
        -- & mapsM (\(g :> x) -> liftIO (print g) >> pure (g :> x))
        & S.groupBy ((==) `on` _iso_code)
        & mapsM parseGroup
        & S.catMaybes
        & S.fold byContinent M.empty id
