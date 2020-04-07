{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Maple.Staticize where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Tuple
import           Linear
import           Maple.AABB
import           Maple.Config
import           Maple.Map
import           System.Directory
import           System.FilePath

staticize :: forall c l s i v p b e m
           . (JS.ToJSON p, JS.ToJSONKey b, JS.ToJSON (I m), BinReader c (IGridMap2 v p b) l v p b m, MonadIO m)
          => (HasMapleConfig c (IGridMap2 v p b) l s i v p b e)
          => FilePath -> m ()
staticize t = do
  liftIO $ createDirectoryIfMissing True t
  (sv::Map b (BoundingBox V2 p)) <- view (binMap.igmBBs.to (Map.fromList . map (swap) . concatMap toList . toList))
  liftIO $ BL.writeFile (t</>"view") $ JS.encode sv
  let JS.ToJSONKeyText bidf' _ = JS.toJSONKey @b
  let dibf = T.unpack . bidf'
  liftIO $ createDirectoryIfMissing False (t</>"bin")
  forM_ (Map.keys sv) $ \b -> do
    bc <- map _dbmItem <$> tip b
    liftIO $ BL.writeFile (t</>"bin"</>(dibf b)) $ JS.encode bc
  sfs <- view mcImageFolders
  liftIO $ createDirectoryIfMissing False (t</>"static")
  forM_ sfs $ \f -> do
    liftIO $ putStrLn $ "Make sure to copy "<>f<>" into "<>(t</>"static/")
