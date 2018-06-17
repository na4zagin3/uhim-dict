module Language.UHIM.Dictionary.Transform.Tcvime where

import Control.Lens
import Data.Maybe
import qualified Data.Map as M
import Language.UHIM.Japanese.Prim
import qualified Language.UHIM.Dictionary.Keybind.Vim as Vim
import Language.UHIM.Dictionary.Yaml

data Config = Config { name :: String
                     , layoutName :: String
                     , extractConfig :: ExtractConfig
                     }

data ExtractConfig = ExtractConfig { kanjiExtractor :: KanjiShapes -> Maybe Kanji
                                   }

defaultConfig :: Config
defaultConfig = Config { name = ""
                       , layoutName = "TUT"
                       , extractConfig = ExtractConfig { kanjiExtractor = extractKyuKanji
                                                       }
                       }

type KeyEntry = (String, String, String)

extractConvEntry :: ExtractConfig -> String -> (Position, DictEntry) -> [Vim.Mapping]
extractConvEntry c layout (pos, Entry字 decl) = maybeToList $ do
  m <- decl^.decl鍵
  kanji <- kanjiExtractor c $ decl^.decl體
  k <- M.lookup layout m
  return $ Vim.Mapping k kanji $ show pos
extractConvEntry _ _ _ = []

extract :: Config -> Dictionary -> [(FilePath, String)]
extract c dict = [(name c ++ ".vim", Vim.emit keyMap)]
  where
    keyMap = Vim.KeyMap { Vim.name = name c
                        , Vim.mappings = tuts
                        }
    tuts = concatMap (extractConvEntry ec $ layoutName c) dict
    ec = extractConfig c
