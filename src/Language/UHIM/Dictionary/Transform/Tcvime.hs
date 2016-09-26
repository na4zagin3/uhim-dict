module Language.UHIM.Dictionary.Transform.Tcvime where

import Language.UHIM.Japanese.Prim
import qualified Language.UHIM.Dictionary.Keybind.Vim as Vim
import Language.UHIM.Dictionary.Yaml
import Data.Maybe
import qualified Data.Map as M

data Config = Config { name :: String
                     , extractConfig :: ExtractConfig
                     }

data ExtractConfig = ExtractConfig { kanjiExtractor :: KanjiShapes -> Maybe Kanji
                                   }

defaultConfig :: Config
defaultConfig = Config { name = ""
                       , extractConfig = ExtractConfig { kanjiExtractor = extractKyuKanji
                                                       }
                       }

type KeyEntry = (String, String, String)

extractConvEntry :: ExtractConfig -> String -> (Position, DictEntry) -> [Vim.Mapping]
extractConvEntry c layout (pos, Entry字 decl) = maybeToList $ do
  m <- kanji鍵 decl
  kanji <- kanjiExtractor c $ kanji體 decl
  k <- M.lookup layout m
  return $ Vim.Mapping k kanji $ show pos
extractConvEntry _ _ _ = []

extract :: Config -> Dictionary -> [(FilePath, String)]
extract c dict = [(name c ++ ".vim", Vim.emit keyMap)]
  where
    keyMap = Vim.KeyMap { Vim.name = name c
                        , Vim.mappings = tuts
                        }
    tuts = concatMap (extractConvEntry ec "TUT") dict
    ec = extractConfig c
