module Language.UHIM.Dictionary.Transform.TcEl where

import qualified Language.UHIM.Dictionary.Transform.Tcvime as TVim
import qualified Language.UHIM.Dictionary.Keybind.TcEl as TE
import Language.UHIM.Dictionary.Yaml

data Config = Config { name :: String
                     , indicator :: String
                     , layoutName :: String
                     , extractConfig :: TVim.ExtractConfig
                     }

defaultConfig :: Config
defaultConfig = Config { name = ""
                       , indicator = " TUT"
                       , layoutName = "TUT"
                       , extractConfig = TVim.ExtractConfig { TVim.kanjiExtractor = extractKyuKanji
                                                       }
                       }

extract :: Config -> Dictionary -> [(FilePath, String)]
extract c dict = [(name c ++ "-tbl.el", TE.emit keyMap)]
  where
    keyMap = TE.KeyMap { TE.name = name c
                       , TE.indicator = indicator c
                       , TE.mappings = tuts
                       }
    tuts = concatMap (TVim.extractConvEntry ec $ layoutName c) dict
    ec = extractConfig c
