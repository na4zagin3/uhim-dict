module Language.UHIM.Dictionary.Yaml.Prim where

import Data.Aeson.TH

jsonOptions :: Options
jsonOptions = defaultOptions{omitNothingFields = True}
