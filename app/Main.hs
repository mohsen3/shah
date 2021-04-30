{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Executor
import Types as T

import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad (join, foldM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Options.Applicative as O

main :: IO ()
main = do
    content <- BS.readFile "shah.yml"
    let parsedContent = Y.decodeEither content :: Either String DevDefinition
    case parsedContent of
        Left err -> error err
        Right defs@DevDefinition{..} -> do
            let
                g = fromMaybe defaultGlobal global
                infoMod = mconcat $ catMaybes [Just fullDesc, O.header <$> T.header g, O.progDesc <$> T.help (g :: Global)]
            join $ execParser (info (opts g commands) infoMod)
