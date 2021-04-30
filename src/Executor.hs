{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Executor where

import Types as T
import Options.Applicative as O
import Data.Semigroup ((<>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Char (toUpper)
import Data.Traversable (sequenceA)
import Control.Monad (join, foldM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson as A
import qualified Data.Aeson.Text as A
import Data.List (find)

import qualified Data.Text as Text
import qualified Data.Text.Lazy  as Text hiding (Text, pack, unpack)
import qualified Data.Text.Lazy.Builder as Text

import qualified Text.Karver as Karver

import qualified System.Environment as System
import qualified System.Process as System
import qualified System.IO as System


mkParser :: [CmdArg] -> Parser Binding
mkParser cmdArgs = Binding <$> (sequenceA $ map mkParser1 cmdArgs)
    where
        boolToSwitch :: String -> Bool -> String
        boolToSwitch switchName flag = if flag then "--" ++ switchName else ""

        mkParser1 :: CmdArg -> Parser NamedValue
        mkParser1 CmdArg{..} =
            let
                argMod :: Show b => Mod a b
                argMod = O.help (fromMaybe "No help available" help) <>
                         O.showDefault

                shortName :: HasName f => Mod f a
                shortName = maybe mempty O.short short

                valueMod :: HasValue f => Mod f String
                valueMod = maybe mempty O.value defaultValue
            in
                fmap (SimpleNamedValue name) $
                case kind of
                     Positional -> argument str (metavar (map toUpper name) <> argMod <> valueMod)
                     Flag -> fmap (boolToSwitch name) (switch (long name <> argMod <> shortName))
                     Named -> option str (long name <> argMod <> shortName <> valueMod)

runCommand :: Global -> CommandDefinition -> Binding -> IO ()
runCommand Global{environmentFrom=globalEnvFrom, interpreter=globalInterpreter}
           CommandDefinition{environment, commands, environmentFrom=cmdEnvFrom, interpreter=cmdInterpreter}
           binding = do
    processEnv <- System.getEnvironment
    globalEnv <- loadEnvFromFile globalEnvFrom
    commandEnv <- loadEnvFromFile cmdEnvFrom
    let sanitizedEnv = map (getEnvironment (processEnv <> commandEnv <> globalEnv)) environment
    foldM_ (runCmd sanitizedEnv) binding commands
    where
        getEnvironment :: [(String, String)] -> EnvironmentVariable -> (String, String)
        getEnvironment cmdEnvironment EnvironmentVariable{..} =
            let def = (name, fromMaybe (error $ "Environment variable " <> name <> " must be set") defaultValue)
            in  fromMaybe def $ find ((name ==) . fst) cmdEnvironment

        encodeToJson :: A.ToJSON a => a -> Text.Text
        encodeToJson = Text.toStrict . Text.toLazyText . A.encodeToTextBuilder

        renderTemplate :: Binding -> String -> String
        renderTemplate binding = Text.unpack . Karver.renderTemplate' (encodeToJson binding) . Text.pack

        runCmd env binding@(Binding bindingValues) CmdStep{step, output} = do
            let Interpreter executable args = fromMaybe globalInterpreter cmdInterpreter
                createProcess = (System.proc executable (args <> [renderTemplate binding step])){ System.env = Just env }
            (exitCode, stdOutStr, stdErrStr) <- System.readCreateProcessWithExitCode createProcess ""
            System.hPutStrLn System.stderr stdErrStr
            case output of
                Ignore -> return $ binding
                Pipe -> error "Pipe action not implemented yet!"
                Print -> do
                    putStrLn stdOutStr
                    return binding
                Capture -> return $ Binding $ SimpleNamedValue "previous_step" stdOutStr : bindingValues
                ParseJson -> do
                    let json = fromMaybe (error $ "Output not a valid json: " <> stdOutStr) (A.decode $ LBS.pack stdOutStr)
                    return $ Binding $ JsonNamedValue "previous_step" json : bindingValues

        loadEnvFromFile :: Maybe String -> IO [(String, String)]
        loadEnvFromFile Nothing = return []
        loadEnvFromFile (Just filePath) = do
            content <- BS.readFile filePath
            case Y.decodeEither content of
                Right vars -> return vars
                Left err -> error $ "Cannot decode environment variable file " <> filePath <> " -- " <> err

mkCommand :: Global -> CommandDefinition -> Mod CommandFields (IO ())
mkCommand g cmdDef@CommandDefinition{..} =
    command name (info (runCommand g cmdDef <$> (mkParser args)) (fullDesc <> progDesc help))

opts :: Global -> [CommandDefinition] -> Parser (IO ())
opts global cmdDefs = helper <*> (hsubparser $ mconcat $ map (mkCommand global) cmdDefs)

