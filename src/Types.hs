{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( EnvironmentVariable(..)
    , CmdArgType(..)
    , CmdArg(..)
    , CommandDefinition(..)
    , DevDefinition(..)
    , NamedValue(..)
    , Global(..)
    , Binding(..)
    , Interpreter(..)
    , CmdStep(..)
    , CmdStepOutput(..)
    , defaultGlobal
    ) where

import GHC.Generics
import Control.Applicative ((<|>))
import Data.Aeson as A (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.=), (.:), (.:?), (.!=))
import qualified Data.Text as T

data EnvironmentVariable = EnvironmentVariable
    { name :: String
    , defaultValue :: Maybe String
    } deriving(Show, Generic)

instance FromJSON EnvironmentVariable

data CmdArgType = Positional | Flag | Named deriving(Show, Generic)

instance FromJSON CmdArgType

data CmdArg = CmdArg
    { kind :: CmdArgType
    , name :: String
    , short :: Maybe Char
    , defaultValue :: Maybe String
    , help :: Maybe String
    } deriving(Show, Generic)

instance FromJSON CmdArg where
  parseJSON = withObject "command arg" $ \o ->
    CmdArg <$> o .:? "kind" .!= Positional
           <*> o .:  "name"
           <*> o .:? "short" .!= Nothing
           <*> o .:? "default" .!= Nothing
           <*> o .:? "help" .!= Nothing

data CmdStepOutput = Ignore | Pipe | Print | Capture | ParseJson deriving(Show, Generic)
instance FromJSON CmdStepOutput

data CmdStep = CmdStep
    { step :: String
    , output :: CmdStepOutput
    } deriving(Show)

instance FromJSON CmdStep where
  parseJSON (Object o) = CmdStep <$> o .: "step" <*> o .: "output"
  parseJSON (String s) = return $ CmdStep (T.unpack s) Print
  parseJSON _ = fail "CmdStep must be either an object with step and output or a string"

data CommandDefinition = CommandDefinition
    { name :: String
    , environment :: [EnvironmentVariable]
    , environmentFrom :: Maybe String
    , commands :: [CmdStep]
    , args :: [CmdArg]
    , help :: String
    , interpreter :: Maybe Interpreter
    } deriving(Show, Generic)

instance FromJSON CommandDefinition where
  parseJSON = withObject "command definition" $ \o ->
    CommandDefinition <$> o .: "name"
                      <*> o .:? "environment" .!= []
                      <*> o .:? "environmentFrom" .!= Nothing
                      <*> o .:? "commands" .!= []
                      <*> o .:? "args" .!= []
                      <*> o .:? "help" .!= "No help available"
                      <*> o .:? "interpreter" .!= Nothing


data Interpreter = Interpreter
    { executable :: String
    , args :: [String]
    } deriving(Show, Generic)

instance FromJSON Interpreter

data Global = Global
    { header :: Maybe String
    , help :: Maybe String
    , environmentFrom :: Maybe String
    , interpreter :: Interpreter
    } deriving (Show, Generic)

defaultInterpreter = Interpreter "bash" ["-c"]
defaultGlobalHelp = Just "dev++ command line tool"
defaultGlobal = Global Nothing defaultGlobalHelp Nothing defaultInterpreter

instance FromJSON Global where
  parseJSON = withObject "global" $ \o ->
    Global <$> o .:? "header" .!= Nothing
           <*> o .:? "help" .!= defaultGlobalHelp
           <*> o .:? "environmentFrom" .!= Nothing
           <*> o .:? "interpreter" .!= defaultInterpreter

data DevDefinition = DevDefinition
    { commands :: [CommandDefinition]
    , global :: Maybe Global
    } deriving(Show, Generic)

instance FromJSON DevDefinition


data NamedValue = SimpleNamedValue String String | JsonNamedValue String Value deriving(Show)

data Binding = Binding [NamedValue] deriving(Show)

instance ToJSON Binding where
    toJSON (Binding vars) = object $ map tj vars
        where
            tj (SimpleNamedValue k v) = T.pack k .= T.pack v
            tj (JsonNamedValue k v) = T.pack k .= v
