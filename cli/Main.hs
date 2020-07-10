{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Generic
import Protolude
import System.Environment (getProgName)

newtype CLIOptions = CLIOptions { file :: FilePath }
  deriving (Generic, Show)


instance ParseRecord CLIOptions


main :: IO ()
main = do
  progName <- getProgName
  _options :: CLIOptions <- getRecord (toS progName)
  pure ()


-- Local Variables:
-- dante-target: "exe:cli"
-- End:
