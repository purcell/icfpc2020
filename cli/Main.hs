{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Generic
import Prelude

data CLIOptions = CLIOptions { file :: FilePath }
  deriving (Generic, Show)


instance ParseRecord CLIOptions


main :: IO ()
main = do
  _options :: CLIOptions <- getRecord "cli"
  pure ()


-- Local Variables:
-- dante-target: "exe:cli"
-- End:
