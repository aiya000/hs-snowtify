{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void, forM_, when)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Safe (headMay)
import Turtle (Shell, sh, procStrictWithErr, ExitCode(..), cut, Pattern, newline, spaces, proc)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


main :: IO ()
main = sh $ do
  (exitCode, out, err) <- procStrictWithErr "stack" ["test"] ""
  let result = out <> err
  if exitCode == ExitSuccess
     then void . notifySend $ "stack test is succeed: " <> result
     else do
       let blobs = cut sections result
       notifySend "stack test is finished with errors"
       forM_ blobs $ \blob ->
         when (isErrorSection blob) . void $ notifySend blob
  where
    sections :: Pattern ()
    sections = void $ do
      newline
      spaces
      newline

    isErrorSection :: Text -> Bool
    isErrorSection x =
      case headMay $ T.lines x of
        Nothing        -> False
        Just firstLine -> any (== "error:") $ T.words firstLine


-- | Send a message to the notify-daemon
notifySend :: Text -> Shell ExitCode
notifySend msg = proc "notify-send" ["snowtify", msg] ""
