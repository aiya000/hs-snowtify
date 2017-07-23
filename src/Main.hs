{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception.Safe (MonadThrow, SomeException)
import Control.Monad (void, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Data.Either (isLeft)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Safe (headMay)
import Turtle (Shell, ExitCode, Pattern)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Turtle as TT


-- | Call `execute` with a haskell-stack command
main :: IO ()
main = runEitherT getBuildCommand >>= TT.sh . execute
  where
    -- Get a sub command of haskell-stack from CLI arguments.
    -- This must be "build" or "test", or empty.
    -- If CLI arguments is not given, return "build".
    -- If CLI arguments is neither "build" nor "test", throw an exception.
    getBuildCommand :: (MonadIO m, MonadThrow m) => m Text
    getBuildCommand = do
      args <- headMay <$> liftIO TT.arguments
      case args of
        Nothing      -> return "build" -- run 'build' by default
        Just "test"  -> return "test"
        Just "build" -> return "build"
        Just unknown -> fail . T.unpack $ "snowtify doesn't know '" <> unknown <> "' command x("


execute :: Either SomeException Text -> Shell ()
execute (Left err)      = liftIO $ print err
execute (Right command) = do
  (exitCode, out, err) <- TT.procStrictWithErr "stack" [command] ""
  let result = out <> err
  let notifyer = if exitCode == TT.ExitSuccess
                    then notifySucceeding
                    else notifyErrors
  notifyer result


-- | Show succeeding with the notify-daemon
notifySucceeding :: Text -> Shell ()
notifySucceeding = void . notifySend . ("stack test is succeed: " <>)

-- | Show errors with the notify-daemon
notifyErrors :: Text -> Shell ()
notifyErrors result = do
  let blobs = TT.cut sections result
  notifySend "stack test is finished with errors"
  forM_ blobs $ \blob ->
    when (isErrorSection blob) . void $ notifySend blob
  where
    sections :: Pattern ()
    sections = void $ do
      TT.newline
      TT.spaces
      TT.newline

    isErrorSection :: Text -> Bool
    isErrorSection x =
      case headMay $ T.lines x of
        Nothing        -> False
        Just firstLine -> any (== "error:") $ T.words firstLine


-- | Send a message to the notify-daemon
notifySend :: Text -> Shell ExitCode
notifySend msg = TT.proc "notify-send" ["snowtify", msg] ""
