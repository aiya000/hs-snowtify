{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception.Safe (MonadThrow, SomeException)
import Control.Monad (void, forM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Data.Default (Default(..))
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
main = runEitherT getBuildCommand >>= void . TT.sh . execute
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


execute :: Either SomeException Text -> Shell ExitCode
execute (Left err)      = notifySend . T.pack $ show err
execute (Right command) = do
  (exitCode, out, err) <- TT.procStrictWithErr "stack" [command] ""
  let result = out <> err
  let notifyer = if exitCode == TT.ExitSuccess
                    then notifySucceeding
                    else notifyErrors
  notifyer command result


-- | Show succeeding with the notify-daemon
notifySucceeding :: Text -> Text -> Shell ExitCode
notifySucceeding command result = do
  notifySend $ "stack " <> command <> " is succeed"
  whenDef (not $ T.null result) $ notifySend result

-- | Show errors with the notify-daemon
notifyErrors :: Text -> Text -> Shell ExitCode
notifyErrors command result = do
  let blobs = TT.cut sections result
  notifySend $ "stack " <> command <> " is finished with errors"
  (totalize <$>) . forM blobs $ \blob ->
    whenDef (isErrorSection blob) $ notifySend blob
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


-- |
-- If all `ExitCode` is `ExitSuccess`, return `ExitSuccess`.
-- Otherwise, return `ExitFailure 1`.
totalize :: [ExitCode] -> ExitCode
totalize = exitCode . extremize . sum . map weaken
  where
    weaken :: ExitCode -> Int
    weaken TT.ExitSuccess     = 0
    weaken (TT.ExitFailure n) = n

    extremize :: Int -> Int
    extremize 0 = 0
    extremize _ = 1


-- | Generalized a value constructor of `ExitCode`
exitCode :: Int -> ExitCode
exitCode 0 = TT.ExitSuccess
exitCode n = TT.ExitFailure n


-- | Send a message to the notify-daemon
notifySend :: Text -> Shell ExitCode
notifySend msg = TT.proc "notify-send" ["snowtify", msg] ""


-- | for `whenDef`
instance Default ExitCode where
  def = TT.ExitSuccess


-- | Simular to `when` but don't forget result
whenDef :: (Applicative f, Default a) => Bool -> f a -> f a
whenDef True f  = f
whenDef False _ = pure def
