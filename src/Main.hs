{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception.Safe (MonadThrow, SomeException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Data.Monoid ((<>))
import Data.Text (Text)
import Safe (headMay)
import Turtle (Shell, ExitCode, Pattern)
import qualified Data.Text as T
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
  notifySend $ "'" <> command <> "' will be started"
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
  notifySections ["warning", "cycle"] result

-- | Show errors with the notify-daemon
notifyErrors :: Text -> Text -> Shell ExitCode
notifyErrors command result = do
  notifySend $ "stack " <> command <> " is finished with errors"
  notifySections ["error", "warning", "cycle"] result


-- |
-- This is used for judge any section from a result of `stack (build|test)`.
--
-- This value is \1 of "^.*:\w+:\w+: (.+):$".
type SectionWord = Text


-- |
-- Send sections to the notify-daemon.
-- The sections are cut from a result of `stack (build|test)` with `[SectionWord]`.
notifySections :: [SectionWord] -> Text -> Shell ExitCode
notifySections = ((totalize <$>) .) . (mapM notifySend .) . sections
  where
    sections :: [SectionWord] -> Text -> [Text]
    sections sWords result =
      let blobs = TT.cut resultDelimiter result
      in [y | p <- map isItSection sWords, (x,y) <- map twice blobs, p x]

    -- A delimiter for cut result to sections
    resultDelimiter :: Pattern ()
    resultDelimiter = void $ do
      TT.newline
      TT.spaces
      TT.newline

    isItSection :: Text -> Text -> Bool
    isItSection it x =
      let it' = it <> ":" -- 'it' is \1 of "^.*:\w+:\w+: (.+):$"
      in any (== it') . concatMap T.words $ T.lines x


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


twice :: a -> (a, a)
twice x = (x, x)
