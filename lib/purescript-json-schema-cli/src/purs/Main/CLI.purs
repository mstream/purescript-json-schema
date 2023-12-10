module Main.CLI (main) where

import Prelude

import CLI (Command(..))
import CLI.Command.Compat as Compat
import CLI.Command.Diff as Diff
import CLI.Command.Validate as Validate
import CLI.Node as Node
import CLI.Program (class FileAccess, ProgramOutput)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, runReaderT)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Process as Process
import Options.Applicative as O

newtype AppM a = AppM (ReaderT Unit Effect a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadError Error AppM

instance FileAccess AppM where
  readFileContent = liftEffect <<< FS.readTextFile UTF8

runApp ∷ ∀ a. AppM a → Unit → Effect a
runApp (AppM readerT) env = runReaderT readerT env

main ∷ Effect Unit
main = Aff.launchAff_ do
  options ← liftEffect $ O.execParser Node.opts
  programOutput ← runCommand $ case options.command of
    Compat outputFormat commandOptions →
      Compat.program outputFormat commandOptions
    Diff outputFormat commandOptions →
      Diff.program outputFormat commandOptions
    Validate outputFormat commandOptions →
      Validate.program outputFormat commandOptions

  Console.info programOutput.stdout
  Console.error programOutput.stderr
  liftEffect $ Process.setExitCode programOutput.exitCode
  where
  runCommand ∷ AppM ProgramOutput → Aff ProgramOutput
  runCommand command = liftEffect $ runApp command unit
