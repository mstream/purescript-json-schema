module CLI (Command(..), Options) where

import CLI.Command.Compat as Compat
import CLI.Command.Diff as Diff
import CLI.Command.Validate as Validate
import CLI.Program (OutputFormat)

type Options = { command ∷ Command }

data Command
  = Compat OutputFormat Compat.Options
  | Diff OutputFormat Diff.Options
  | Validate OutputFormat Validate.Options
