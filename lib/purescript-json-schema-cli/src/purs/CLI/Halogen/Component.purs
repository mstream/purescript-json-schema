module CLI.Halogen (commandFormComponent) where

import Prelude

import CLI (Command(..))
import CLI.Command.Compat as Compat
import CLI.Command.Diff as Diff
import CLI.Command.Validate as Validate
import CLI.Halogen.Command as Command
import CLI.Halogen.CompatOptions as CompatOptions
import CLI.Halogen.DiffOptions as DiffOptions
import CLI.Halogen.OutputFormat as OutputFormat
import CLI.Halogen.ValidateOptions as ValidateOptions
import Control.Monad.State (get, modify_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sandbox.Component as Sandbox
import Type.Proxy (Proxy(..))

data SelectedCommand
  = CompatCommand
  | DiffCommand
  | ValidateCommand

derive instance Eq SelectedCommand

data Action
  = Dummy
  | SelectedCommandUpdated SelectedCommand

type State =
  { selectedCommand ∷ SelectedCommand }

type Slots =
  ( compatOptions ∷ Slot (Command.Query Compat.Options) Void Unit
  , diffOptions ∷ Slot (Command.Query Diff.Options) Void Unit
  , outputFormat ∷ Slot OutputFormat.Query Void Unit
  , validateOptions ∷ Slot (Command.Query Validate.Options) Void Unit
  )

type ComponentMonad m a = HalogenM State Action Slots Sandbox.Output m a

commandFormComponent
  ∷ ∀ m. MonadAff m ⇒ Component Sandbox.Query Unit Sandbox.Output m
commandFormComponent = H.mkComponent
  { eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  , initialState: const { selectedCommand: ValidateCommand }
  , render
  }

handleAction ∷ ∀ m. Action → ComponentMonad m Unit
handleAction = case _ of
  Dummy →
    pure unit
  SelectedCommandUpdated newSelectedCommand → do
    modify_ \st → st { selectedCommand = newSelectedCommand }
    H.raise Sandbox.SelectedCommandChanged

handleQuery
  ∷ ∀ a m. MonadAff m ⇒ Sandbox.Query a → ComponentMonad m (Maybe a)
handleQuery = case _ of
  Sandbox.RequestProgramExecution reply → do
    state ← get
    outputFormat ← H.request
      (Proxy ∷ Proxy "outputFormat")
      unit
      OutputFormat.GetOutputFormat
    case outputFormat of
      Nothing →
        pure Nothing
      Just format →
        case state.selectedCommand of
          CompatCommand → do
            input ← H.request
              (Proxy ∷ Proxy "compatOptions")
              unit
              Command.GetInput

            case input of
              Just { filesystem, options } → do
                programOutput ← Sandbox.runCommand
                  filesystem
                  (Compat.program format options)
                pure $ Just $ reply programOutput
              Nothing →
                pure Nothing

          DiffCommand → do
            input ← H.request
              (Proxy ∷ Proxy "diffOptions")
              unit
              Command.GetInput

            case input of
              Just { filesystem, options } → do
                programOutput ← Sandbox.runCommand
                  filesystem
                  (Diff.program format options)
                pure $ Just $ reply programOutput
              Nothing →
                pure Nothing

          ValidateCommand → do
            input ← H.request
              (Proxy ∷ Proxy "validateOptions")
              unit
              Command.GetInput

            case input of
              Just { filesystem, options } → do
                programOutput ← Sandbox.runCommand
                  filesystem
                  (Validate.program format options)
                pure $ Just $ reply programOutput
              Nothing →
                pure Nothing

render ∷ ∀ m. State → ComponentHTML Action Slots m
render { selectedCommand } = HH.form_
  [ renderCommandSelectionDropdown
  , HH.slot_
      (Proxy ∷ Proxy "outputFormat")
      unit
      OutputFormat.component
      unit
  , renderCommandOptionsForm
  ]
  where
  renderCommandOptionsForm ∷ ComponentHTML Action Slots m
  renderCommandOptionsForm = HH.fieldset_
    [ HH.legend_ [ HH.text "Command Options" ]
    , case selectedCommand of
        CompatCommand →
          HH.slot_
            (Proxy ∷ Proxy "compatOptions")
            unit
            CompatOptions.component
            unit
        DiffCommand →
          HH.slot_
            (Proxy ∷ Proxy "diffOptions")
            unit
            DiffOptions.component
            unit
        ValidateCommand →
          HH.slot_
            (Proxy ∷ Proxy "validateOptions")
            unit
            ValidateOptions.component
            unit
    ]

  renderCommandSelectionDropdown ∷ ComponentHTML Action Slots m
  renderCommandSelectionDropdown = HH.div_
    [ HH.label
        [ HP.for "command" ]
        [ HH.text "Command" ]
    , HH.select
        [ HP.id "command"
        , HP.name "command"
        , HE.onSelectedIndexChange case _ of
            0 →
              SelectedCommandUpdated CompatCommand
            1 →
              SelectedCommandUpdated DiffCommand
            2 →
              SelectedCommandUpdated ValidateCommand
            _ →
              Dummy
        ]
        [ renderCommandSelectionItemFor CompatCommand
        , renderCommandSelectionItemFor DiffCommand
        , renderCommandSelectionItemFor ValidateCommand
        ]
    ]

  renderCommandSelectionItemFor
    ∷ SelectedCommand → ComponentHTML Action Slots m
  renderCommandSelectionItemFor command =
    HH.option
      [ HP.selected $ command == selectedCommand ]
      [ HH.text $ showSelectedCommand command ]

showSelectedCommand ∷ SelectedCommand → String
showSelectedCommand = case _ of
  CompatCommand →
    "compat"
  DiffCommand →
    "diff"
  ValidateCommand →
    "validate"
