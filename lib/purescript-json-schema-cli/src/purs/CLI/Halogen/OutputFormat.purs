module CLI.Halogen.OutputFormat (Query(..), component) where

import Prelude

import CLI.Program (OutputFormat(..))
import Control.Monad.State (get, modify_)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Action = OutputFormatUpdated OutputFormat

data Query a = GetOutputFormat (OutputFormat → a)

type State =
  { outputFormat ∷ OutputFormat }

component ∷ ∀ m. Component Query Unit Void m
component = H.mkComponent
  { eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction, handleQuery = handleQuery }
  , initialState: const { outputFormat: Json }
  , render
  }

handleAction
  ∷ ∀ m. Action → HalogenM State Action () Void m Unit
handleAction = case _ of
  OutputFormatUpdated newOutputFormat →
    modify_ \st → st { outputFormat = newOutputFormat }

handleQuery ∷ ∀ a m. Query a → HalogenM State Action () Void m (Maybe a)
handleQuery = case _ of
  GetOutputFormat reply → do
    state ← get
    pure $ Just $ reply state.outputFormat

render ∷ ∀ m. State → ComponentHTML Action () m
render { outputFormat } = HH.fieldset_
  [ HH.legend_ [ HH.text "Output Format" ]
  , renderOutputFormatFieldFor Json
  , renderOutputFormatFieldFor Markdown
  ]
  where
  renderOutputFormatFieldFor
    ∷ OutputFormat → ComponentHTML Action () m
  renderOutputFormatFieldFor format = HH.div_
    [ HH.label
        [ HP.for $ showOutputFormat format ]
        [ HH.text $ showOutputFormat format ]
    , HH.input
        [ HP.type_ InputRadio
        , HP.name "outputFormat"
        , HP.id $ showOutputFormat format
        , HP.value $ showOutputFormat format
        , HP.checked $ format == outputFormat
        , HE.onClick $ const $ OutputFormatUpdated format
        ]
    ]

showOutputFormat ∷ OutputFormat → String
showOutputFormat = case _ of
  Json →
    "JSON"
  Markdown →
    "Markdown"
