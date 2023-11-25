module Sandbox.Main (main) where

import Prelude

import CLI (OutputFormat(..), ProgramOutput)
import CLI as CLI
import CLI.Validate (ProgramInput)
import CLI.Validate as Validate
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as JsonSchema

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI component unit body

type State =
  { outputFormat ∷ OutputFormat
  , programInput ∷ Validate.ProgramInput
  , programOutput ∷ Maybe ProgramOutput
  }

data Action = RunProgram | UpdateJson String | UpdateSchema String

component ∷ ∀ q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState ∷ ∀ i. i → State
initialState _ =
  { outputFormat: Json
  , programInput:
      { jsonText: A.stringify initialJson
      , schemaText: show $ JsonSchema.print initialSchema
      }
  , programOutput: Nothing
  }

initialJson ∷ Json
initialJson = A.jsonSingletonObject "foo" (A.fromNumber 1.0)

initialSchema ∷ JsonSchema
initialSchema = ObjectSchema JsonSchema.defaultKeywords
  { typeKeyword = Just $ Set.singleton JsonObject }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render { programInput, programOutput } =
  HH.div_
    [ renderProgramInput programInput
    , maybe
        (HH.text "")
        (HH.fromPlainHTML <<< renderProgramOutput)
        programOutput
    ]

renderProgramInput ∷ ∀ w. ProgramInput → HH.HTML w Action
renderProgramInput { jsonText, schemaText } = HH.form_
  [ HH.div_
      [ HH.label_
          [ HH.text "JSON schema:"
          ]
      , HH.input
          [ HE.onValueInput UpdateSchema
          , HP.value schemaText
          ]
      ]
  , HH.div_
      [ HH.label_ [ HH.text "JSON value:" ]
      , HH.input
          [ HE.onValueInput UpdateJson
          , HP.value jsonText
          ]
      ]
  , HH.button
      [ HE.onClick \_ → RunProgram
      , HP.type_ ButtonButton
      ]
      [ HH.text "execute program" ]
  ]

renderProgramOutput ∷ ProgramOutput → HH.PlainHTML
renderProgramOutput { exitCode, stderr, stdout } = HH.div_
  [ HH.div_
      [ HH.label_ [ HH.text "Exit code:" ]
      , HH.text $ show exitCode
      ]
  , HH.div_
      [ HH.label_ [ HH.text "Standard output:" ]
      , HH.pre_ [ HH.samp_ [ HH.text stdout ] ]
      ]
  , HH.div_
      [ HH.label_ [ HH.text "Standard error:" ]
      , HH.pre_ [ HH.samp_ [ HH.text stderr ] ]
      ]
  ]

handleAction ∷ ∀ o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  RunProgram →
    H.modify_ \st → st
      { programOutput = Just $ CLI.runProgram
          st.outputFormat
          Validate.compute
          st.programInput
      }
  UpdateJson newJsonText →
    H.modify_ \st → st
      { programInput = st.programInput { jsonText = newJsonText } }
  UpdateSchema newSchemaText →
    H.modify_ \st → st
      { programInput = st.programInput { schemaText = newSchemaText } }
