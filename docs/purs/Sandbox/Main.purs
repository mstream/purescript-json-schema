module Sandbox.Main (main) where

import Prelude

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
import Main.Validate as Validate

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI component unit body

type State =
  { programInput ∷ Validate.ProgramInput
  , programOutput ∷ Maybe Validate.ProgramOutput
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
  { programInput:
      { jsonText: A.stringify initialJson
      , outputFormat: Validate.Json
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
  let
    outputFormatLabel = case programInput.outputFormat of
      Validate.Json →
        "JSON"
      Validate.Markdown →
        "Markdown"
  in
    HH.div_
      [ HH.div_
          [ HH.label_
              [ HH.text "JSON schema:"
              ]
          , HH.input
              [ HE.onValueInput UpdateSchema
              , HP.value programInput.schemaText
              ]
          ]
      , HH.div_
          [ HH.label_ [ HH.text "JSON value:" ]
          , HH.input
              [ HE.onValueInput UpdateJson
              , HP.value programInput.jsonText
              ]
          ]
      , HH.button
          [ HE.onClick \_ → RunProgram
          ]
          [ HH.text $ "show program result in "
              <> outputFormatLabel
              <> " format"
          ]
      , maybe
          (HH.text "")
          (HH.fromPlainHTML <<< renderProgramOutput)
          programOutput
      ]

renderProgramOutput ∷ Validate.ProgramOutput → HH.PlainHTML
renderProgramOutput { exitCode, stderr, stdout } = HH.div_
  [ HH.div_
      [ HH.label_ [ HH.text "Exit code:" ]
      , HH.text $ show exitCode
      ]
  , HH.div_
      [ HH.label_ [ HH.text "Standard output:" ]
      , HH.text stdout
      ]
  , HH.div_
      [ HH.label_ [ HH.text "Standard error:" ]
      , HH.text stderr
      ]
  ]

handleAction ∷ ∀ o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  RunProgram →
    H.modify_ \st → st
      { programOutput = Just $ Validate.compute st.programInput }
  UpdateJson newJsonText →
    H.modify_ \st → st
      { programInput = st.programInput { jsonText = newJsonText } }
  UpdateSchema newSchemaText →
    H.modify_ \st → st
      { programInput = st.programInput { schemaText = newSchemaText } }
