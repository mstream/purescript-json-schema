module CLI.Halogen.ValidateOptions (component) where

import Prelude

import CLI.Command.Validate as Validate
import CLI.Halogen.Command (CommandComponent)
import CLI.Halogen.Command as Command
import Control.Monad.State (modify_)
import Data.Argonaut.Core as A
import Data.Argonaut.Encode as AE
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Halogen (ComponentHTML)
import Halogen as H
import Halogen.HTML as HH
import JsonSchema (JsonValueType(..))
import JsonSchema as JsonSchema

data OptionAction
  = UpdateJsonFilePath String
  | UpdateSchemaFilePath String

component ∷ ∀ m. CommandComponent Validate.Options m
component = H.mkComponent
  { eval: H.mkEval $ H.defaultEval
      { handleAction = Command.handleAction handleOptionAction
      , handleQuery = Command.handleQuery
      }
  , initialState
  , render
  }

initialState ∷ Unit → Command.State Validate.Options
initialState _ =
  { filesystem: Map.fromFoldable
      [ jsonFilePath
          /\
            ( A.stringifyWithIndent 2
                (AE.encodeJson $ A.fromNumber 123.0)
            )
      , schemaFilePath /\
          ( A.stringifyWithIndent 2
              ( AE.encodeJson
                  $ JsonSchema.print
                  $
                    JsonSchema.ObjectSchema
                      JsonSchema.defaultKeywords
                        { typeKeyword = Just $ Set.singleton
                            JsonString
                        }
              )
          )
      ]
  , options: { jsonFilePath, schemaFilePath }
  }
  where
  jsonFilePath ∷ String
  jsonFilePath = "value.json"

  schemaFilePath ∷ String
  schemaFilePath = "schema.json"

handleOptionAction
  ∷ ∀ m
  . OptionAction
  → Command.CommandComponentMonad OptionAction Validate.Options m Unit
handleOptionAction = case _ of
  UpdateJsonFilePath newFilePath → do
    modify_ \st → st
      { options = st.options
          { jsonFilePath = newFilePath }
      }
  UpdateSchemaFilePath newFilePath → do
    modify_ \st → st
      { options = st.options
          { schemaFilePath = newFilePath }
      }

render
  ∷ ∀ m
  . (Command.State Validate.Options)
  → ComponentHTML (Command.Action OptionAction) () m
render { filesystem, options } =
  HH.div_
    [ HH.fieldset_
        [ HH.legend_ [ HH.text "Command Options" ]
        , Command.renderField
            "schema file path"
            options.schemaFilePath
            UpdateSchemaFilePath
        , Command.renderField
            "json file path"
            options.jsonFilePath
            UpdateJsonFilePath
        ]
    , Command.renderFilesystemFieldset filesystem
    ]
