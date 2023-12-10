module CLI.Halogen.CompatOptions (component) where

import Prelude

import CLI.Command.Compat as Compat
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
  = UpdateLeftSchemaFilePath String
  | UpdateRightSchemaFilePath String

component ∷ ∀ m. CommandComponent Compat.Options m
component = H.mkComponent
  { eval: H.mkEval $ H.defaultEval
      { handleAction = Command.handleAction handleOptionAction
      , handleQuery = Command.handleQuery
      }
  , initialState
  , render
  }

initialState ∷ Unit → Command.State Compat.Options
initialState _ =
  { filesystem: Map.fromFoldable
      [ leftSchemaFilePath
          /\
            ( A.stringifyWithIndent 2
                ( AE.encodeJson $ JsonSchema.print $
                    JsonSchema.ObjectSchema
                      JsonSchema.defaultKeywords
                        { typeKeyword = Just $ Set.singleton
                            JsonInteger
                        }
                )
            )
      , rightSchemaFilePath /\
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
  , options: { leftSchemaFilePath, rightSchemaFilePath }
  }
  where
  leftSchemaFilePath ∷ String
  leftSchemaFilePath = "schemata/left.json"

  rightSchemaFilePath ∷ String
  rightSchemaFilePath = "schemata/right.json"

handleOptionAction
  ∷ ∀ m
  . OptionAction
  → Command.CommandComponentMonad OptionAction Compat.Options m Unit
handleOptionAction = case _ of
  UpdateLeftSchemaFilePath newFilePath → do
    modify_ \st → st
      { options = st.options
          { leftSchemaFilePath = newFilePath }
      }
  UpdateRightSchemaFilePath newFilePath → do
    modify_ \st → st
      { options = st.options
          { rightSchemaFilePath = newFilePath }
      }

render
  ∷ ∀ m
  . Command.State Compat.Options
  → ComponentHTML (Command.Action OptionAction) () m
render { filesystem, options } =
  HH.div_
    [ HH.fieldset_
        [ HH.legend_ [ HH.text "Command Options" ]
        , Command.renderField
            "left schema file path"
            options.leftSchemaFilePath
            UpdateLeftSchemaFilePath
        , Command.renderField
            "right schema file path"
            options.rightSchemaFilePath
            UpdateRightSchemaFilePath
        ]
    , Command.renderFilesystemFieldset filesystem
    ]
