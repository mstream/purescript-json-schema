module CLI.Halogen.Command
  ( Action(..)
  , CommandComponent
  , CommandComponentMonad
  , Input
  , Query(..)
  , State
  , handleAction
  , handleQuery
  , renderField
  , renderFilesystemFieldset
  ) where

import Prelude

import Control.Monad.State (get, modify_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Action act = UpdateOption act | UpdateFileSystem String String

type Input opts =
  { options ∷ opts, filesystem ∷ Map String String }

data Query opts a = GetInput (Input opts → a)

type State opts = { filesystem ∷ Map String String, options ∷ opts }

type CommandComponent opts m = Component (Query opts) Unit Void m

type CommandComponentMonad act opts m a = HalogenM (State opts)
  (Action act)
  ()
  Void
  m
  a

handleQuery
  ∷ ∀ a action m opts
  . Query opts a
  → HalogenM (State opts) action () Void m (Maybe a)
handleQuery (GetInput reply) = (Just <<< reply) <$> get

handleAction
  ∷ ∀ act m opts
  . (act → CommandComponentMonad act opts m Unit)
  → Action act
  → CommandComponentMonad act opts m Unit
handleAction handleOptionAction = case _ of
  UpdateFileSystem filePath fileContents →
    modify_ \st → st
      { filesystem = Map.insert filePath fileContents
          st.filesystem
      }
  UpdateOption optionAction →
    handleOptionAction optionAction

renderFilesystemFieldset
  ∷ ∀ act m. Map String String → ComponentHTML (Action act) () m
renderFilesystemFieldset = HH.fieldset_
  <<< foldlWithIndex
    ( \filePath acc fileContents → acc <>
        [ HH.label [ HP.for filePath ] [ HH.text filePath ]
        , HH.textarea
            [ HE.onValueInput $ UpdateFileSystem filePath
            , HP.id filePath
            , HP.rows 10
            , HP.value fileContents
            ]
        ]
    )
    [ HH.legend_ [ HH.text "Filesystem" ] ]

renderField
  ∷ ∀ act m
  . String
  → String
  → (String → act)
  → ComponentHTML (Action act) () m
renderField name value toAction = HH.div_
  [ HH.label [ HP.for name ] [ HH.text name ]
  , HH.input
      [ HP.id name
      , HP.name name
      , HP.value value
      , HE.onValueChange (UpdateOption <<< toAction)
      ]
  ]
