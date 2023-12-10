module Sandbox.Component
  ( AppM
  , Output(..)
  , Query(..)
  , component
  , runCommand
  ) where

import Prelude

import CLI.Program (class FileAccess, ProgramOutput)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Control.Monad.State (put)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Exception as Exception
import Halogen (Component, ComponentHTML, HalogenM, Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

newtype AppM a = AppM (ReaderT (Map String String) Effect a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadError Error AppM
derive newtype instance MonadAsk (Map String String) AppM

instance FileAccess AppM where
  readFileContent filePath = do
    fileContents ← asks $ Map.lookup filePath
    maybe
      ( throwError
          $ Exception.error
          $ "file " <> filePath <> " not found"
      )
      pure
      fileContents

runApp ∷ ∀ a. AppM a → Map String String → Effect a
runApp (AppM readerT) env = runReaderT readerT env

data Output = SelectedCommandChanged

type State = { programOutput ∷ Maybe ProgramOutput }

data Action = HandleOptionsFormOutput Output | RunProgram

data Query a = RequestProgramExecution (ProgramOutput → a)

type Slots = (optionsForm ∷ Slot Query Output Unit)

type ComponentMonad m a = HalogenM State Action Slots Void m a

component
  ∷ ∀ q m
  . MonadAff m
  ⇒ MonadError Error m
  ⇒ Component Query Unit Output m
  → Component q Unit Void m
component formComponent =
  H.mkComponent
    { initialState: const { programOutput: Nothing }
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    , render: render formComponent
    }

render
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadError Error m
  ⇒ Component Query Unit Output m
  → State
  → ComponentHTML Action Slots m
render formComponent { programOutput } =
  HH.div
    [ HP.style "display: flex" ]
    [ HH.div
        [ HP.style "width: 100%" ]
        [ HH.div_
            [ HH.text "Options Form:" ]
        , HH.slot
            (Proxy ∷ Proxy "optionsForm")
            unit
            formComponent
            unit
            HandleOptionsFormOutput
        , HH.button
            [ HE.onClick $ const RunProgram
            , HP.type_ ButtonButton
            ]
            [ HH.text "execute program" ]
        ]
    , HH.div
        [ HP.style "width: 100%" ]
        [ HH.div_ [ HH.text "Program Output:" ]
        , maybe
            (HH.text "")
            (HH.fromPlainHTML <<< renderProgramOutput)
            programOutput
        ]
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

handleAction
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadError Error m
  ⇒ Action
  → ComponentMonad m Unit
handleAction = case _ of
  HandleOptionsFormOutput SelectedCommandChanged →
    put { programOutput: Nothing }

  RunProgram → do
    programOutput ← H.request
      (Proxy ∷ Proxy "optionsForm")
      unit
      RequestProgramExecution
    put { programOutput }

runCommand
  ∷ ∀ m
  . MonadAff m
  ⇒ Map String String
  → AppM ProgramOutput
  → m ProgramOutput
runCommand filesystem command =
  liftAff $ liftEffect $ runApp command filesystem
