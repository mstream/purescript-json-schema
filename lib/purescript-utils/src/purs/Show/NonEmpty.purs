module Show.NonEmpty (class Show1, show1) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Type.Proxy (Proxy(..))

class Show1 a where
  show1 ∷ a → NonEmptyString

instance Show1 Boolean where
  show1 b =
    if b then StringNE.nes (Proxy @"true")
    else StringNE.nes (Proxy @"false")

instance Show1 Int where
  show1 = fromMaybe (StringNE.nes (Proxy @"impossible"))
    <<< StringNE.fromString
    <<< show

instance Show1 Number where
  show1 = fromMaybe (StringNE.nes (Proxy @"impossible"))
    <<< StringNE.fromString
    <<< show

instance Show1 NonEmptyString where
  show1 = show1 <<< StringNE.toString

instance Show1 String where
  show1 = StringNE.appendString (StringNE.nes (Proxy @"\""))
    <<< (_ <> "\"")
