module JsonSchema.Difference
  ( Difference(..)
  , DifferenceType(..)
  , calculate
  ) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Argonaut.Encode as AE
import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Markdown (FlowContentNode)
import Data.Markdown as M
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Docs.Document (class Document, document)
import JsonSchema (JsonSchema(..), JsonValueType, Keywords)
import JsonSchema as Schema
import JsonSchema.SchemaPath (SchemaPath, SchemaPathSegment(..))
import JsonSchema.SchemaPath as SchemaPath
import Show.NonEmpty (show1)
import Type.Proxy (Proxy(..))

newtype Difference = Difference
  { differenceType ∷ DifferenceType, path ∷ SchemaPath }

derive instance Eq Difference
derive instance Generic Difference _
derive instance Ord Difference

derive newtype instance EncodeJson Difference

instance Show Difference where
  show = genericShow

instance Document Difference where
  document (Difference { differenceType, path }) =
    ( M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy @"JSON schema path: ")
            <> SchemaPath.render path
    ) :| (Array.fromFoldable $ document differenceType)

data DifferenceType
  = BooleanSchemaChange Boolean
  | ExclusiveMaximumChange (Maybe Number) (Maybe Number)
  | ExclusiveMinimumChange (Maybe Number) (Maybe Number)
  | MaximumChange (Maybe Number) (Maybe Number)
  | MinimumChange (Maybe Number) (Maybe Number)
  | MultipleOfChange (Maybe Number) (Maybe Number)
  | SchemaChangeFromBooleanToObject Boolean Keywords
  | SchemaChangeFromObjectToBoolean Keywords Boolean
  | TypeChange (Maybe (Set JsonValueType)) (Maybe (Set JsonValueType))

derive instance Eq DifferenceType
derive instance Generic DifferenceType _
derive instance Ord DifferenceType

instance EncodeJson DifferenceType where
  encodeJson = case _ of
    BooleanSchemaChange b →
      A.fromString $ "boolean schema changed to " <> show b
    ExclusiveMaximumChange old new →
      A.fromString $ "exclusive maximum value change from "
        <> maybe "not set" show old
        <> " to "
        <> maybe "not set" show new

    ExclusiveMinimumChange old new →
      A.fromString $ "exclusive minimum value change from "
        <> maybe "not set" show old
        <> " to "
        <> maybe "not set" show new

    MaximumChange old new →
      A.fromString $ "maximum value change from "
        <> maybe "not set" show old
        <> " to "
        <> maybe "not set" show new

    MinimumChange old new →
      A.fromString $ "minimum value change from "
        <> maybe "not set" show old
        <> " to "
        <> maybe "not set" show new

    MultipleOfChange old new →
      A.fromString $ "multiple of change from "
        <> maybe "not set" show old
        <> " to "
        <> maybe "not set" show new

    SchemaChangeFromBooleanToObject b keywords →
      "newObjectSchema" := AE.encodeJson keywords
        ~> "oldBooleanSchema" := A.fromBoolean b
        ~> A.jsonEmptyObject

    SchemaChangeFromObjectToBoolean keywords b →
      "newSchema" := A.fromString ("boolean schema of " <> show b)
        ~> "oldObjectSchema" := AE.encodeJson keywords
        ~> A.jsonEmptyObject

    TypeChange old new →
      "newAcceptableTypes" := AE.encodeJson new
        ~> "oldAcceptableTypes" := AE.encodeJson old
        ~> A.jsonEmptyObject

instance Show DifferenceType where
  show = genericShow

instance Document DifferenceType where
  document = case _ of
    BooleanSchemaChange false →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes
            ( Proxy
                @"change of boolean schema from allow-all to reject-all"
            )
    BooleanSchemaChange true →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes
            ( Proxy
                @"change of boolean schema from reject-all to allow-all"
            )
    ExclusiveMaximumChange before after →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $
          StringNE.nes
            ( Proxy @"change of exclusiveMaximum from "
            )
            <> renderOptionalNumber before
            <> StringNE.nes
              (Proxy @" to ")
            <> renderOptionalNumber after
    ExclusiveMinimumChange before after →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $
          StringNE.nes
            ( Proxy @"change of exclusiveMinimum from "
            )
            <> renderOptionalNumber before
            <> StringNE.nes
              (Proxy @" to ")
            <> renderOptionalNumber after
    MaximumChange before after →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy @"change of maximum from ")
            <> renderOptionalNumber before
            <> StringNE.nes (Proxy @" to ")
            <> renderOptionalNumber after
    MinimumChange before after →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy @"change of minimum from ")
            <> renderOptionalNumber before
            <> StringNE.nes (Proxy @" to ")
            <> renderOptionalNumber after
    MultipleOfChange before after →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy @"change of multipleOf from ")
            <> renderOptionalNumber before
            <> StringNE.nes (Proxy @" to ")
            <> renderOptionalNumber after
    SchemaChangeFromBooleanToObject _ _ →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes
            (Proxy @"change of boolean schema to object schema")
    SchemaChangeFromObjectToBoolean _ _ →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes
            (Proxy @"change of object schema to boolean schema")
    TypeChange typesBefore typesAfter →
      ( M.paragraph $ ArrayNE.singleton $ M.text $
          StringNE.nes
            (Proxy @"change of accepted JSON value types from")
      ) :|
        [ renderJsonValueTypes typesBefore
        , M.paragraph
            $ ArrayNE.singleton
            $ M.text
            $ StringNE.nes
                (Proxy @"to")
        , renderJsonValueTypes typesAfter
        ]

calculate ∷ JsonSchema → JsonSchema → Set Difference
calculate = go Nil
  where
  go ∷ SchemaPath → JsonSchema → JsonSchema → Set Difference
  go path previousSchema nextSchema = case previousSchema, nextSchema of
    BooleanSchema previousBool, BooleanSchema nextBool →
      Set.fromFoldable
        $ calculateBooleanSchemataDiff path previousBool nextBool
    BooleanSchema previousBool, ObjectSchema nextKeywords →
      Set.singleton
        $ calculateBooleanAndObjectSchemataDiff
            path
            previousBool
            nextKeywords
    ObjectSchema previousKeywords, BooleanSchema nextBool →
      Set.singleton
        $ calculateObjectAndBooleanSchemataDiff
            path
            previousKeywords
            nextBool
    ObjectSchema previousKeywords, ObjectSchema nextKeywords →
      calculateObjectSchemataDiff path previousKeywords nextKeywords

  calculateBooleanSchemataDiff
    ∷ SchemaPath → Boolean → Boolean → Maybe Difference
  calculateBooleanSchemataDiff path =
    case _, _ of
      false, true →
        Just $ Difference
          { differenceType: BooleanSchemaChange true, path }

      true, false →
        Just $ Difference
          { differenceType: BooleanSchemaChange false, path }
      _, _ →
        Nothing

  calculateBooleanAndObjectSchemataDiff
    ∷ SchemaPath → Boolean → Keywords → Difference
  calculateBooleanAndObjectSchemataDiff path bool keywords = Difference
    { differenceType: SchemaChangeFromBooleanToObject bool keywords
    , path
    }

  calculateObjectAndBooleanSchemataDiff
    ∷ SchemaPath → Keywords → Boolean → Difference
  calculateObjectAndBooleanSchemataDiff path keywords bool = Difference
    { differenceType: SchemaChangeFromObjectToBoolean keywords bool
    , path
    }

  calculateObjectSchemataDiff
    ∷ SchemaPath → Keywords → Keywords → Set Difference
  calculateObjectSchemataDiff path previousKeywords nextKeywords =
    foldMap
      (\f → f path previousKeywords nextKeywords)
      [ calculateMultipleOfDiff
      , calculateRangeDiff
      , calculateTypeKeywordDiff
      ]

calculateMultipleOfDiff
  ∷ ∀ r
  . SchemaPath
  → { multipleOf ∷ Maybe Number | r }
  → { multipleOf ∷ Maybe Number | r }
  → Set Difference
calculateMultipleOfDiff path previousKeywords nextKeywords =
  if previousKeywords.multipleOf == nextKeywords.multipleOf then
    Set.empty
  else Set.singleton $ Difference
    { differenceType: MultipleOfChange
        previousKeywords.multipleOf
        nextKeywords.multipleOf
    , path: MultipleOf : path
    }

calculateRangeDiff ∷ SchemaPath → Keywords → Keywords → Set Difference
calculateRangeDiff path previousKeywords nextKeywords =
  exclusiveMaximumDiff
    <> exclusiveMinimumDiff
    <> maximumDiff
    <> minimumDiff
  where
  exclusiveMaximumDiff ∷ Set Difference
  exclusiveMaximumDiff =
    if
      previousKeywords.exclusiveMaximum == nextKeywords.exclusiveMaximum then
      Set.empty
    else Set.singleton $ Difference
      { differenceType: ExclusiveMaximumChange
          previousKeywords.exclusiveMaximum
          nextKeywords.exclusiveMaximum
      , path: ExclusiveMaximum : path
      }

  exclusiveMinimumDiff ∷ Set Difference
  exclusiveMinimumDiff =
    if
      previousKeywords.exclusiveMinimum == nextKeywords.exclusiveMinimum then
      Set.empty
    else Set.singleton $ Difference
      { differenceType: ExclusiveMinimumChange
          previousKeywords.exclusiveMinimum
          nextKeywords.exclusiveMinimum
      , path: ExclusiveMinimum : path
      }

  maximumDiff ∷ Set Difference
  maximumDiff =
    if
      previousKeywords.maximum == nextKeywords.maximum then
      Set.empty
    else Set.singleton $ Difference
      { differenceType: MaximumChange
          previousKeywords.maximum
          nextKeywords.maximum
      , path: Maximum : path
      }

  minimumDiff ∷ Set Difference
  minimumDiff =
    if
      previousKeywords.minimum == nextKeywords.minimum then
      Set.empty
    else Set.singleton $ Difference
      { differenceType: MinimumChange
          previousKeywords.minimum
          nextKeywords.minimum
      , path: Minimum : path
      }

calculateTypeKeywordDiff
  ∷ SchemaPath → Keywords → Keywords → Set Difference
calculateTypeKeywordDiff path previousKeywords nextKeywords =
  if previousKeywords.typeKeyword == nextKeywords.typeKeyword then
    Set.empty
  else
    Set.singleton $ Difference
      { differenceType: TypeChange
          previousKeywords.typeKeyword
          nextKeywords.typeKeyword
      , path: TypeKeyword : path
      }

renderOptionalNumber ∷ Maybe Number → NonEmptyString
renderOptionalNumber = maybe
  (StringNE.nes (Proxy @"unspecified"))
  show1

renderJsonValueTypes ∷ Maybe (Set JsonValueType) → FlowContentNode
renderJsonValueTypes = maybe
  ( M.paragraph
      $ ArrayNE.singleton
      $ M.text
      $ StringNE.nes (Proxy @"unspecified")
  )
  toFlowContentNode
  where
  toFlowContentNode ∷ Set JsonValueType → FlowContentNode
  toFlowContentNode = ArrayNE.fromFoldable >>> case _ of
    Just jsonValueTypes →
      M.unorderedList $ ArrayNE.singleton <$> renderJsonValueType <$>
        jsonValueTypes
    Nothing →
      M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy @"none")

renderJsonValueType ∷ JsonValueType → FlowContentNode
renderJsonValueType = M.paragraph
  <<< ArrayNE.singleton
  <<< M.text
  <<< Schema.renderJsonValueType
