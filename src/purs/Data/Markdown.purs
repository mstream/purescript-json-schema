module Data.Markdown
  ( CodeLanguage(..)
  , Document
  , FlowContentNode(..)
  , FormattingOptions
  , HeadingId(..)
  , HeadingLevel(..)
  , Node(..)
  , PhrasingContentNode(..)
  , Url(..)
  , blockquote
  , codeBlock
  , codeBlock'
  , document
  , emphasis
  , flowContent
  , formatAnchor
  , formatText
  , heading1
  , heading2
  , heading3
  , heading4
  , heading5
  , heading6
  , headingIdToString
  , inlineCode
  , lineBreak
  , link
  , orderedList
  , unorderedList
  , paragraph
  , phrasingContent
  , render
  , renderFlowContentNode
  , renderPhrasingContentNodes
  , renderMermaid
  , rule
  , text
  , urlOfHeading
  , urlOfLocalFile
  ) where

import Prelude

import Control.Lazy as Lazy
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as ListNE
import Data.Maybe (Maybe(..))
import Data.Mermaid.FlowChart (FlowChartDef)
import Data.Mermaid.FlowChart as FlowChart
import Data.Semigroup.Foldable (class Foldable1, foldMap1)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable1 (unfoldr1)

mapFirst ∷ ∀ a. (a → a) → NonEmptyList a → NonEmptyList a
mapFirst f = ListNE.uncons >>> \{ head, tail } →
  f head `ListNE.cons'` tail

mapOtherThanLast ∷ ∀ a. (a → a) → NonEmptyList a → NonEmptyList a
mapOtherThanLast f =
  ListNE.uncons >>> \{ head, tail } → case ListNE.fromList tail of
    Just restOfList →
      ListNE.singleton (f head) <> mapOtherThanLast f restOfList
    Nothing →
      ListNE.singleton head

mapLast ∷ ∀ a. (a → a) → NonEmptyList a → NonEmptyList a
mapLast f =
  ListNE.uncons >>> \{ head, tail } → case ListNE.fromList tail of
    Just restOfList →
      ListNE.singleton head <> mapLast f restOfList
    Nothing →
      ListNE.singleton $ f head

mapLast' ∷ ∀ a. (a → a) → List a → List a
mapLast' f = ListNE.fromList >>> case _ of
  Just xs →
    ListNE.toList $ mapLast f xs
  Nothing →
    Nil

mapTail ∷ ∀ a. (a → a) → NonEmptyList a → NonEmptyList a
mapTail f = ListNE.uncons >>> \{ head, tail } →
  head `ListNE.cons'` (f <$> tail)

appendWith'
  ∷ ∀ a. (a → a → a) → List a → List a → List a
appendWith' f left = List.uncons >>> case _ of
  Just { head: headOfRight, tail: tailOfRight } →
    mapLast' (\x → f x headOfRight) left <> tailOfRight
  Nothing →
    left

newtype HeadingId = HeadingId String

derive instance Eq HeadingId
derive instance Ord HeadingId
derive newtype instance Show HeadingId

headingIdToString ∷ HeadingId → String
headingIdToString (HeadingId s) = s

data Url = ExternalUrl String | InternalUrl HeadingId

derive instance Eq Url
derive instance Ord Url
derive instance Generic Url _

instance Show Url where
  show = genericShow

type Document = Array Node

document ∷ ∀ f. Foldable f ⇒ f FlowContentNode → Document
document = map FlowContent <<< Array.fromFoldable

data Node
  = FlowContent FlowContentNode
  | PhrasingContent PhrasingContentNode

derive instance Eq Node
derive instance Ord Node
derive instance Generic Node _

instance Show Node where
  show = genericShow

flowContent ∷ FlowContentNode → Node
flowContent = FlowContent

phrasingContent ∷ PhrasingContentNode → Node
phrasingContent = PhrasingContent

data FlowContentNode
  = Blockquote (NonEmptyArray FlowContentNode)
  | CodeBlock CodeLanguage (Array String)
  | Heading HeadingLevel (NonEmptyArray PhrasingContentNode)
  | List Boolean (NonEmptyArray (NonEmptyArray FlowContentNode))
  | Paragraph (NonEmptyArray PhrasingContentNode)
  | Rule

derive instance Eq FlowContentNode
derive instance Ord FlowContentNode

derive instance Generic FlowContentNode _

instance Show FlowContentNode where
  show node = genericShow node

data PhrasingContentNode
  = Emphasis (NonEmptyArray PhrasingContentNode)
  | LineBreak
  | Link NonEmptyString Url
  | InlineCode NonEmptyString
  | Text NonEmptyString

derive instance Eq PhrasingContentNode
derive instance Ord PhrasingContentNode
derive instance Generic PhrasingContentNode _

instance Show PhrasingContentNode where
  show node = genericShow node

type FormattingOptions = { maxLineLength ∷ Int }

renderMermaid ∷ FlowChartDef → FlowContentNode
renderMermaid flowChartDef =
  codeBlock Mermaid code
  where
  code ∷ Array String
  code = String.split (Pattern "\n") $ FlowChart.render flowChartDef

render ∷ FormattingOptions → Document → String
render options = (_ <> "\n")
  <<< String.joinWith "\n"
  <<< Array.fromFoldable
  <<< List.reverse
  <<< List.dropWhile String.null
  <<< foldl f Nil
  <<< List.fromFoldable
  where
  f ∷ List String → Node → List String
  f acc = case _ of
    FlowContent flowContentNode →
      let
        renderedLines = ListNE.toList
          $ renderFlowContentNode options flowContentNode

        mergedLines = renderedLines <> acc
      in
        mergedLines
    PhrasingContent phrasingContentNode →
      let
        renderedLines = ListNE.toList
          $ renderPhrasingContentNodes options
          $ ArrayNE.singleton phrasingContentNode

        mergedLines = appendWith'
          (flip (<>))
          renderedLines
          acc

      in
        mergedLines

renderFlowContentNode
  ∷ FormattingOptions → FlowContentNode → NonEmptyList String
renderFlowContentNode options = Lazy.defer \_ →
  case _ of
    Blockquote children →
      renderBlockquote options children
    CodeBlock codeBlockType code →
      renderCodeBlock codeBlockType code
    Heading headingLevel children →
      renderHeading
        options { maxLineLength = top }
        headingLevel
        children
    List isOrdered listItems →
      renderList options isOrdered listItems
    Paragraph children →
      renderParagraph options children
    Rule →
      renderRule

renderPhrasingContentNodes
  ∷ FormattingOptions
  → NonEmptyArray PhrasingContentNode
  → NonEmptyList String
renderPhrasingContentNodes options = foldl f
  (ListNE.singleton "")
  where
  f ∷ NonEmptyList String → PhrasingContentNode → NonEmptyList String
  f acc node =
    case node of
      Emphasis children →
        ListNE.uncons (renderEmphasis options children) #
          \{ head, tail } →
            mapFirst (_ <> head) acc `ListNE.appendFoldable` tail
      Link title url →
        mapFirst (_ <> renderLink title url) acc
      InlineCode code →
        mapFirst (_ <> renderInlineCode code) acc
      LineBreak →
        ListNE.singleton "" <> mapFirst (_ <> "\\") acc
      Text str →
        let
          { head, tail } = ListNE.uncons
            $ formatText options.maxLineLength str
        in
          case ListNE.fromList tail of
            Nothing →
              mapFirst (_ <> StringNE.toString head) acc
            Just lines →
              (StringNE.toString <$> ListNE.reverse lines)
                <> mapFirst (_ <> StringNE.toString head) acc

formatText ∷ Int → NonEmptyString → NonEmptyList NonEmptyString
formatText maxLineLength = unfoldr1 splitAtFurthestSpace
  where
  splitAtFurthestSpace
    ∷ NonEmptyString → NonEmptyString /\ Maybe NonEmptyString
  splitAtFurthestSpace s =
    if StringNE.length s <= maxLineLength then
      s /\ Nothing
    else case StringNE.lastIndexOf' (Pattern " ") maxLineLength s of
      Nothing →
        s /\ Nothing
      Just i →
        let
          { after, before } = StringNE.splitAt i s
        in
          case before, after of
            Just b, Just a →
              b /\ StringNE.stripPrefix (Pattern " ") a
            _, _ →
              s /\ Nothing

renderEmphasis
  ∷ FormattingOptions
  → NonEmptyArray PhrasingContentNode
  → NonEmptyList String
renderEmphasis options = Lazy.defer \_ →
  mapLast (_ <> "_")
    <<< mapFirst ("_" <> _)
    <<< renderPhrasingContentNodes options

renderBlockquote
  ∷ FormattingOptions
  → NonEmptyArray FlowContentNode
  → NonEmptyList String
renderBlockquote options children =
  let
    formatLine s = if String.null s then ">" else "> " <> s

    renderedLines = foldMap1
      (renderFlowContentNode options)
      (ArrayNE.reverse children)

    { init, last } = ListNE.unsnoc renderedLines

    formattedFirstLine = formatLine last
    formattedOtherLines = formatLine <$> List.dropWhile String.null init
  in
    ("" : formattedOtherLines) `ListNE.snoc'` formattedFirstLine

renderCodeBlock ∷ CodeLanguage → Array String → NonEmptyList String
renderCodeBlock codeLanguage code =
  bottom `ListNE.appendFoldable` codeLines <> top
  where
  codeLines ∷ List String
  codeLines = List.reverse $ List.fromFoldable code

  top ∷ NonEmptyList String
  top = ListNE.singleton $ "```" <> renderCodeLanguage codeLanguage

  bottom ∷ NonEmptyList String
  bottom = ListNE.singleton "" <> ListNE.singleton "```"

renderHeading
  ∷ FormattingOptions
  → HeadingLevel
  → NonEmptyArray PhrasingContentNode
  → NonEmptyList String
renderHeading options headingLevel children =
  let
    renderedLines = renderPhrasingContentNodes options children
    formattedLine = String.joinWith ""
      ( String.replaceAll (Pattern "\\") (Replacement "<br/>")
          <$> (Array.fromFoldable $ ListNE.reverse renderedLines)
      )
  in
    ListNE.singleton ""
      <> (ListNE.singleton $ prefix <> formattedLine)
  where
  prefix ∷ String
  prefix = (_ <> " ") $ case headingLevel of
    L1 →
      "#"
    L2 →
      "##"
    L3 →
      "###"
    L4 →
      "####"
    L5 →
      "#####"
    L6 →
      "######"

renderInlineCode ∷ NonEmptyString → String
renderInlineCode code = "`" <> StringNE.toString code <> "`"

renderLink ∷ NonEmptyString → Url → String
renderLink name url = "["
  <> StringNE.toString name
  <> "]("
  <> urlString
  <> ")"
  where
  urlString ∷ String
  urlString = case url of
    ExternalUrl s →
      s
    InternalUrl (HeadingId s) →
      "#" <> s

renderList
  ∷ FormattingOptions
  → Boolean
  → NonEmptyArray (NonEmptyArray FlowContentNode)
  → NonEmptyList String
renderList options isOrdered children =
  foldMap1 renderListItem (ArrayNE.reverse children)
  where
  renderListItem ∷ NonEmptyArray FlowContentNode → NonEmptyList String
  renderListItem nodes =
    let
      renderedLines = foldMap1
        (renderFlowContentNode options)
        (ArrayNE.reverse nodes)
      { init, last } = ListNE.unsnoc renderedLines
      formattedFirstLine = itemPrefix <> last

      formattedOtherLines =
        (\s → if String.null s then s else indent <> s)
          <$> List.dropWhile String.null init
    in
      ("" : formattedOtherLines) `ListNE.snoc'` formattedFirstLine

  indent ∷ String
  indent = String.joinWith ""
    $ Array.replicate (String.length itemPrefix) " "

  itemPrefix ∷ String
  itemPrefix = (if isOrdered then "1." else "-") <> " "

renderParagraph
  ∷ FormattingOptions
  → NonEmptyArray PhrasingContentNode
  → NonEmptyList String
renderParagraph options nodes =
  let
    renderedLines = renderPhrasingContentNodes options nodes
  in
    ListNE.singleton "" <> renderedLines

renderRule ∷ NonEmptyList String
renderRule = ListNE.singleton "" <> ListNE.singleton "---"

data CodeLanguage = Json | Mermaid | PlainText

derive instance Eq CodeLanguage
derive instance Ord CodeLanguage
derive instance Generic CodeLanguage _

instance Show CodeLanguage where
  show = genericShow

data HeadingLevel = L1 | L2 | L3 | L4 | L5 | L6

derive instance Eq HeadingLevel
derive instance Ord HeadingLevel
derive instance Generic HeadingLevel _

instance Show HeadingLevel where
  show = genericShow

renderCodeLanguage ∷ CodeLanguage → String
renderCodeLanguage = case _ of
  Json →
    "json"
  Mermaid →
    "mermaid"
  PlainText →
    "text"

blockquote ∷ ∀ f. Foldable1 f ⇒ f FlowContentNode → FlowContentNode
blockquote = Blockquote <<< ArrayNE.fromFoldable1

codeBlock ∷ ∀ f. Foldable f ⇒ CodeLanguage → f String → FlowContentNode
codeBlock lang = CodeBlock lang <<< Array.fromFoldable

codeBlock' ∷ ∀ f. Foldable f ⇒ f String → FlowContentNode
codeBlock' = CodeBlock PlainText <<< Array.fromFoldable

emphasis
  ∷ ∀ f. Foldable1 f ⇒ f PhrasingContentNode → PhrasingContentNode
emphasis = Emphasis <<< ArrayNE.fromFoldable1

inlineCode ∷ NonEmptyString → PhrasingContentNode
inlineCode = InlineCode

heading1 ∷ ∀ f. Foldable1 f ⇒ f PhrasingContentNode → FlowContentNode
heading1 = Heading L1 <<< ArrayNE.fromFoldable1

heading2 ∷ ∀ f. Foldable1 f ⇒ f PhrasingContentNode → FlowContentNode
heading2 = Heading L2 <<< ArrayNE.fromFoldable1

heading3 ∷ ∀ f. Foldable1 f ⇒ f PhrasingContentNode → FlowContentNode
heading3 = Heading L3 <<< ArrayNE.fromFoldable1

heading4 ∷ ∀ f. Foldable1 f ⇒ f PhrasingContentNode → FlowContentNode
heading4 = Heading L4 <<< ArrayNE.fromFoldable1

heading5 ∷ ∀ f. Foldable1 f ⇒ f PhrasingContentNode → FlowContentNode
heading5 = Heading L5 <<< ArrayNE.fromFoldable1

heading6 ∷ ∀ f. Foldable1 f ⇒ f PhrasingContentNode → FlowContentNode
heading6 = Heading L6 <<< ArrayNE.fromFoldable1

lineBreak ∷ PhrasingContentNode
lineBreak = LineBreak

link ∷ NonEmptyString → Url → PhrasingContentNode
link = Link

orderedList ∷ ∀ f. Foldable1 f ⇒ f (f FlowContentNode) → FlowContentNode
orderedList = List true
  <<< map ArrayNE.fromFoldable1
  <<< ArrayNE.fromFoldable1

unorderedList
  ∷ ∀ f. Foldable1 f ⇒ f (f FlowContentNode) → FlowContentNode
unorderedList = List false
  <<< map ArrayNE.fromFoldable1
  <<< ArrayNE.fromFoldable1

paragraph ∷ ∀ f. Foldable1 f ⇒ f PhrasingContentNode → FlowContentNode
paragraph = Paragraph <<< ArrayNE.fromFoldable1

rule ∷ FlowContentNode
rule = Rule

text ∷ NonEmptyString → PhrasingContentNode
text = Text

urlOfHeading ∷ NonEmptyString → Url
urlOfHeading = InternalUrl <<< formatAnchor

urlOfLocalFile ∷ NonEmptyString → Url
urlOfLocalFile = ExternalUrl <<< StringNE.toString

formatAnchor ∷ NonEmptyString → HeadingId
formatAnchor = HeadingId
  <<< String.replaceAll (Pattern " ") (Replacement "-")
  <<< String.replaceAll (Pattern ".") (Replacement "")
  <<< String.replaceAll (Pattern ",") (Replacement "")
  <<< String.replaceAll (Pattern "'") (Replacement "")
  <<< String.replaceAll (Pattern "(") (Replacement "")
  <<< String.replaceAll (Pattern ")") (Replacement "")
  <<< String.toLower
  <<< StringNE.toString
