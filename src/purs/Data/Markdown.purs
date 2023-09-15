module Data.Markdown
  ( CodeLanguage(..)
  , Document
  , FlowContentNode(..)
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

mapOtherThanLast' ∷ ∀ a. (a → a) → List a → List a
mapOtherThanLast' f = ListNE.fromList >>> case _ of
  Just xs →
    ListNE.toList $ mapOtherThanLast f xs
  Nothing →
    Nil

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

appendWith
  ∷ ∀ a. (a → a → a) → NonEmptyList a → NonEmptyList a → NonEmptyList a
appendWith f left = ListNE.uncons >>>
  \{ head: headOfRight, tail: tailOfRight } →
    mapLast (\x → f x headOfRight) left
      `ListNE.appendFoldable` tailOfRight

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
  | Text String

derive instance Eq PhrasingContentNode
derive instance Ord PhrasingContentNode
derive instance Generic PhrasingContentNode _

instance Show PhrasingContentNode where
  show node = genericShow node

renderMermaid ∷ FlowChartDef → FlowContentNode
renderMermaid flowChartDef =
  codeBlock Mermaid code
  where
  code ∷ Array String
  code = String.split (Pattern "\n") $ FlowChart.render flowChartDef

render ∷ Document → String
render = (_ <> "\n")
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
        renderedLines = ListNE.toList $ renderFlowContentNode
          flowContentNode

        mergedLines = renderedLines <> acc
      in
        mergedLines
    PhrasingContent phrasingContentNode →
      let
        renderedLines = ListNE.toList
          $ renderPhrasingContentNodes
          $ ArrayNE.singleton phrasingContentNode

        mergedLines = appendWith'
          (flip (<>))
          renderedLines
          acc

      in
        mergedLines

renderFlowContentNode ∷ FlowContentNode → NonEmptyList String
renderFlowContentNode = Lazy.defer \_ →
  case _ of
    Blockquote children →
      renderBlockquote children
    CodeBlock codeBlockType code →
      renderCodeBlock codeBlockType code
    Heading headingLevel children →
      renderHeading headingLevel children
    List isOrdered listItems →
      renderList isOrdered listItems
    Paragraph children →
      renderParagraph children
    Rule →
      renderRule

renderPhrasingContentNodes
  ∷ NonEmptyArray PhrasingContentNode → NonEmptyList String
renderPhrasingContentNodes = foldl f
  (ListNE.singleton "")
  where
  f ∷ NonEmptyList String → PhrasingContentNode → NonEmptyList String
  f acc node =
    case node of
      Emphasis children →
        ListNE.uncons (renderEmphasis children) # \{ head, tail } →
          mapFirst (_ <> head) acc `ListNE.appendFoldable` tail
      Link title url →
        mapFirst (_ <> renderLink title url) acc
      InlineCode code →
        mapFirst (_ <> renderInlineCode code) acc
      LineBreak →
        ListNE.singleton "" <> mapFirst (_ <> "\\") acc
      Text s →
        mapFirst (_ <> s) acc

renderEmphasis ∷ NonEmptyArray PhrasingContentNode → NonEmptyList String
renderEmphasis = Lazy.defer \_ →
  mapLast (_ <> "_")
    <<< mapFirst ("_" <> _)
    <<< renderPhrasingContentNodes

renderBlockquote ∷ NonEmptyArray FlowContentNode → NonEmptyList String
renderBlockquote children =
  let
    formatLine s = if String.null s then ">" else "> " <> s

    renderedLines = foldMap1
      renderFlowContentNode
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
  ∷ HeadingLevel
  → NonEmptyArray PhrasingContentNode
  → NonEmptyList String
renderHeading headingLevel children =
  let
    renderedLines = renderPhrasingContentNodes children
    formattedLines = mapFirst (prefix <> _)
      $ mapTail (indentation <> _) renderedLines
  in
    ListNE.singleton "" <> formattedLines
  where
  indentation ∷ String
  indentation = String.joinWith ""
    $ Array.replicate (String.length prefix) " "

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
  ∷ Boolean
  → NonEmptyArray (NonEmptyArray FlowContentNode)
  → NonEmptyList String
renderList isOrdered children =
  foldMap1 renderListItem (ArrayNE.reverse children)
  where
  renderListItem ∷ NonEmptyArray FlowContentNode → NonEmptyList String
  renderListItem nodes =
    let
      renderedLines = foldMap1
        renderFlowContentNode
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
  ∷ NonEmptyArray PhrasingContentNode → NonEmptyList String
renderParagraph nodes =
  let
    renderedLines = renderPhrasingContentNodes nodes
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

text ∷ String → PhrasingContentNode
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
