module Data.Markdown
  ( CodeBlockType(..)
  , Document
  , Node
  , codeBlock
  , codeBlock'
  , heading1
  , heading2
  , heading3
  , heading4
  , heading5
  , heading6
  , link
  , list
  , paragraph
  , render
  , rule
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap)
import Data.String as String

type Document = Array Node

render ∷ Document → String
render = foldMap f
  where
  f ∷ Node → String
  f = case _ of
    CodeBlock codeBlockType code →
      renderCodeBlock codeBlockType code <> "\n"
    Heading headingLevel text →
      renderHeading headingLevel text
    Link name url →
      renderLink name url
    List nodes →
      String.joinWith "\n" (("- " <> _) <<< f <$> nodes) <> "\n"
    Paragraph text →
      text <> "\n\n"
    Rule →
      "---\n"

renderCodeBlock ∷ CodeBlockType → String → String
renderCodeBlock codeBlockType code =
  "```"
    <> renderCodeBlockType codeBlockType
    <> "\n"
    <> code
    <> "\n```"

renderHeading ∷ HeadingLevel → String → String
renderHeading headingLevel text =
  prefix <> " " <> text <> "\n"
  where
  prefix ∷ String
  prefix = case headingLevel of
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

renderLink ∷ String → String → String
renderLink name url = "[" <> name <> "](" <> url <> ")"

data Node
  = CodeBlock CodeBlockType String
  | Heading HeadingLevel String
  | Link String String
  | List (Array Node)
  | Paragraph String
  | Rule

derive instance Eq Node
derive instance Ord Node

data CodeBlockType = Json | Mermaid | Text

derive instance Eq CodeBlockType
derive instance Ord CodeBlockType

data HeadingLevel = L1 | L2 | L3 | L4 | L5 | L6

derive instance Eq HeadingLevel
derive instance Ord HeadingLevel

renderCodeBlockType ∷ CodeBlockType → String
renderCodeBlockType = case _ of
  Json →
    "json"
  Mermaid →
    "mermaid"
  Text →
    ""

codeBlock ∷ CodeBlockType → String → Node
codeBlock = CodeBlock

codeBlock' ∷ String → Node
codeBlock' = CodeBlock Text

heading1 ∷ String → Node
heading1 = Heading L1

heading2 ∷ String → Node
heading2 = Heading L2

heading3 ∷ String → Node
heading3 = Heading L3

heading4 ∷ String → Node
heading4 = Heading L4

heading5 ∷ String → Node
heading5 = Heading L5

heading6 ∷ String → Node
heading6 = Heading L6

link ∷ String → String → Node
link = Link

list ∷ ∀ f. Foldable f ⇒ f Node → Node
list = List <<< Array.fromFoldable

paragraph ∷ String → Node
paragraph = Paragraph

rule ∷ Node
rule = Rule
