module Data.Mermaid.FlowChart
  ( FlowChartDef(..)
  , NodeDef(..)
  , NodeShape(..)
  , Orientation(..)
  , Segment
  , box
  , boxWithSubroutine
  , capsule
  , circle
  , dottedArrow
  , flag
  , normalArrow
  , normalArrowWithAnnotation
  , render
  , style
  , subGraph
  , thickArrow
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))

data Orientation
  = TopToBottom
  | BottomToTop
  | RightToLeft
  | LeftToRight

data NodeShape = Box | BoxWithSubroutine | Capsule | Circle | Flag

type NodeDef = { id ∷ String, label ∷ String, shape ∷ NodeShape }

data Segment
  = Link LinkDef
  | Node NodeDef
  | Style String (Array (String /\ String))
  | SubGraph String (Array Segment)

type LinkDef =
  { annotation ∷ Maybe String
  , from ∷ String
  , style ∷ LinkStyle
  , to ∷ String
  }

data LinkStyle = DottedArrow | NormalArrow | ThickArrow

data FlowChartDef = FlowChartDef Orientation (Array Segment)

style ∷ String → Array (String /\ String) → Segment
style id attrs = Style id attrs

dottedArrow ∷ String → String → Segment
dottedArrow from to = Link
  { annotation: Nothing, from, style: DottedArrow, to }

normalArrow ∷ String → String → Segment
normalArrow from to = Link
  { annotation: Nothing, from, style: NormalArrow, to }

normalArrowWithAnnotation ∷ String → String → String → Segment
normalArrowWithAnnotation annotation from to = Link
  { annotation: Just annotation, from, style: NormalArrow, to }

thickArrow ∷ String → String → Segment
thickArrow from to = Link
  { annotation: Nothing, from, style: ThickArrow, to }

box ∷ String → String → Segment
box id label = Node { id, label, shape: Box }

boxWithSubroutine ∷ String → String → Segment
boxWithSubroutine id label = Node
  { id, label, shape: BoxWithSubroutine }

capsule ∷ String → String → Segment
capsule id label = Node { id, label, shape: Capsule }

circle ∷ String → String → Segment
circle id label = Node { id, label, shape: Circle }

flag ∷ String → String → Segment
flag id label = Node { id, label, shape: Flag }

subGraph ∷ String → Array Segment → Segment
subGraph id segments = SubGraph id segments

render ∷ FlowChartDef → String
render = case _ of
  FlowChartDef orientation segments →
    "flowchart "
      <> renderOrientation orientation
      <> "\n"
      <> String.joinWith "\n" (renderSegment 4 <$> segments)

renderSegment ∷ Int → Segment → String
renderSegment indentation = case _ of
  Link connectionDef →
    renderLink indentation connectionDef

  Node nodeDef →
    renderNode indentation nodeDef

  Style id attrs →
    renderStyle indentation id attrs

  SubGraph id segments →
    renderIndentation indentation
      <> "subgraph "
      <> id
      <> "\n"
      <>
        ( Array.foldMap
            (\segment → renderSegment (indentation + 4) segment <> "\n")
            segments
        )
      <> renderIndentation indentation
      <> "end"

renderLink ∷ Int → LinkDef → String
renderLink indentation def =
  renderIndentation indentation
    <> def.from
    <> " "
    <> arrow
    <> annotation
    <> " "
    <> def.to
  where
  annotation ∷ String
  annotation = maybe "" (\s → "|" <> s <> "|") def.annotation

  arrow ∷ String
  arrow = case def.style of
    DottedArrow →
      "-.->"

    NormalArrow →
      "-->"

    ThickArrow →
      "==>"

renderNode ∷ Int → NodeDef → String
renderNode indentation { id, label, shape } =
  renderIndentation indentation
    <> id
    <> renderLabel ("\"" <> label <> "\"")
  where
  renderLabel s = case shape of
    Box → "[" <> s <> "]"
    BoxWithSubroutine → "[[" <> s <> "]]"
    Capsule → "(" <> s <> ")"
    Circle → "((" <> s <> "))"
    Flag → ">" <> s <> "]"

renderStyle ∷ Int → String → Array (String /\ String) → String
renderStyle indentation id attrs =
  renderIndentation indentation
    <> "style "
    <> id
    <> " "
    <> String.joinWith "," (renderAttr <$> attrs)
  where
  renderAttr (name /\ value) = name <> ":" <> value

renderIndentation ∷ Int → String
renderIndentation n =
  String.joinWith "" $ Array.replicate n " "

renderOrientation ∷ Orientation → String
renderOrientation = case _ of
  TopToBottom →
    "TD"

  BottomToTop →
    "BT"

  RightToLeft →
    "RL"

  LeftToRight →
    "LR"
