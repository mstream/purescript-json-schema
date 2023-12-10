module Test.Unit.Spec.Data.Markdown (spec) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (foldMap)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as ListNE
import Data.Markdown
  ( CodeLanguage(..)
  , Document
  , FlowContentNode
  , FormattingOptions
  , PhrasingContentNode
  )
import Data.Markdown as M
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.TestSpec (TestSpec)
import Type.Proxy (Proxy(..))

text ∷ String → PhrasingContentNode
text = unsafePartial $ M.text <<< StringNE.unsafeFromString

spec ∷ TestSpec
spec = do
  describe "formatting" do
    describe "of text" do
      textFormattingTestCase
        "keeps single line if text is short enough"
        10
        (StringNE.nes (Proxy ∷ Proxy "aaaa"))
        (ListNE.singleton (StringNE.nes (Proxy ∷ Proxy "aaaa")))

      textFormattingTestCase
        "splits text into multiple lines if text is to long"
        10
        (StringNE.nes (Proxy ∷ Proxy "aaaa bbbb cccc dddd eeee"))
        ( (StringNE.nes (Proxy ∷ Proxy "aaaa bbbb")) `ListNE.cons'`
            List.fromFoldable
              [ StringNE.nes (Proxy ∷ Proxy "cccc dddd")
              , StringNE.nes (Proxy ∷ Proxy "eeee")
              ]
        )

  describe "rendering" do
    describe "of phrasing content nodes" do
      phrasingContentNodesTestCase
        "text without line breaks"
        ( text "l1w1" `ArrayNE.cons'`
            [ text ",", text "l1w2", text ",", text "l1w3" ]
        )
        [ "l1w1,l1w2,l1w3" ]

      phrasingContentNodesTestCase
        "text with line breaks"
        ( text "l1w1" `ArrayNE.cons'`
            [ text ","
            , text "l1w2"
            , M.lineBreak
            , text "l2w1"
            , M.lineBreak
            , text "l3w1"
            ]
        )
        [ "l3w1", "l2w1\\", "l1w1,l1w2\\" ]

      phrasingContentNodesTestCase
        "emphasis"
        ( ArrayNE.singleton
            $ M.emphasis
            $ text "l1w1"
                `ArrayNE.cons'` [ text ",", text "l1w2" ]
        )
        [ "_l1w1,l1w2_" ]

      phrasingContentNodesTestCase
        "link"
        ( ArrayNE.singleton
            $ M.link
                (StringNE.nes (Proxy ∷ Proxy "title"))
                ( M.urlOfLocalFile $ StringNE.nes
                    (Proxy ∷ Proxy "dir1/file1")
                )
        )
        [ "[title](dir1/file1)" ]

      phrasingContentNodesTestCase
        "inline code"
        ( ArrayNE.singleton
            $ M.inlineCode
            $ StringNE.nes (Proxy ∷ Proxy "code1")
        )
        [ "`code1`" ]

    describe "of flow content nodes" do

      flowContentNodeTestCase
        "rule"
        M.rule
        [ "", "---" ]

      flowContentNodeTestCase
        "heading"
        (M.heading3 $ ArrayNE.singleton $ text "w1")
        [ "", "### w1" ]

      flowContentNodeTestCase
        "multi-line heading"
        ( M.heading3 $ text "l1w1" `ArrayNE.cons'`
            [ text ","
            , text "l1w2"
            , M.lineBreak
            , text "l2w1"
            , text ","
            , text "l2w2"
            ]
        )
        [ "", "### l1w1,l1w2<br/>l2w1,l2w2" ]

      flowContentNodeTestCase
        "code block"
        (M.codeBlock' [ "l1", "l2", "l3" ])
        [ "", "```", "l3", "l2", "l1", "```text" ]

      flowContentNodeTestCase
        "paragraph"
        ( M.paragraph $ text "l1w1" `ListNE.cons'`
            List.fromFoldable
              [ text ","
              , text "l1w2"
              , M.lineBreak
              , text "l2w1"
              , text ","
              , text "l2w2"
              ]
        )
        [ "", "l2w1,l2w2", "l1w1,l1w2\\" ]

      flowContentNodeTestCase
        "blockquote"
        ( M.blockquote
            $ ArrayNE.singleton
            $ M.paragraph
            $ text "l1w1" `ListNE.cons'`
                List.fromFoldable
                  [ text ","
                  , text "l1w2"
                  , M.lineBreak
                  , text "l2w1"
                  , text ","
                  , text "l2w2"
                  ]
        )
        [ "", "> l2w1,l2w2", "> l1w1,l1w2\\" ]

      flowContentNodeTestCase
        "list"
        ( let
            paragraph id = M.paragraph
              $ text (id <> ": ") `ListNE.cons'`
                  List.fromFoldable
                    [ text "l1w1"
                    , text ","
                    , text "l1w2"
                    , M.lineBreak
                    , text "l2w1"
                    , text ","
                    , text "l2w2"
                    ]

            item n = paragraph (show n <> ".1")
              `ListNE.cons'` List.fromFoldable
                [ paragraph (show n <> ".2")
                , paragraph (show n <> ".3")
                ]

          in
            M.unorderedList $ item 1 `ListNE.cons'` List.fromFoldable
              [ item 2, item 3 ]
        )
        [ ""
        , "  l2w1,l2w2"
        , "  3.3: l1w1,l1w2\\"
        , ""
        , "  l2w1,l2w2"
        , "  3.2: l1w1,l1w2\\"
        , ""
        , "  l2w1,l2w2"
        , "- 3.1: l1w1,l1w2\\"
        , ""
        , "  l2w1,l2w2"
        , "  2.3: l1w1,l1w2\\"
        , ""
        , "  l2w1,l2w2"
        , "  2.2: l1w1,l1w2\\"
        , ""
        , "  l2w1,l2w2"
        , "- 2.1: l1w1,l1w2\\"
        , ""
        , "  l2w1,l2w2"
        , "  1.3: l1w1,l1w2\\"
        , ""
        , "  l2w1,l2w2"
        , "  1.2: l1w1,l1w2\\"
        , ""
        , "  l2w1,l2w2"
        , "- 1.1: l1w1,l1w2\\"
        ]

    describe "of document" do

      testCase
        "renders multiple words"
        (M.phrasingContent <<< text <$> [ "word1", "word2", "word3" ])
        [ "word1word2word3" ]

      testCase
        "renders multiple words with line breaks"
        ( M.phrasingContent <$>
            [ text "word1"
            , M.lineBreak
            , text "word2"
            , M.lineBreak
            , text "word3"
            ]
        )
        [ "word1\\"
        , "word2\\"
        , "word3"
        ]

      testCase
        "renders paragraphs"
        ( M.document
            $ M.paragraph <<< ArrayNE.singleton <<< text
                <$>
                  [ "word1"
                  , "word2"
                  , "word3"
                  ]
        )
        [ "word1"
        , ""
        , "word2"
        , ""
        , "word3"
        ]

    testCase
      "renders headings"
      [ M.flowContent $ M.heading1 $ ArrayNE.singleton $ text "word1"
      , M.flowContent $ M.heading2 $ ArrayNE.singleton $ text "word2"
      , M.flowContent $ M.heading3 $ ArrayNE.singleton $ text "word3"
      ]
      [ "# word1"
      , ""
      , "## word2"
      , ""
      , "### word3"
      ]

    testCase
      "renders emphasized words"
      [ M.phrasingContent $ M.emphasis $ text "word1"
          `ArrayNE.cons'` [ text "word2" ]
      ]
      [ "_word1word2_" ]

    testCase
      "renders emphasized words inside paragraph"
      [ M.flowContent
          $ M.paragraph
          $ (M.emphasis $ ArrayNE.singleton $ text "word1")
              `ArrayNE.cons'` [ text "word2" ]
      ]
      [ "_word1_word2" ]

    testCase
      "renders rules"
      [ M.flowContent M.rule
      , M.flowContent M.rule
      ]
      [ "---"
      , ""
      , "---"
      ]

    testCase
      "renders inline code"
      [ M.phrasingContent
          $ M.inlineCode
          $ StringNE.nes (Proxy ∷ Proxy "word1")
      ]
      [ "`word1`" ]

    testCase
      "renders code blocks"
      [ M.flowContent $ M.codeBlock' [ "word1", "word2" ]
      , M.flowContent $ M.codeBlock Json [ "{}" ]
      ]
      [ "```text"
      , "word1"
      , "word2"
      , "```"
      , ""
      , "```json"
      , "{}"
      , "```"
      ]

    testCase
      "renders blockquoted paragraphs"
      ( M.document $ Array.singleton $ M.blockquote
          $
            ( M.paragraph $ text "word1" `ArrayNE.cons'`
                [ M.lineBreak
                , text "word2"
                , M.lineBreak
                , text "word3"
                ]
            ) `ArrayNE.cons'`
              ( M.paragraph <<< ArrayNE.singleton <<< text <$>
                  [ "word4"
                  , "word5"
                  ]
              )
      )
      [ "> word1\\"
      , "> word2\\"
      , "> word3"
      , ">"
      , "> word4"
      , ">"
      , "> word5"
      ]

    testCase
      "renders simple unordered list"
      ( M.document
          $ Array.singleton
          $ M.unorderedList
          $
            ArrayNE.singleton
              <<< M.paragraph
              <<< ArrayNE.singleton
              <<< text
              <$> "word1"
                `ArrayNE.cons'`
                  [ "word2", "word3" ]
      )
      [ "- word1"
      , ""
      , "- word2"
      , ""
      , "- word3"
      ]

    testCase
      "renders simple ordered list"
      ( M.document
          $ Array.singleton
          $ M.orderedList
          $
            ArrayNE.singleton
              <<< M.paragraph
              <<< ArrayNE.singleton
              <<< text
              <$> "word1"
                `ArrayNE.cons'`
                  [ "word2", "word3" ]
      )
      [ "1. word1"
      , ""
      , "1. word2"
      , ""
      , "1. word3"
      ]

    testCase
      "renders unordered list with multiple paragraphs per each item"
      ( let
          paragraph id = M.paragraph
            $ ArrayNE.singleton
            $ text
            $ "paragraph " <> id

          item i =
            paragraph (show i <> ".1")
              `ArrayNE.cons'`
                [ paragraph (show i <> ".2")
                , paragraph (show i <> ".3")
                ]

        in
          M.document
            $ Array.singleton
            $ M.unorderedList
            $ item 1 `ArrayNE.cons'`
                [ item 2, item 3 ]
      )
      [ "- paragraph 1.1"
      , ""
      , "  paragraph 1.2"
      , ""
      , "  paragraph 1.3"
      , ""
      , "- paragraph 2.1"
      , ""
      , "  paragraph 2.2"
      , ""
      , "  paragraph 2.3"
      , ""
      , "- paragraph 3.1"
      , ""
      , "  paragraph 3.2"
      , ""
      , "  paragraph 3.3"
      ]

    testCase
      "renders unordered list with multi line paragraph items"
      ( M.document
          $ Array.singleton
          $ M.unorderedList
          $
            ( ArrayNE.singleton $ M.paragraph $ (text "word1")
                `ArrayNE.cons'`
                  [ M.lineBreak
                  , text "word2"
                  , M.lineBreak
                  , text "word3"
                  ]
            )
              `ArrayNE.cons'`
                [ ArrayNE.singleton $ M.paragraph $ (text "word4")
                    `ArrayNE.cons'`
                      [ text "word5"
                      , M.lineBreak
                      , text "word6"
                      ]
                , ArrayNE.singleton $ M.paragraph $ (text "word7")
                    `ArrayNE.cons'`
                      [ M.lineBreak
                      , text "word8"
                      , text "word9"
                      ]

                ]
      )
      [ "- word1\\"
      , "  word2\\"
      , "  word3"
      , ""
      , "- word4word5\\"
      , "  word6"
      , ""
      , "- word7\\"
      , "  word8word9"
      ]

    testCase
      "renders unordered list of unordered lists"
      ( let
          list = M.unorderedList
            $
              ArrayNE.singleton
                <<< M.paragraph
                <<< ArrayNE.singleton
                <<<
                  text
                <$> "word1"
                  `ArrayNE.cons'`
                    [ "word2", "word3" ]
        in
          M.document $ Array.singleton $ M.unorderedList $
            ArrayNE.singleton <$> (list `ArrayNE.cons'` [ list, list ])
      )
      [ "- - word1"
      , ""
      , "  - word2"
      , ""
      , "  - word3"
      , ""
      , "- - word1"
      , ""
      , "  - word2"
      , ""
      , "  - word3"
      , ""
      , "- - word1"
      , ""
      , "  - word2"
      , ""
      , "  - word3"
      ]

    testCase
      "renders blockquoted list"
      ( let
          item n = ArrayNE.singleton $ M.paragraph
            $ text (show n <> ": ") `ArrayNE.cons'`
                [ text "l1w1"
                , text ","
                , text "l1w2"
                , M.lineBreak
                , text "l2w1"
                , text ","
                , text "l2w2"
                ]
        in
          M.document
            $ Array.singleton
            $ M.blockquote
            $ ArrayNE.singleton
            $ M.unorderedList
            $ item 1 `ArrayNE.cons'` [ item 2, item 3 ]
      )
      [ "> - 1: l1w1,l1w2\\"
      , ">   l2w1,l2w2"
      , ">"
      , "> - 2: l1w1,l1w2\\"
      , ">   l2w1,l2w2"
      , ">"
      , "> - 3: l1w1,l1w2\\"
      , ">   l2w1,l2w2"
      ]

testCase ∷ String → Document → Array String → TestSpec
testCase title document expectedLines = it title do
  let
    actual = M.render formattingOptions document
    expected = foldMap (_ <> "\n") expectedLines

  actual `shouldEqual` expected

flowContentNodeTestCase
  ∷ String
  → FlowContentNode
  → Array String
  → TestSpec
flowContentNodeTestCase title node expected = it title
  do
    let
      actual = M.renderFlowContentNode formattingOptions node

    (ListNE.toUnfoldable actual) `shouldEqual`
      (Array.fromFoldable expected)

phrasingContentNodesTestCase
  ∷ String
  → NonEmptyArray PhrasingContentNode
  → Array String
  → TestSpec
phrasingContentNodesTestCase title nodes expected = it title
  do
    let
      actual = M.renderPhrasingContentNodes formattingOptions nodes

    (ListNE.toUnfoldable actual) `shouldEqual`
      (Array.fromFoldable expected)

textFormattingTestCase
  ∷ String
  → Int
  → NonEmptyString
  → NonEmptyList NonEmptyString
  → TestSpec
textFormattingTestCase title maxLineLength unformattedString expected =
  it title do
    let
      actual = M.formatText maxLineLength unformattedString

    actual `shouldEqual` expected

formattingOptions ∷ FormattingOptions
formattingOptions = { maxLineLength: top }
