module Test.Unit.Spec.JsonSchema.Codec where

{-
type CodecExample = Example Unit Unit

examples ∷ Array CodecExample
examples = []

spec ∷ TestSpec
spec = describe "Codec" do

  describe "parseSchema and printSchema" do

    generativeTestCase Long
      "Schemata do not change after being printed and parsed back"
      do
        originalSchema ← SchemaGen.genSchema
        let
          printedOriginalSchema = Printing.printSchema originalSchema
          parsingResult = Parsing.parseSchema printedOriginalSchema
        pure case parsingResult of
          Left errorMessage →
            failWithDetails
              ("Parsing has failed: " <> errorMessage)
              { originalSchema
              , printedOriginalSchema: A.stringify printedOriginalSchema
              }
          Right parsedSchema →
            if (parsedSchema == originalSchema) then Success
            else
              failWithDetails
                "Schemata do not match"
                { originalSchema
                , parsedSchema
                , printedOriginalSchema: A.stringify
                    printedOriginalSchema
                , printedParsedSchema: A.stringify
                    $ Printing.printSchema parsedSchema
                }
-}
