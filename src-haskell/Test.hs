import A21ExpressionsA22Declarations
import A23Statements (compoundStatementC)
import A24ExternalDefinitions (declSpecWithoutEndCD, declarationListC, functionDefinitionC, translationUnitC)
import Parsing
import Tokenizer

parseTest =
  parseWith
    "123*123*123*123"
    ( getnode $
        ( do
            getnode $ do
              pchar '1'
              pchar '2'
              pchar '3'
            ( seterr "asdasdasdA" $ getnode $ pkleene $ do
                pstr "*"
                pstr "123"
                pure "asdasdasd"
              )
        )
    )

testcase2 = "int main() { test = 1 + 2; }"

tl = parseWith "true ? 1 : 123 * 456 + 0x789" parseTokenListC

cst1 = case tl of
  CSTExpression d _ _ -> parseWith d expressionC
  _ -> error "lexing failed"

t2 = parseWith testcase2 parseTokenListC

cst2 = case t2 of
  CSTExpression d _ _ -> parseWith d functionDefinitionC

testParseNode testname str parser =
  case parseWith str parseTokenListC of
    CSTExpression tokens _ _ ->
      case parseWith tokens parser of
        CSTExpression cst _ endptr -> testname ++ ": " ++ show cst
        CSTError {} -> testname ++ ": " ++ "parsing failed"
    CSTError {} -> testname ++ ": " ++ "lexing failed"

tests =
  foldl
    (\a b -> a ++ "\n\n" ++ b)
    ""
    [ testParseNode
        "TYPENAME"
        "int"
        typeNameC,
      testParseNode
        "DECLSPEC"
        "int"
        declarationSpecifiersC,
      testParseNode
        "FUNC DECLARATOR"
        "main()"
        declaratorC,
      testParseNode
        "FUNC BODY"
        "{ test = 1 + 2; }"
        compoundStatementC,
      testParseNode
        "FUNC DECLSPEC AGAIN"
        "int (main)() { test = 1 + 2; }"
        ( do
            a <- declarationSpecifiersC
            pure (a)
        ),
      testParseNode
        "LAZIER DECLSPEC"
        "int main"
        declSpecWithoutEndCD,
      testParseNode
        "LAZIER DECLSPEC WITH IDENT AFTER"
        "int main"
        ( do
            ds <- declSpecWithoutEndCD
            id <- identifierT
            pure id
        ),
      testParseNode
        "FUNC DEFINITION"
        "int main(int argc, char **argv) { test = 1 + 2; }"
        functionDefinitionC
    ]

asdasd =
  case (parseWith "int main" parseTokenListC) of
    CSTExpression e _ _ ->
      let result = (parseWith e declSpecWithoutEndCD)
       in (show result)
            ++ "\n\n"
            ++ (show $ nodeStart result)
            ++ "\n\n"
            ++ (show $ nodeEnd result)