import A21ExpressionsA22Declarations
import Parsing

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