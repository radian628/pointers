module A12Keywords where

import GrammarTypes
import Parsing

--- A.1.2 Keywords

keywordC =
  paltv $
    map
      ( \(kw, str) ->
          kw
            <$ pstr str
      )
      [ (KeywordAuto, "auto"),
        (KeywordBreak, "break"),
        (KeywordCase, "case"),
        (KeywordChar, "char"),
        (KeywordConst, "const"),
        (KeywordContinue, "continue"),
        (KeywordDefault, "default"),
        (KeywordDo, "do"),
        (KeywordDouble, "double"),
        (KeywordElse, "else"),
        (KeywordEnum, "enum"),
        (KeywordExtern, "extern"),
        (KeywordFloat, "float"),
        (KeywordFor, "for"),
        (KeywordGoto, "goto"),
        (KeywordIf, "if"),
        (KeywordInline, "inline"),
        (KeywordInt, "int"),
        (KeywordLong, "long"),
        (KeywordRegister, "register"),
        (KeywordRestrict, "restrict"),
        (KeywordReturn, "return"),
        (KeywordShort, "short"),
        (KeywordSigned, "signed"),
        (KeywordSizeof, "sizeof"),
        (KeywordStatic, "static"),
        (KeywordStruct, "struct"),
        (KeywordSwitch, "switch"),
        (KeywordTypedef, "typedef"),
        (KeywordUnion, "union"),
        (KeywordUnsigned, "unsigned"),
        (KeywordVoid, "void"),
        (KeywordVolatile, "volatile"),
        (KeywordWhile, "while"),
        (KeywordAlignAs, "_Alignas"),
        (KeywordAlignOf, "_Alignof"),
        (KeywordAtomic, "_Atomic"),
        (KeywordBool, "_Bool"),
        (KeywordComplex, "_Complex"),
        (KeywordGeneric, "_Generic"),
        (KeywordImaginary, "_Imaginary"),
        (KeywordNoreturn, "_Noreturn"),
        (KeywordStaticAssert, "_Static_assert"),
        (KeywordThreadLocal, "_Thread_local")
      ]