import Parsing

doTokenTest inputString expectedTokens =
  parseWith inputString parseTokenListC