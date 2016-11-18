import Text.Trifecta
import Text.Parser.Token
import Text.Trifecta.Delta



naturalparser :: Parser Integer
naturalparser = natural
-- use the two modules 'trifecta' and 'parsers'

parseString naturalparser  (Columns 0 0) "123"

