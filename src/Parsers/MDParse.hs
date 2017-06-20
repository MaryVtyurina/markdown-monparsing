module MDParse where

import Control.Monad
import Data.Char
import ParserCombinators
import Control.Applicative ((<|>), some)

-----------------------------------------------------------------
---------------------------Ограничения---------------------------
-----------------------------------------------------------------
-- Всех не перечесть, парсим пока только заголовки и параграфы простого текста,
-- а также списки; вложенные конструкции не допускаются


-------------------Data Types-------------------

type Document = [Block]

-- |Represents block entity
data Block = Blank
           | Header (Int,Line)
           | Paragraph [Line]
           | UnorderedList [Line]
           | BlockQuote [Line]
  deriving (Show,Eq)

-- |Represents line as list of inline elements (words)
data Line = Empty | NonEmpty [Inline]
  deriving (Show,Eq)

-- |Represent inline entity, just a string for this moment
-- Что делать с пунктуацией и пробелами?
data Inline = Plain String
            | Bold String
            | Italic String
            | Monospace String
  deriving (Show,Eq)

-----------------------------------------------------------------
-------------------Parsers for Inline elements-------------------
-----------------------------------------------------------------
-------------------Helper Parsers-----------------
punctuation :: Parser Char
punctuation = foldl1 (<|>) (map char marks)
  where marks = ".,:;!?"

alphanumsWithPunctuation :: Parser String
alphanumsWithPunctuation = some (alphanum <|> punctuation)

-- |Sequence of alphanums with punctuations and spaces
sentence :: Parser String
sentence =
  some (do
    w <- alphanumsWithPunctuation
    s <- (many (char ' '))
    return $ w ++ s
  ) >>= return . concat

-------------------Parsers for single word (word may be plain, bold or italic)-------------------

-- |Parse plain text
plain :: Parser Inline
plain = do
  txt <- sentence
  return . Plain $ txt

-- |Parse italic text (html <em>)
italic :: Parser Inline
italic = do
  txt <- bracket (char '*') sentence (char '*') <|>
         bracket (char '_') sentence (char '_')
  p   <- many punctuation
  return . Italic $ txt ++ p

-- |Parse bold text (html <strong>)
bold :: Parser Inline
bold = do
  txt <- bracket asterisks sentence asterisks <|>
       bracket underlines sentence underlines
  p   <- many punctuation
  return . Bold $ txt ++ p
  where
    asterisks = char '*' >> char '*'
    underlines = char '_' >> char '_'

monospace :: Parser Inline
monospace = do
  txt <- bracket (char '`') sentence (char '`')
  p   <- many punctuation
  return . Monospace $ txt ++ p
-----------------------------------------------------------------
-------------------Parser for Lines-------------------
-----------------------------------------------------------------

-- |Парсит слова (plain, bold, italic), разделённые одним пробелом
-- Ломается, если, например, внутри plain есть *
line :: Parser Line
line = emptyLine +++ nonEmptyLine
--
emptyLine :: Parser Line
emptyLine = do
    many (sat wspaceOrTab)
    x <- char '\n'
    return $ Empty

-- TODO: Получилось как-то сложно, подумать, как бы попроще
nonEmptyLine :: Parser Line
nonEmptyLine = do
  many (sat wspaceOrTab)
  l <- sepby (bold <|> italic <|> plain <|> monospace) (many (char ' '))
  many (sat wspaceOrTab)
  char '\n'
  return . NonEmpty $ l

-----------------------------------------------------------------
-------------------Parsers for Block elements--------------------
-----------------------------------------------------------------

-- |Parse header
-- поддерживаются только заголовки в стиле #
header :: Parser Block
header = do
  hashes <- token (some (char '#'))
  text <- nonEmptyLine
  return $ Header (length hashes,text)

-- |Parse paragraph
paragraph :: Parser Block
paragraph = do
  --l <- bracket emptyLine nonEmptyLine emptyLine
  l <- some nonEmptyLine
  return . Paragraph $ l

-- |Parse unordered list
-- unorderdList :: Parser Block
-- unorderdList = do
--   items <- some (token bullet >> line)
--   return . UnorderedList $ items
--   where
--     bullet :: Parser Char
--     bullet = char '*' <|> char '+' <|> char '-' >>= return

unorderdList :: Parser Block
unorderdList = do
  items <- many1_offside (token bullet >> line)
  return . UnorderedList $ items
  where
    bullet :: Parser Char
    bullet = char '*' <|> char '+' <|> char '-' >>= return

-- TODO: Эта функция делает почти тоже самое, что и emptyLine,
-- TODO непонятно, как совместить их в одну, или, по крайней мере,
-- TODO избежать дублирования кода
blank :: Parser Block
blank = many (sat wspaceOrTab) >> char '\n' >> return Blank

-- |Parse blockquote
blockquote :: Parser Block
blockquote = do
  lines <- many1_offside (token (char '>') >> line)
  return . BlockQuote $ lines

-- |Черновик для латех-блоков
blockMath :: Parser Block
blockMath = (bracket (string "$$") (some (sat (/= '$'))) (string "$$")) >>=
  return . Paragraph . (: []) . NonEmpty . (: []) . Plain .
    (\x -> "$$" ++ x ++ "$$")

-----------------------------------------------------------------
-------------------Parsers for whole Document--------------------
-----------------------------------------------------------------

-- |Парсит документ и возвращает список блоков
doc :: Parser Document
doc = do
  ls <- first $ many1_offside block
  --a <- header
  --b <- blank
  --c <- paragraph
  return $ ls
  --return [a,b,c]
  where
    block =
      blank <|> header <|> paragraph <|>
      unorderdList <|> blockquote <|> blockMath
