module TestMDParser where
import MDParse

-- tests punctuation
test_punctuation1 = parse (first $ many punctuation) ",,,;" == [(",,,;",((1,4),""))]
test_punctuation2 = parse (many punctuation) "ab," == [("",((1,0),"ab,"))]
----
-- tests alphanumsWithPunctuation
test_alpha_punct1 = parse (first $ many alphanumsWithPunctuation ) "ab," == [(["ab,"],((1,3),""))]
test_alpha_punct2 = parse (first $ many alphanumsWithPunctuation ) "12ab c." == [(["12ab"],((1,4)," c."))]
----
-- tests sentence
test_sentence1 = parse (first $ sentence) "Hello! 123  :yes" == [("Hello! 123  :yes",((1,16),""))]
test_sentence2 = parse (first $ sentence) "" == []
----
-- tests inline
test_plain1 = parse (first $ plain) "Yello!" == [(Plain "Yello!",((1,6),""))]
test_italic1 = parse (first $ italic) "Yello!" == []
test_italic2 = parse (first $ italic) "*Yello!*" == [(Italic "Yello!",((1,8),""))]

test_bold1 = parse (first $ bold) "**Yello!**" == [(Bold "Yello!",((1,10),""))]

test_monospace1 = parse (first $ monospace ) "`Yello!`" == [(Monospace "Yello!",((1,8),""))]
----
-- tests lines
test_nonEmpty = arse (first nonEmptyLine)  "aaa    \n" == [(NonEmpty [Plain "aaa    "],((2,0),""))]
test_Empty = parse (first emptyLine)  "    \n" == [(Empty,((2,0),""))]
test_line = parse line  "aaa    \n" == [(NonEmpty [Plain "aaa    "],((2,0),""))]
----
------ Block --------
-- tests
test_header = parse (first header)  "#lalala    \n" == [(Header (1,NonEmpty [Plain "lalala    "]),((2,0),""))]

test_paragraph = parse (first paragraph )  "lalala    \n" == [(Paragraph [NonEmpty [Plain "lalala    "]],((2,0),""))]
----
-- tests list
test_unordered_list = parse (first $ many unorderdList)  "+lalala \n *lololo \n" == [([UnorderedList [NonEmpty [Plain "lalala "]]],((2,0)," *lololo \n"))]
----
-- tests blank
test_blank = parse blank   "   \n" == [(Blank,((2,0),""))]
----
-- tests BlockQuote
test_blockQuote = parse (first $ many blockquote)  ">lalala   \n > lll\n" == [([BlockQuote [NonEmpty [Plain "lalala   "]]],((2,0)," > lll\n"))]

test_math = parse (first $ blockMath )  "$$ lalala $$" == [(Paragraph [NonEmpty [Plain "$$ lalala $$"]],((1,12),""))]
----
-- tests
----
