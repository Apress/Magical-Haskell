{-# LANGUAGE OverloadedStrings #-}

module Util.Formatting
where

import CMark
import qualified Data.Text as T

import Skylighting -- Import the Skylighting library
import Skylighting.Format.ANSI (formatANSI) -- Import the ANSI formatting capability
import Skylighting.Core (TokenizerConfig(..), tokenize)
import Skylighting.Syntax (defaultSyntaxMap)
import Skylighting.Types (defaultFormatOpts)
import Skylighting.Styles (kate, breezeDark, espresso)
import Util.PrettyPrinting (lgray, as, bold, white, lgreen, underlined)


-- Get the tokenizer configuration 
tokenizerConfig = TokenizerConfig {
                    syntaxMap = defaultSyntaxMap,
                    traceOutput = False
                }


nodeTreeToText :: Node -> (Node -> T.Text) -> T.Text
nodeTreeToText node nodeToText = T.intercalate "" (traverseNodes node)
  where
    traverseNodes n@(Node _ _ children) = nodeToText n : concatMap traverseNodes children

highlightCode lang code =
    maybe code (\x -> processCode $ tokenize tokenizerConfig x code) (lookupSyntax lang defaultSyntaxMap)
    where
        processCode (Left _) = code
        processCode (Right src) = formatANSI defaultFormatOpts kate src

nodeToText n@(Node _ ntype _) = case ntype of
        HTML_BLOCK txt -> highlightCode "html" txt `T.append` "\n"
        HTML_INLINE txt -> T.pack (as [lgray] (T.unpack txt))
        CODE txt -> T.pack (as [white, bold] (T.unpack txt))
        CODE_BLOCK lang code -> highlightCode lang code `T.append` "\n"
        _        -> let ltx = nodeToCommonmark [optNormalize] Nothing n
                    in highlightCode "Markdown" ltx


nodeToTextE (Node _ ntype _) = case ntype of
        TEXT txt -> txt -- `T.append` "\n"
        HTML_BLOCK txt -> highlightCode "html" txt `T.append` "\n"
        HTML_INLINE txt -> T.pack (as [lgray] (T.unpack txt))
        CODE txt -> T.pack (as [white, bold] (T.unpack txt))
        CODE_BLOCK lang code -> highlightCode lang code `T.append` "\n"
        LINEBREAK -> "\n"
        LINK url title -> T.pack (as [lgreen,bold] (T.unpack title) ++ " (" ++ as [white,underlined] (T.unpack url) ++ ")")
        IMAGE url title -> T.pack (as [lgreen,bold] ("[IMAGE]: " ++ T.unpack title) ++ " (" ++ as [white,underlined] (T.unpack url) ++ ")")
        _        -> "" -- `T.append` "\n"

formatViaLatex msg = do
    -- let ltx = commonmarkToLaTeX [optNormalize] Nothing msg
    -- putStrLn (T.unpack ltx)
    let nds = commonmarkToNode [optNormalize] msg
    putStrLn (show nds)
    putStrLn $ T.unpack $ nodeTreeToText nds nodeToText
{-
formatMessage msg = do
    let nds = commonmarkToNode [optNormalize] msg
    let txt = nodeToText nds
    mapM_ (\x -> putStrLn $ T.unpack x) txt
-}
{-
nodeToText :: Node -> [T.Text]
nodeToText (Node _ DOCUMENT xs) = map nodeToText xs
nodeToText (Node _ (CODE_BLOCK language code) xs) = [code:map nodeToText xs]
nodeToText node = [nodeToLaTeX [optNormalize] Nothing node]
-}