{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Data.Either
import qualified Data.Foldable                            as F
import           Data.Monoid
import           Network.HTTP                             (getRequest,
                                                           getResponseBody,
                                                           simpleHTTP)
import           System.Environment                       (getArgs)
import           Text.HTML.TagSoup
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Combinator
import           Text.Read                                (readMaybe)


main :: IO ()
main = mapM_ handlePkg =<< getArgs

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

verNum = parse parser "get pkg version #"
    where parser = many1 digit `sepBy1` char '.'

handlePkg :: String -> IO ()
handlePkg pkgName = handleHTML =<< openURL (url pkgName)
  where url pkgName = "http://hackage.haskell.org/package/" <> pkgName
        handleHTML = print . map concat . parseVersions
        parseVersions html = rights [ verNum txt | TagText txt <- parseTags html ]

