{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Dianoga.Analysis.Html
  ( HtmlDependencies
  , emptyHtmlDependencies
  , imageFiles
  , cssFiles
  , jsFiles
  , inlineStyles
  , inlineScripts
  , ScrapeState
  , startingScrapeState
  , depth
  , innerTags
  , openTags
  , state
  , html5VoidElements
  , isVoidElementName
  , tagHasNoCloseTag
  , dependenciesAccumulator
  , accumulateDependencies ) where

import Control.Lens ((&), (?~), (.~), (%~), (^.), (%=), use, view, set, over)
import Control.Foldl hiding (set)
import Control.Lens.TH
import Control.Monad (unless)
import Control.Monad.State (State, execState)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import Text.HTML.TagSoup
import Text.StringLike (StringLike)
import qualified Data.HashSet as S

data HtmlDependencies
  = HtmlDependencies
    { htmlDependenciesImageFiles    :: ![Text]
    , htmlDependenciesCssFiles      :: ![Text]
    , htmlDependenciesJsFiles       :: ![Text]
    , htmlDependenciesInlineStyles  :: ![[Tag Text]]
    , htmlDependenciesInlineScripts :: ![[Tag Text]]
    } deriving (Show, Eq)

emptyHtmlDependencies :: HtmlDependencies
emptyHtmlDependencies = HtmlDependencies [] [] [] [] []

data ScrapeState a = ScrapeState
  { scrapeStateDepth     :: !Int
  , scrapeStateInnerTags :: ![[Tag Text]]
  , scrapeStateOpenTags  :: ![Tag Text]
  , scrapeStateState     :: !a
  } deriving (Show, Eq)

startingScrapeState :: a -> ScrapeState a
startingScrapeState = ScrapeState 0 [] []

exec = flip execState

makeFields ''HtmlDependencies
makeFields ''ScrapeState

html5VoidElements :: S.HashSet (CI.CI Text)
html5VoidElements = S.fromList
  [ "area"
  , "base"
  , "br"
  , "col"
  , "embed"
  , "hr"
  , "img"
  , "input"
  , "keygen"
  , "link"
  , "meta"
  , "param"
  , "source"
  , "track"
  , "wbr"
  ]

safeTail :: [a] -> [a]
safeTail (x:xs) = xs
safeTail [] = []

newContext :: a -> [[a]] -> [[a]]
newContext x stacks = [] : addToContext x stacks

addToContext :: a -> [[a]] -> [[a]]
addToContext x = map (x:)


isVoidElementName :: Text -> Bool
isVoidElementName = (`S.member` html5VoidElements) . CI.mk

tagHasNoCloseTag (TagOpen str _) = isVoidElementName str || (CI.mk str == "!doctype")
tagHasNoCloseTag _ = False

dependenciesAccumulator :: Fold (Tag Text) (ScrapeState HtmlDependencies)
dependenciesAccumulator = Fold handleTag startState finalize
  where
    finalize :: ScrapeState HtmlDependencies -> ScrapeState HtmlDependencies
    finalize st = st & state %~ (\deps -> deps & imageFiles %~ reverse
                                               & cssFiles %~ reverse
                                               & jsFiles %~ reverse
                                               & inlineStyles %~ (map reverse . reverse)
                                               & inlineScripts %~ (map reverse . reverse))
                     & innerTags %~ reverse
                     & openTags %~ reverse

    startState :: ScrapeState HtmlDependencies
    startState = ScrapeState 0 [] [] $ HtmlDependencies [] [] [] [] []

    handleTag :: ScrapeState HtmlDependencies -> Tag Text -> ScrapeState HtmlDependencies
    handleTag st tag = exec st $ case tag of
      TagOpen name attrs -> do
        case name of
          "img" -> state.imageFiles %= (\srcs -> maybe srcs (: srcs) $ lookup "src" attrs)

          "link" -> state.cssFiles %= (\srcs -> maybe srcs (: srcs) $ case lookup "rel" attrs of
            -- Default link type is equivalent to setting rel=stylesheet
            Nothing -> lookup "href" attrs
            Just "stylesheet" -> lookup "href" attrs
            _ -> Nothing)

          "script" -> state.jsFiles %= (\srcs -> maybe srcs (: srcs) $ lookup "src" attrs)

          _ -> return ()

        if (tagHasNoCloseTag tag)
          then innerTags %= addToContext tag
          else do
            depth %= succ
            innerTags %= newContext tag
            openTags %= (tag :)

      TagClose str -> do
        ts <- use openTags 

        case ts of
          (TagOpen name _):_ -> case str of
            "style" -> case view innerTags st of
              (ts:_) -> state . inlineStyles %= (ts :)
              _ -> return ()

            "script" -> case view innerTags st of
              (ts:_) -> unless (Prelude.null ts) $ state . inlineScripts %= (ts :)
              _ -> return ()
            _ -> return ()
          _ -> return ()

        depth %= pred
        innerTags %= safeTail
        openTags %= safeTail

      _ -> innerTags %= addToContext tag

accumulateDependencies :: [Tag Text] -> ScrapeState HtmlDependencies
accumulateDependencies = fold dependenciesAccumulator

