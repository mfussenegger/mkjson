{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module ObjectGroups
  ( mergeObjects,
    makeTree,
  )
where

import qualified Cli
import qualified Data.List as L
import qualified Data.Text as T
import Expr (Expr (..), pattern Fn)


data Tree a = Node T.Text [Tree a]
            | Leaf T.Text a
            deriving (Show, Eq)


-- | Convert fields into a tree grouped by the fields path
--
-- >>> makeTree . Cli.fields <$> Cli.parseArgs ["a=1", "b=2"]
-- [Leaf "a" (IntLiteral 1),Leaf "b" (IntLiteral 2)]
--
-- >>> makeTree . Cli.fields <$> Cli.parseArgs ["a.b=1"]
-- [Node "a" [Leaf "b" (IntLiteral 1)]]
--
-- >>> makeTree . Cli.fields <$> Cli.parseArgs ["a.b.c=1"]
-- [Node "a" [Node "b" [Leaf "c" (IntLiteral 1)]]]
--
-- >>> makeTree . Cli.fields <$> Cli.parseArgs ["o.x=1", "o.y=2"]
-- [Node "o" [Leaf "x" (IntLiteral 1),Leaf "y" (IntLiteral 2)]]
makeTree :: [Cli.Field] -> [Tree Expr]
makeTree = foldr step []
  where
    step (fieldName, expr) = mkTree (T.splitOn "." fieldName) expr

    mkTree :: [T.Text] -> Expr -> [Tree Expr] -> [Tree Expr]
    mkTree [] _ _ = error "splitOn must always create a list with at least one entry"
    mkTree [x] expr siblings = Leaf x expr : siblings
    mkTree field@(x : xs) expr siblings =
      case L.find matchingNode siblings of
        Nothing -> Node x (mkTree xs expr []) : siblings
        Just existing@(Node _ children) ->
          let new = mkTree xs expr children
          in Node x new : filter (/= existing) siblings
        Just (Leaf _ existing) -> cannotCombineError existing
      where
        matchingNode (Node nodeName _) = nodeName == x
        matchingNode _ = False
        cannotCombineError existing =
          error $
            unwords
              ["Cannot combine", T.unpack $ T.intercalate "." field, "and", show existing]


-- | Merge fields with object notation by path into object functions
--
-- >>> mergeObjects . Cli.fields <$> Cli.parseArgs ["a=1"]
-- [("a",IntLiteral 1)]
--
-- >>> mergeObjects . Cli.fields <$> Cli.parseArgs ["a.b=1"]
-- [("a",FunctionCall (Function {fcName = "object", fcArgs = [StringLiteral "b",IntLiteral 1]}))]
--
-- >>> mergeObjects . Cli.fields <$> Cli.parseArgs ["a.b.c=1", "a.b.d=2"]
-- [("a",FunctionCall (Function {fcName = "object", fcArgs = [StringLiteral "b",FunctionCall (Function {fcName = "object", fcArgs = [StringLiteral "c",IntLiteral 1,StringLiteral "d",IntLiteral 2]})]}))]
-- 
mergeObjects :: [Cli.Field] -> [Cli.Field]
mergeObjects fields = fmap makeField (makeTree fields)
  where
    makeField :: Tree Expr -> Cli.Field
    makeField (Leaf leafName expr) = (leafName, expr)
    makeField (Node nodeName children ) = (nodeName, Fn "object" (concatMap makeArgs children))

    makeArgs :: Tree Expr -> [Expr]
    makeArgs (Leaf name expr) = [StringLiteral name, expr]
    makeArgs (Node name children) = [StringLiteral name, Fn "object" $ concatMap makeArgs children]
