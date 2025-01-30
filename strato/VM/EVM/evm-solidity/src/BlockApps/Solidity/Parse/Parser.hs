{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Parser
-- Description: The Solidity source parser function
-- Maintainer: Ryan Reich <ryan@blockapps.net>
module BlockApps.Solidity.Parse.Parser where

import BlockApps.Solidity.Parse.File
import BlockApps.Solidity.Parse.ParserTypes
import BlockApps.Solidity.Xabi (Xabi (..), xabiTypes)
import BlockApps.Solidity.Xabi.Def
import BlockApps.Solidity.Xabi.Type (VarType (..))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec hiding (parse)

-- | The 'parseSolidity' function parses a single Solidity source string (with
-- a given name) into a Haskell data type.  Parser errors are generated by
-- 'Parsec'.
parseXabi :: FileName -> String -> Either String (SolcVersion, [(Text, Xabi)])
parseXabi filename input = do
  fi@(File units) <- showError $ runParser solidityFile "" filename input
  let xabis = [(name, pair) | NamedXabi name pair <- units]
  let inheritanceFullXabis = map (fmap $ addInheritedDeclarations inheritanceFullXabis) xabis

  xabis' <- traverse sequence inheritanceFullXabis
  let withNames = map (fmap $ addContractNames $ map (Text.unpack . fst) xabis') xabis'
  return $! (decideVersion fi, withNames)

parseXabiNoInheritanceMerge :: FileName -> String -> Either String File
parseXabiNoInheritanceMerge filename input = do
  showError $ runParser solidityFile "" filename input

addContractNames :: [String] -> Xabi -> Xabi
addContractNames contracts xabi =
  xabi {xabiTypes = xabiTypes xabi `Map.union` Map.fromList (map ((\x -> (x, Contract 0)) . Text.pack) contracts)}

addInheritedDeclarations :: [(Text, Either String Xabi)] -> (Xabi, [Text]) -> Either String Xabi
addInheritedDeclarations _ (xabi, []) = Right xabi
addInheritedDeclarations xabisWithInheritedDeclarations (xabi, parent : rest) = do
  parentXabiOrError <-
    lookup parent xabisWithInheritedDeclarations
      `orError` ("Contract was inherited from a non existent contract: " ++ Text.unpack parent)
  parentXabi <- parentXabiOrError
  !mergedXabis <- addInheritedDeclarations xabisWithInheritedDeclarations (xabi, rest)
  return (xabiMerge mergedXabis parentXabi)

xabiMerge :: Xabi -> Xabi -> Xabi
xabiMerge x y =
  Xabi
    { xabiFuncs = xabiFuncs x `Map.union` xabiFuncs y,
      xabiConstr = xabiConstr x,
      xabiVars = fmap (bumpAtBytes bumper) (xabiVars x) `Map.union` xabiVars y,
      xabiTypes = xabiTypes x `Map.union` xabiTypes y,
      xabiModifiers = xabiModifiers x `Map.union` xabiModifiers y,
      xabiEvents = xabiEvents x `Map.union` xabiEvents y,
      -- This doesn't make any sense, I broke the type ¯\_(ツ)_/¯
      -- Please don't try to merge libraries or interfaces
      xabiKind = xabiKind x,
      xabiUsing = xabiUsing x `Map.union` xabiUsing y
    }
  where
    bumper = if null (variables $ xabiVars y) then 0 else maximum (fmap varTypeAtBytes (xabiVars y)) + 32
    bumpAtBytes n varType =
      varType {varTypeAtBytes = varTypeAtBytes varType + n}
    variables = Map.filter (maybe True not . varTypeConstant)

------------
-- Some error conversion functions

showError :: Show a => Either a b -> Either String b
showError (Left e) = Left $ show e
showError (Right x) = Right x

orError :: Maybe a -> String -> Either String a
orError Nothing msg = Left msg
orError (Just x) _ = Right x
