{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Data.List (intersperse)
import Data.Monoid
import Data.Text.Lazy (Text, intercalate, null)
import Data.Text.Lazy.Builder
import Data.Text.Format
import Prelude hiding (null)

import Parser

-- convert the haskellname to the name of the foreign function
toForeignName :: Text -> Text
toForeignName = (<> "_CImpl")

haskellFile :: [Line] -> Builder
haskellFile = mconcat . map haskellLine

haskellLine :: Line -> Builder
haskellLine (PlainLine s) = fromLazyText s <> "\n"

haskellLine (ImportLine _ hsName cConstr hsType) =
  if needsConversion hsType then
    --expression with something that needs conversion
    -- so output to lines, the foreign call with converted types, and the conversion function
    build "foreign import ccall \"{}\" {}_With_CTypes :: {}\n{}{} = {}{}_With_CTypes{}\n"
        (toForeignName hsName, hsName, foreignSignature hsType, hsName, argumentList, resultConversion, hsName, argumentListWithConversion)
  else
    --expression without strings
    build "foreign import ccall \"{}\" {} :: {}\n" (toForeignName hsName, hsName, foreignSignature hsType)
  where
    -- return true if there is a IO or function type in the argument list
    -- return true if there is coversion type with to conversion in argument list
    -- return true of there is converion type with from conversion as result
    needsConversion t = case t of
      (IOType t')        -> needsConversion t'
      (ConvertType dat)  -> if null (fromConvert dat) then False else True
      (FunctionType a r) -> argNeedsConversion a || needsConversion r
      _                  -> False
    argNeedsConversion t = case t of
      (ConvertType dat)  -> if null (toConvert dat) then False else True
      IOVoid             -> True
      (IOType _)         -> True
      (FunctionType _ _) -> True
      _                  -> False
    numArgs = numArgs' hsType
      where
      numArgs' (FunctionType _ r) = 1 + (numArgs' r)
      numArgs' _                  = 0
    argumentList :: Builder
    argumentList = mconcat $ map (build " a{}" . Only) $ ([1..numArgs] :: [Int])
    argumentListWithConversion :: Builder
    -- argumentListWithConversion = foldr (\x y -> x <> " " <> y) "" $ zipWith argumentConversion ([1..] :: [Int]) (argTypes hsType)
    argumentListWithConversion = mconcat $ intersperse " " $ zipWith argumentConversion ([1..] :: [Int]) (argTypes hsType)
      where
      argumentConversion ident t = let name = build "a{}" (show ident) in case t of
        (ConvertType dat)  -> if null (toConvert dat) then
                                 name
                              else
                                 build "({} {})" (toConvert dat, name)
        (FunctionType _ _) -> build "(mkCallback $ {})" (Only name)
        IOVoid             -> build "(mkCallback $ {})" (Only name)
        IOType _           -> build "(mkCallback $ {})" (Only name)
        _                  -> name
    resultConversion = resultConversion' (resultType hsType)
      where
      resultConversion' (ConvertType dat)
          | null (fromConvert dat) = ""
          | otherwise              = build "{} $ " (Only (fromConvert dat))
      resultConversion' _ = ""


    argTypeList :: Type -> Builder
    argTypeList t = mconcat $ map (build "{} -> " . (Only . showArgType)) $ argTypes t
    foreignSignature :: Type -> Builder
    {-cConstraintString classConstr = (className classConstr) ++ concatMap (\p -> ' ':p) (parameters classConstr)
    cConstraintsString
      | cConstr == [] = ""
      | otherwise     = "(" ++ intercalate "," (map cConstraintString cConstr) ++ ") => "-}
    foreignSignature t = {-cConstraintsString ++-} build "{} {}" (argTypeList t, showResType $ resultType t)
    showArgType :: Type -> Builder
    showArgType (ConvertType dat)   = fromLazyText $ foreignTypeName dat
    showArgType IOVoid              = "JSFun (IO ())"
    showArgType (IOType t)          = build "JSFun (IO ({}))" (Only (showArgType t))
    showArgType (PlainType s)       = fromLazyText s
    showArgType (FunctionType f r)  = build "JSFun ({})" (Only (foreignSignature (FunctionType f r)))
    showResType :: Type -> Builder
    showResType (ConvertType dat)   = fromLazyText $ foreignTypeName dat
    showResType IOVoid              = "IO ()"
    showResType (IOType t)          = build "IO ({})" (Only (showResType t))
    showResType (PlainType s)       = fromLazyText s


javascriptFile :: [Line] -> Builder
javascriptFile = mconcat . map javascriptLine

javascriptLine :: Line -> Builder
javascriptLine (PlainLine _) = ""
javascriptLine (ImportLine jsExp hsName cConstr hsType) =
    build "function {} ({}) { {} }" (name, args, body) where
        name = (toForeignName hsName)
        args = (argumentList $ length (argTypes hsType))
        body = if resultType hsType == IOVoid
                   then build "{}; return;" (Only jsCommand)
                   else build "return {};" (Only jsCommand)
        argumentList :: Int -> Text
        argumentList max = intercalate "," $ map (format "a{}" . Only) [1..max]
        jsCommand = mconcat . map showExprPart $ jsExp
        showExprPart :: JSExprPart -> Text
        showExprPart (StringPart s) = s
        showExprPart (ArgumentPart i) = format "a{}" (Only (show i))
        showExprPart (RestArgPart) = intercalate "," . map showExprPart $ restArguments
        restArguments :: [JSExprPart]
        restArguments = let argId (ArgumentPart i) = i
                            argId _ = 0
                            highestArgument = maximum . map (argId) $ jsExp
                            numArguments = length (argTypes hsType)
                            missingArgs = if highestArgument >= numArguments then [] else [(highestArgument+1) .. numArguments]
                        in map (\i -> ArgumentPart i) missingArgs
javascriptLine (ExportLine ModuleExport name hsType) =
    build "module.exports = {};" (Only (toForeignName name))
javascriptLine (ExportLine (NameExport jsName) hsName hsType) =
    build "exports[{}] = {};" (jsName, toForeignName hsName)

-- helper functions
argTypes :: Type -> [Type]
argTypes t = case t of
  FunctionType a r -> a:(argTypes r)
  _                -> []

resultType :: Type -> Type
resultType t = case t of
  FunctionType _ r -> resultType r
  r                -> r
