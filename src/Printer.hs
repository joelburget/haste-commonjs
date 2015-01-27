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
    -- expression with something that needs conversion
    build "foreign import ccall \"{}\" {}_With_CTypes :: {}\n{}{} = {}{}_With_CTypes{}\n"
        -- javascript name of this symbol
        ( toForeignName hsName

        -- the name we're importing to hs
        -- "name_With_CTypes :: ..."
        , hsName
        , foreignSignature hsType

        -- definition of the converted function
        -- "name arg1 arg2 = convert $ name_With_CTypes"
        , hsName
        , argumentList hsType
        , resultConversion hsType
        , hsName
        , argumentListWithConversion hsType
        )
  else
    -- expression without strings
    build "foreign import ccall \"{}\" {} :: {}\n"
        ( toForeignName hsName
        , hsName
        , foreignSignature hsType
        )

haskellLine (ExportLine ModuleExport hsName hsTy) =
    -- "export \"jsModule\" hsName\n"
    "\n"
haskellLine (ExportLine (NameExport jsName) hsName hsTy) =
    "\n"

-- return true if there is a IO or function type in the argument list
-- return true if there is coversion type with to conversion in argument list
-- return true of there is converion type with from conversion as result
needsConversion :: Type -> Bool
needsConversion (IOType t')        = needsConversion t'
needsConversion (ConvertType dat)  = not (null (fromConvert dat))
needsConversion (FunctionType a r) = argNeedsConversion a || needsConversion r
needsConversion _                  = False

argNeedsConversion :: Type -> Bool
argNeedsConversion (ConvertType dat)  = not (null (toConvert dat))
argNeedsConversion IOVoid             = True
argNeedsConversion (IOType _)         = True
argNeedsConversion (FunctionType _ _) = True
argNeedsConversion _                  = False

numArgs :: Type -> Int
numArgs (FunctionType _ r) = 1 + numArgs r
numArgs _                  = 0

argumentList :: Type -> Builder
argumentList ty = mconcat $ map (build " a{}" . Only) [1..numArgs ty]

argumentListWithConversion :: Type -> Builder
argumentListWithConversion ty =
    mconcat $
    intersperse " " $
    zipWith argumentConversion [1..] (argTypes ty)
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

resultConversion :: Type -> Builder
resultConversion ty = resultConversion' (resultType ty) where
  resultConversion' (ConvertType dat)
      | null (fromConvert dat) = ""
      | otherwise              = build "{} $ " (Only (fromConvert dat))
  resultConversion' _ = ""


argTypeList :: Type -> Builder
argTypeList t = mconcat $ map (build "{} -> " . Only . showArgType) $ argTypes t

foreignSignature :: Type -> Builder
foreignSignature t = build "{} {}" (argTypeList t, showResType $ resultType t)

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
    build "function {} ({}) { {} }\n" (name, args, body) where
        name = toForeignName hsName
        args = argumentList $ length (argTypes hsType)
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
                            highestArgument = maximum $ map argId jsExp
                            numArguments = length (argTypes hsType)
                            missingArgs = if highestArgument >= numArguments then [] else [(highestArgument+1) .. numArguments]
                        in map ArgumentPart missingArgs

javascriptLine (ExportLine ModuleExport name hsType) =
    build "module.exports = {};\n" (Only (toForeignName name))

javascriptLine (ExportLine (NameExport jsName) hsName hsType) =
    build "exports[{}] = {};\n" (jsName, toForeignName hsName)

-- helper functions
argTypes :: Type -> [Type]
argTypes t = case t of
  FunctionType a r -> a:argTypes r
  _                -> []

resultType :: Type -> Type
resultType t = case t of
  FunctionType _ r -> resultType r
  r                -> r
