{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad ((<=<), mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Monoid
import Data.Functor
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Text.Lazy hiding (unwords, map, concat, foldl')
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.IO (writeFile)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import System.Environment
import System.Console.GetOpt
import System.IO.Error
import System.Process
import Text.Parsec hiding (Line)

import Prelude hiding (writeFile)

import Parser
import Printer

data JsonStuff = JsonStuff
    { conversions :: [ConvertData]
    , stubs :: [Text]
    , inFile :: Text
    , out :: Text
    }

anObject :: Value -> Maybe Object
anObject (Object m) = Just m
anObject _          = Nothing

anArray :: Value -> Maybe Array
anArray (Array a) = Just a
anArray _         = Nothing

aString :: Value -> Maybe Text
aString (String t) = Just (fromStrict t)
aString _          = Nothing

aConvertData :: Value -> Maybe ConvertData
aConvertData (Object m) = do
    a <- (aString <=< H.lookup "hsTy") m
    b <- (aString <=< H.lookup "jsTy") m
    c <- (aString <=< H.lookup "hsToJs") m
    d <- (aString <=< H.lookup "jsToHs") m
    return $ ConvertData a b c d

instance FromJSON JsonStuff where
    parseJSON (Object package) =
        let maybeThing = do
                options <- H.lookup "haste-options" package
                options' <- anObject options

                inFile <- (aString <=< H.lookup "in") options'
                out <- aString (H.lookupDefault (String "out.js") "out" options')

                stubs <- H.lookup "stubs" options'
                stubs' <- anArray stubs
                stubs'' <- sequence $ V.toList $ V.map aString stubs'

                conversions <- H.lookup "conversions" options'
                conversions' <- anArray conversions
                conversions'' <- mapM aConvertData $ V.toList conversions'

                return $ JsonStuff conversions'' stubs'' inFile out

        in case maybeThing of
               Just something -> return something
               Nothing -> mzero
    parseJSON _ = mzero

writeFilesOut :: String -> String -> [Line] -> IO ()
writeFilesOut hsFile jsFile lines = do
    let hsData = haskellFile lines
        jsData = javascriptFile lines
    writeFile hsFile $ toLazyText hsData
    writeFile jsFile $ toLazyText jsData

main :: IO ()
main = do
    args <- getArgs

    jsonBS <- BS.readFile "package.json" `catch`
        (\e -> error $ if isDoesNotExistError e
            then "Must be run in a directory with package.json"
            else show e)

    let jsonStuff :: Maybe JsonStuff
        jsonStuff = decode' jsonBS

    let jsonStuff' = case jsonStuff of
            Nothing -> error "Couldn't parse package.json"
            Just jsonStuff'' -> jsonStuff''
        outFile = unpack $ out jsonStuff'
        inFile' = unpack $ inFile jsonStuff'

    contents <- T.readFile inFile'
    let lines = runParser parseFFIFile (conversions jsonStuff') inFile' contents

    putStrLn "writing intermediate files"
    writeFilesOut "__preprocessed.hs" "__stubs.js" $ case lines of
        Left msg -> error $ show msg
        Right lines' -> lines'

    -- hastec preprocessed.hs --with-js=Mainstub.js --with-js=hs-stubs.js --debug
    putStrLn "calling hastec"
    let stubLines = map ("--with-js=" ++) $
            "__stubs.js":(map unpack (stubs jsonStuff'))
    callProcess "hastec"
        (["__preprocessed.hs"] ++ args ++ stubLines)

    -- ./node_modules/.bin/browserify preprocessed.js -o mui.js
    putStrLn "calling browserify"
    callProcess "./node_modules/.bin/browserify"
        ["__preprocessed.js", "-o", outFile]

    -- sed -i '' -e's/var window = {}window;//' mui.js
    callProcess "sed" ["-i", "''", "-e", "s/var window = {};//", outFile]
