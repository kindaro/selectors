{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Either
import           Data.List                (sort)
import           Data.List.Utils
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text
import qualified Data.Text.IO
import           Data.Text.Lazy.Encoding
import           GHC.IO.Exception         (IOErrorType (InappropriateType), IOException (..))
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Text.Blaze               (toMarkup)
import           Text.Blaze.Renderer.Text (renderMarkup)
import           Text.XML
import           Text.XML.Cursor

import           Text.XML.Selectors.CSS   (parsePath, toAxis)

data CaseFiles = CaseFiles
    { pathToDirectory ::  FilePath
    , pathToSelector  ::  FilePath
    , pathToOriginal  ::  FilePath
    , pathsToCuts     :: [FilePath]
    } deriving Show

data Case = Case
    { caseName     :: String
    , caseSelector :: String
    , caseOriginal :: Document
    , caseCuts     :: [FilePath]
    } deriving Show

makeName = replace "_" " " . takeFileName . dropTrailingPathSeparator . dropExtension'
  where dropExtension' s = if last s == '.' then s else dropExtension s

main = do
    cases <- discoverCases
    defaultMain (testGroup "Selector cases." $ makeTestCase <$> cases)

discoverCases :: IO [Case]
discoverCases = do
    listing <- getListing
    traverse caseFilesToCase $ do
        (dir, files) <- listing
        return . either error id $ do
        -- Wouldn't it be best to rewire this with Writer, so as to include all errors rather than
        -- just the first?
            s <- listToEither ("No selector found in test case " ++ pathToCases </> dir)
                 . filter (hasExtension "selector") $ files
            o <- listToEither ("No original found in test case " ++ pathToCases </> dir)
                 . filter (hasAnyExtension ["xml", "html"]) $ files
            c <- let x = filter (hasExtension "cut") files in
                 if null x
                    then Left ("No cuts found in test case " ++ pathToCases </> dir)
                    else Right x
            return CaseFiles
                { pathToDirectory = pathToCases </> dir
                , pathToSelector  = pathToCases </> dir </> s
                , pathToOriginal  = pathToCases </> dir </> o
                , pathsToCuts     = combine (pathToCases </> dir) <$> c
                }

  where
    pathToCases :: FilePath
    pathToCases = takeDirectory __FILE__ </> "cases"

    tryListDirectory :: FilePath -> IO (Either IOError [FilePath])
    tryListDirectory path = tryJust maybeUnableToListError (listDirectory path)

    maybeUnableToListError :: IOError -> Maybe IOError
    maybeUnableToListError e | isDoesNotExistError e || isInappropriateType e = Just e
                   | otherwise = Nothing
      where
        isInappropriateType x = case ioe_type x of
            InappropriateType -> True
            _                 -> False

    getListing :: IO [(FilePath, [FilePath])]
    getListing = listDirectory pathToCases >>= -- TODO: This is kinda poor.
        fmap (sort . rights) <$> traverse (\x -> fmap (x,) <$> tryListDirectory (pathToCases </> x))

    hasExtension :: String -> FilePath -> Bool
    hasExtension ext file = takeExtension file == '.' : ext

    hasAnyExtension :: [String] -> FilePath -> Bool
    hasAnyExtension exts file = or (($ file) . hasExtension <$> exts)

    listToEither :: e -> [a] -> Either e a
    listToEither e = maybe (Left e) Right . listToMaybe

    caseFilesToCase :: CaseFiles -> IO Case
    caseFilesToCase CaseFiles{..} = do
        let caseName = makeName pathToDirectory
            caseCuts = pathsToCuts
        caseSelector <- Prelude.readFile pathToSelector
        caseOriginal <- Text.XML.readFile def pathToOriginal
        return Case { caseName, caseSelector, caseOriginal, caseCuts }

makeTestCase :: Case -> TestTree
makeTestCase Case{..} = testGroup caseName
    $ testCase "Selector is parsed alright" (void axis)
    : testCase "Number of cuts is just right" (length actualCuts @?= length caseCuts)
    : [ goldenVsString (makeName p) p (return c) | p <- caseCuts | c <- actualCuts ]

  where
    axis :: Monad m => m Axis
    axis = case toAxis <$> parsePath caseSelector of
        Left  e -> fail e
        Right a -> return a

    actualCuts :: [Lazy.ByteString]
    actualCuts = axis >>= \axis -> fromDocument >>> axis >>> map renderCursor $ caseOriginal

    renderCursor = node >>> toMarkup >>> renderMarkup >>> encodeUtf8

-- mkTest :: FilePath -> IO Test
-- mkTest path = do

-- mkTests :: IO TestSuite
-- mkTests f = do
--     dirs = ls 'tests'
--     map mkTest dir

-- specs = return () -- Here will go TemplateHaskell testing.
