{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.Either
import           Data.List          (sort)
import           Data.Maybe
import           GHC.IO.Exception   (IOException (..), IOErrorType (InappropriateType))
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Error
import qualified Data.Text
import qualified Data.Text.IO
import           Test.HUnit
import           Text.Blaze               (toMarkup)
import           Text.Blaze.Renderer.Text (renderMarkup)
import           Text.XML
import           Text.XML.Cursor

import Text.XML.Selectors.CSS (parsePath, toAxis)

data CaseFiles = CaseFiles
    { pathToDirectory ::  FilePath
    , pathToSelector  ::  FilePath
    , pathToOriginal  ::  FilePath
    , pathToCuts      :: [FilePath]
    } deriving Show

data Case = Case
    { caseName     ::  Data.Text.Text
    , caseSelector ::  Data.Text.Text
    , caseOriginal ::  Data.Text.Text
    , caseCuts     :: [Data.Text.Text]
    } deriving Show

main = do
    specs
    discoverCases >>= print

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
                , pathToCuts      = combine (pathToCases </> dir) <$> c
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
    caseFilesToCase x = do
        let n = Data.Text.replace "_" " " . Data.Text.pack . takeFileName
                . dropTrailingPathSeparator . pathToDirectory $ x
        s <- Data.Text.IO.readFile . pathToSelector $ x
        o <- Data.Text.IO.readFile . pathToOriginal $ x
        c <- traverse Data.Text.IO.readFile . sort . pathToCuts $ x
        return $ Case n s o c

-- mkTest :: FilePath -> IO Test
-- mkTest path = do

-- mkTests :: IO TestSuite
-- mkTests f = do
--     dirs = ls 'tests'
--     map mkTest dir

specs = return () -- Here will go TemplateHaskell testing.
