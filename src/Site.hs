{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, sortOn)
import Data.Traversable (for)
import System.Directory (getFileSize)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Microstache as M

import Development.Shake
--import Development.Shake.Command
import Development.Shake.FilePath
--import Development.Shake.Util

-------------------------------------------------------------------------------
-- Static config
-------------------------------------------------------------------------------

gpgKey :: String
gpgKey = "5AC8 9B37 47FF 9612 810F  909E EB79 05A7 B8BB 0BA4"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["site/index.html"]

    "site/index.html" %> \out -> do
        need ["index.tmpl.html", "site/files/SHA256SUMS", "site/files/SHA256SUMS.sig"]

        files <- getDirectoryFiles "artifacts" ["*"]
        need [ "site/files/" ++ f ++ ".xz" | f <- files ]
        need [ "site/files/" ++ f ++ ".xz.sig" | f <- files ]

        files' <- for files $ liftIO . \f -> do
            let f' = f ++ ".xz"
            size <- getFileSize ("site/files/" ++ f')
            pure File
                { haName = f ++ ".xz"
                , haType = fileType f
                , haSize = size
                }

        liftIO $ indexHtml out files'

    "site/files/*.xz" %> \out -> do
        let src = "artifacts" </> takeFileName out -<.> ""
        need [src]

        Stdout xz <- cmd ["xz", "--compress", "--keep", "--stdout", src]
        liftIO $ LBS.writeFile out xz

    "site/files/*.sig" %> \out -> do
        let src = out -<.> ""
        need [src]

        cmd ["gpg2", "-u", gpgKey, "--output", out, "--detach-sig", src]

    "site/files/SHA256SUMS" %> \out -> do
        files <- getDirectoryFiles "artifacts" ["*"]
        let xzs = [ "site/files/" ++ f ++ ".xz" | f <- files ]
        let sigs = [ "site/files/" ++ f ++ ".xz.sig" | f <- files ]
        need (xzs ++ sigs)

        Stdout sums <- cmd ("sha256sum" : xzs ++ sigs)
        liftIO $ LBS.writeFile out sums

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

putStrLnWarn :: String  -> IO ()
putStrLnWarn s = hPutStrLn stderr ("WARN: " ++ s)

-------------------------------------------------------------------------------
-- template
-------------------------------------------------------------------------------

indexHtml :: FilePath ->  [File] -> IO ()
indexHtml out files = do
    let files' = sortOn haName files

    -- template arguments
    let args = object
            [ "cabal" .= filter ((Cabal ==) . haType) files'
            ]

    -- template
    template <- M.compileMustacheFile "index.tmpl.html"
    let (ws, output) = M.renderMustacheW template args
    traverse_ (putStrLnWarn . M.displayMustacheWarning) ws

    -- output
    LT.writeFile out output

-------------------------------------------------------------------------------
-- Files
-------------------------------------------------------------------------------

data FileType
    = Cabal
    | Other
  deriving Eq

fileType :: FilePath -> FileType
fileType f
    | "cabal" `isPrefixOf` f = Cabal
    | otherwise              = Other

data File = File
    { haName     :: String
    , haType     :: FileType
    , haSize     :: Integer
    }

instance ToJSON FileType where
    toJSON Cabal     = "cabal"
    toJSON Other     = "other"

instance ToJSON File where
    toJSON File {..} = object
        [ "name"     .= haName
        , "type"     .= haType
        , "size"     .= humanSize (fromIntegral haSize)
        ]

humanSize :: Rational -> String
humanSize = go ["", " kB", " MB", " GB"] where
    go (pfx : _) n | n < 10   = printf "%.3f" (fromRational n :: Double) ++ pfx
    go (pfx : _) n | n < 100  = printf "%.2f" (fromRational n :: Double) ++ pfx
    go (pfx : _) n | n < 1000 = printf "%.1f" (fromRational n :: Double) ++ pfx
    go (_ : pfxs) n = go pfxs (n / 1000)
    go [] n = show n ++ "?"
