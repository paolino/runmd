{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude

import CMark (Node (..), NodeType (CODE_BLOCK), commonmarkToNode)
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Char (ord)
import Data.Csv.Parser qualified as Csv
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import Data.Yaml (FromJSON)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Options.Applicative qualified as Opt

-- | InfoString is a representation of the info string of a code block
data InfoString = InfoString
    { interpreter :: Text
    -- ^ The first word of the info string
    , keys :: [Text]
    -- ^ The rest of the info string, comma separated values
    }
    deriving (Show)

-- | Parse a comma separated list of keys, we assume CSV format
-- so spaced are allowed. We also trim the keys.
parseKeys :: P.Parser [Text]
parseKeys =
    Csv.record coma <&> fmap (T.strip . decodeUtf8) . V.toList
  where
    coma = fromIntegral (ord ',')

-- | Parse an info string
parseInfoString :: P.Parser InfoString
parseInfoString = do
    interpreter <- P.takeWhile1 (/= ' ') <&> decodeUtf8
    keys <- parseKeys
    return InfoString{..}

-- | 'CodeBlock' is a representation of a code block
data CodeBlock = CodeBlock
    { info :: Maybe InfoString
    -- ^ The info string of the code block if valid
    , code :: Text
    -- ^ The code block content
    }
    deriving (Show)

-- | Try to create a 'CodeBlock' from a 'NodeType'
mkCodeBlock :: NodeType -> Maybe CodeBlock
mkCodeBlock (CODE_BLOCK i t) = Just $ CodeBlock i' t
  where
    i' = case parseOnly parseInfoString (encodeUtf8 i) of
        Left _ -> Nothing
        Right x -> Just x
mkCodeBlock _ = Nothing

-- | Try to create a 'CodeBlock' from a 'Node'
onCodeBlockType :: Node -> Maybe CodeBlock
onCodeBlockType (Node _ t _) = mkCodeBlock t

-- | Depth based extraction
--
-- If the function returns Nothing, the node is ignored
-- If the function returns Just, the node value is added to the result
extract :: (Node -> Maybe a) -> Node -> [a]
extract f node =
    (children node >>= extract f)
        & maybe id (:) (f node)
  where
    children :: Node -> [Node]
    children (Node _ _ nodes) = nodes

-- | 'OneFile' is a representation of a file to be processed in a recipe
data OneFile = OneFile
    { path :: FilePath
    -- ^ Relative path to the markdown file
    , matches :: [Text]
    -- ^ List of keys to match in the code blocks. Order matters.
    }
    deriving (Show, Generic)

instance FromJSON OneFile

-- | 'Recipe' is a list of files to be processed
newtype Recipe = Recipe [OneFile]
    deriving (Show, Generic)

instance FromJSON Recipe

-- | Load a recipe as a YAML file
loadRecipe :: FilePath -> IO Recipe
loadRecipe path = do
    content <- readFile path
    case Yaml.decodeEither' (encodeUtf8 $ pack content) of
        Left err -> error $ show err
        Right x -> return x

-- | 'Options' is a representation of the command line options
data Options = Options
    { recipeFile :: FilePath
    -- ^ Recipe file path
    , baseDir :: FilePath
    -- ^ Base directory for the markdown files
    , enableLogging :: Bool
    -- ^ If to interleave logging messages between code blocks
    , enableEchoing :: Bool
    -- ^ If to enable echoing of the commands in the output
    }
    deriving (Show)

-- | Parse the command line options
parseOptions :: Opt.Parser Options
parseOptions =
    Options
        <$> Opt.strOption
            ( Opt.long "recipe"
                <> Opt.short 'r'
                <> Opt.metavar "FILE"
                <> Opt.help "Recipe file"
            )
        <*> Opt.strOption
            ( Opt.long "base-dir"
                <> Opt.short 'd'
                <> Opt.metavar "DIR"
                <> Opt.help "Base directory"
            )
        <*> Opt.switch
            ( Opt.long "enable-logging"
                <> Opt.short 'l'
                <> Opt.help "Enable logging"
            )
        <*> Opt.switch
            ( Opt.long "enable-echoing"
                <> Opt.short 'e'
                <> Opt.help "Enable echoing"
            )

-- | Generate the bash bailout on error setup string
withEchoing :: Bool -> String
withEchoing True = "set -euox pipefail"
withEchoing False = "set -euo pipefail"

-- | Generate the bash preamble
preamble :: Options -> IO ()
preamble Options{..} = do
    putStrLn "#!/usr/bin/env bash"
    putStrLn $ withEchoing enableEchoing
    putStrLn "\n"

main :: IO ()
main = do
    options@Options{..} <-
        Opt.execParser $ Opt.info parseOptions Opt.fullDesc
    Recipe files <- loadRecipe recipeFile
    preamble options
    forM_ files $ \OneFile{..} -> do
        content <- readFile (baseDir <> "/" <> path)
        let node = commonmarkToNode [] (pack content)
        forM_ matches $ \m -> do
            forM_ (extract onCodeBlockType node) $ \CodeBlock{..} -> do
                when (m `elem` maybe [] keys info) $ do
                    when enableLogging
                        $ putStrLn
                        $ "echo 'Running "
                            <> path
                            <> "' "
                            <> show m
                    putStrLn $ T.unpack code
