{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
  (
   -- | Basic types
   BentoColor
  ,BarIdent
  ,
   -- | Setting ensembles
   -- | (cough)
   ColorSetting
  ,FontSetting
  ,
   -- | The main command-related types
   BarProperty
  ,Command
  ,Payload
  ,
   -- | "Initializer" payloads, and smart constructors thereof.
   mkDefaultSetup
  ,defaultSetup
  ,writePretty
  ,
   -- | For now
   pretty')
  where

import           Data.Aeson                 (ToJSON (..), (.!=), (.:), (.=))
import qualified Data.Aeson.Encode.Pretty   as A (encodePretty)
import           Data.Aeson.Types           (object)
import qualified Data.Aeson.Types           as AT

import qualified Data.ByteString.Lazy       as BS (ByteString (..), writeFile)
import qualified Data.ByteString.Lazy.Char8 as BS (pack, unpack)
import           Data.Monoid                ((<>))

import           Data.Colour                (Colour (..))
import qualified Data.Colour.Names          as Colors
import           Data.Colour.SRGB           (RGB (..))
import qualified Data.Colour.SRGB           as C (toSRGB24)

import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack, toLower)

import           Control.Lens               hiding ((.=))
import           GHC.Exts                   (fromList)
import           GHC.Word                   (Word8)

type BentoColor = Colour Double

instance ToJSON BentoColor where
  toJSON c =
    let (RGB r g b) = C.toSRGB24 c
    in object ["red" .= r,"green" .= g,"blue" .= b]

data VisibilityState
  = Visible
  | Hidden
  deriving (Show,Eq)

data FontFace
  = Normal
  | Bold
  | Italic
  | BoldItalic
  deriving (Show,Eq)

instance ToJSON FontFace where
  toJSON face =
    AT.String $
    case face of
      Normal     -> "normal"
      Bold       -> "bold"
      Italic     -> "italic"
      BoldItalic -> "bold,italic"

data FontType
  =
    -- | Should be self-explanatory.
    NormalFont
  |
    -- | This one too.
    BoldFont
  |
    -- | "Numbered fonts". I don't think I'll need these,
    -- | but they might end up being useful for some kind
    -- | of indexed for-loop thing.
    IndexedFont Int
  |
    -- | Fonts with text identifiers, e.g. one might have
    -- | "mymonofont" and so on. This is to be a sort of
    -- | incubator for new font types that will later be
    -- | merged into this ADT if found significantly useful.
    NamedFont Text
  deriving (Show,Eq)

data Font =
  Font {_name :: Text
       ,_size :: Int
       ,_face :: FontFace}
  deriving (Show,Eq)

-- makeLenses ''Font
instance ToJSON Font where
  toJSON Font{..} =
            object ["name" .= _name,"size" .= _size,"face" .= toJSON _face]

data FontSetting =
  FontSetting {_fontType :: FontType
              ,_font     :: Font}
  deriving (Show,Eq)

instance ToJSON FontSetting where
  toJSON FontSetting{..} =
    object $
    ["data" .= toJSON _font,"type" .= AT.String _fontType'] ++ extraPairs
    where (_fontType',extraPairs) =
            case _fontType of
              (IndexedFont ix) -> ("indexed",["index" .= ix])
              (NamedFont ft)   -> ("named",["ident" .= ft])
              BoldFont         -> ("bold",[])
              NormalFont       -> ("normal",[])

data ColorType
  =
    -- | Background color
    Background
  |
    -- | Foreground color
    Foreground
  | Success
  | Warning
  | Failure
  | OtherColor Text
  deriving (Show,Eq)

lowerName :: (Show a)
          => a -> Text
lowerName = T.toLower . T.pack . show

instance ToJSON ColorType where
  toJSON = toJSON . lowerName

data ColorSetting =
  ColorSetting {_colorType :: ColorType
               ,_color     :: BentoColor}
  deriving (Show,Eq)

instance ToJSON ColorSetting where
  toJSON ColorSetting{..} = object ["name" .= lowerName _colorType
                                   ,"data" .= _color]

data BarProperty
  =
    -- | The text the bar displays.
    Contents Text
  |
    -- | A color
    ColorSettings [ColorSetting]
  |
    -- | A Pango font description
    FontSettings [FontSetting]
  |
    -- | Whether or not the bar should be mapped
    Visibility VisibilityState
  deriving (Show,Eq)

mkColorSettings :: [(ColorType, BentoColor)] -> BarProperty
mkColorSettings = ColorSettings . map (uncurry ColorSetting)

mkFontSettings :: [(FontType, Font)] -> BarProperty
mkFontSettings = FontSettings . map (uncurry FontSetting)

instance ToJSON BarProperty where
  toJSON (Contents t) = wrappedProperty "contents" t
  toJSON (Visibility s) =
    wrappedProperty "hidden"
                    (s == Hidden)
  toJSON (ColorSettings xs) = wrappedProperty "colors" (map toJSON xs)
  toJSON (FontSettings xs) = wrappedProperty "fonts" (map toJSON xs)

data Command
  =
    -- | Set some property of the bar.
    SetProp BarProperty
  |
    -- | Force a repaint
    Repaint
  |
    -- | Create ...
    CreateBar
  |
    -- | ... and destroy bars.
    DestroyBar
  deriving (Show,Eq)

instance ToJSON Command where
  toJSON (SetProp b) = toJSON b
  toJSON cmd         = wrappedCommand $ lowerName cmd

wrappedCommand :: Text -> AT.Value
wrappedCommand ident = object ["type" .= cmdtype,"name" .= ident]
  where cmdtype :: Text
        cmdtype = "command"

wrappedProperty :: (ToJSON b)
                => Text -> b -> AT.Value
wrappedProperty ident val =
  object ["type" .= cmdtype,"name" .= ident,"data" .= val]
  where cmdtype :: Text
        cmdtype = "property"

type BarIdent = Text

data Payload =
  Payload {
           -- | The data of the commands.
           _cmds      :: [Command]
          ,
           -- | The identifier of the target bar on which this command should act.
           _targetBar :: BarIdent}
  deriving (Show,Eq)

makeLenses ''Font
makeLenses ''FontSetting
makeLenses ''Payload

instance ToJSON Payload where
  toJSON (Payload cmds bar) = object ["bar_name" .= bar,"commands" .= cmds]

mkDefaultSetup :: (FontFace -> Font) -- ^ The font to use.
               -> Text -- ^ The name of the bar.
               -> Payload -- ^ A payload that initializes the bar.
mkDefaultSetup f = Payload props
  where props = Repaint : map SetProp [colours,fonts,contents]
        contents = Contents "Some text here"
        colours =
          mkColorSettings
            [(Background,Colors.black)
            ,(Foreground,Colors.white)
            ,(Success,Colors.green)
            ,(Warning,Colors.yellow)
            ,(Failure,Colors.red)]
        fonts =
          mkFontSettings
            [(NormalFont,f Normal)
            ,(BoldFont,f Bold)
            ,(IndexedFont 1,Font "Serif" 12 Normal)
            ,(NamedFont "Sans",Font "Sans" 12 Normal)]

defaultSetup :: Payload
defaultSetup = mkDefaultSetup iosevkaWithFace barName

iosevkaWithFace :: FontFace -> Font
iosevkaWithFace = Font "Iosevka" 12

pretty :: ToJSON a
       => a -> String
pretty = BS.unpack . A.encodePretty

pretty' :: ToJSON a
        => a -> IO ()
pretty' = putStrLn . pretty

writePretty :: ToJSON a
            => FilePath -> a -> IO ()
writePretty path val = BS.writeFile path val'
  where val' = A.encodePretty val <> eofMarker
        eofMarker = BS.pack "\n\0"

barName :: Text
barName = "top/cpu"
