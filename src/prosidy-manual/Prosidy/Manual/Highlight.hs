{-# LANGUAGE OverloadedStrings #-}
module Prosidy.Manual.Highlight where

import qualified Skylighting.Core as S
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)
import Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Foldable (for_)
import Numeric.Natural (Natural)

highlight :: Text -> Text -> Html
highlight stxName src
  | Just stx    <- syntax stxName 
  , Right lines <- tokenize stx src
  = for_ (zip [1..] lines) (uncurry renderLine)
  | otherwise
  = H.code $ H.text src

renderLine :: Natural -> S.SourceLine -> Html
renderLine lno line = H.code 
    ! H.dataAttribute "line-number" (H.toValue $ show lno)
    ! HA.class_ "source-line"
    $ for_ line $ \(type_, token) -> 
        H.span (H.text token) ! H.dataAttribute "type" (tokenClass type_)

tokenClass :: S.TokenType -> H.AttributeValue
tokenClass S.KeywordTok        = "keyword"
tokenClass S.DataTypeTok       = "data-type"
tokenClass S.DecValTok         = "integer"
tokenClass S.BaseNTok          = "nondecimal-integer"
tokenClass S.FloatTok          = "floating-point"
tokenClass S.ConstantTok       = "constant"
tokenClass S.CharTok           = "character"
tokenClass S.SpecialCharTok    = "special-character"
tokenClass S.StringTok         = "string"
tokenClass S.VerbatimStringTok = "verbatim-string"
tokenClass S.SpecialStringTok  = "special-string"
tokenClass S.ImportTok         = "import"
tokenClass S.CommentTok        = "comment"
tokenClass S.DocumentationTok  = "documentation"
tokenClass S.AnnotationTok     = "annotation"
tokenClass S.CommentVarTok     = "comment-variable"
tokenClass S.OtherTok          = "other"
tokenClass S.FunctionTok       = "function"
tokenClass S.VariableTok       = "variable"
tokenClass S.ControlFlowTok    = "control-flow"
tokenClass S.OperatorTok       = "operator"
tokenClass S.BuiltInTok        = "builtin"
tokenClass S.ExtensionTok      = "extension"
tokenClass S.PreprocessorTok   = "preprocessor"
tokenClass S.AttributeTok      = "attribute"
tokenClass S.RegionMarkerTok   = "region-marker"
tokenClass S.InformationTok    = "information"
tokenClass S.WarningTok        = "warning"
tokenClass S.AlertTok          = "alert"
tokenClass S.ErrorTok          = "error"
tokenClass S.NormalTok         = "normal"

tokenize :: S.Syntax -> Text -> Either String [S.SourceLine]
tokenize = S.tokenize S.TokenizerConfig
    { S.syntaxMap   = syntaxMap
    , S.traceOutput = False
    }

syntax :: Text -> Maybe S.Syntax
syntax = flip S.lookupSyntax syntaxMap

syntaxMap :: S.SyntaxMap
syntaxMap = Map.insert "Prosidy" prosidySyntax mempty

formatOptions :: S.FormatOptions
formatOptions = S.defaultFormatOpts

prosidySyntax :: S.Syntax
prosidySyntax = unsafePerformIO $ do 
    result <- S.parseSyntaxDefinition "./kate/prosidy.xml"
    either fail pure result

hex :: String -> Maybe S.Color
hex = S.toColor