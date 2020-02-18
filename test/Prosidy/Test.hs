import Test.Tasty
import Test.Tasty.Runners.AntXML (antXMLRunner)
import qualified Prosidy.Test.Source
import qualified Prosidy.Test.Types
import qualified Prosidy.Test.Parse

main :: IO ()
main = tests >>= defaultMainWithIngredients
    [ antXMLRunner
    ]

tests :: IO TestTree
tests = do
    parseTests <- Prosidy.Test.Parse.tests
    pure $ testGroup "test"
        [ Prosidy.Test.Source.tests
        , Prosidy.Test.Types.tests
        , parseTests
        ]