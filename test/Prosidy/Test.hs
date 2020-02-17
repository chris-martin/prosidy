import Test.Tasty
-- import qualified Prosidy.Test.Prosidy
import qualified Prosidy.Test.Source
import qualified Prosidy.Test.Types
import qualified Prosidy.Test.Parse

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
    parseTests <- Prosidy.Test.Parse.tests
    pure $ testGroup "test"
        [ Prosidy.Test.Source.tests
        , Prosidy.Test.Types.tests
        , parseTests
        ]