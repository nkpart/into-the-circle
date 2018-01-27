import           Control.Monad         (when)
import           System.Exit           (exitFailure)
import           System.IO             (BufferMode (..), IO, hSetBuffering,
                                        stdout)

-- import qualified Test.BreatheEasy.DiagnosisSheet
import qualified Test.Extractor.Remedy
import qualified Test.Types

main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Types.tests,
      Test.Extractor.Remedy.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
