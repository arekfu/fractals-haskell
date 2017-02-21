import qualified FractalsTest

main :: IO ()
main = do
    _ <- FractalsTest.runTests
    return ()
