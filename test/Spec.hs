import Test.Hspec
import Simulation

main :: IO ()
main = hspec $ do
   describe "subgrupo de ejemplo" $ do
       it "descripcion de ejemplo" $ do
           10 `shouldBe` 10 
