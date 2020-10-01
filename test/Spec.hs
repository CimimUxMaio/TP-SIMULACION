import Test.Hspec
import Simulation

main :: IO ()
main = hspec $ do
   describe "setTC/2:" $ do
       it "setTC 10 to a state with tc = 0 should return a state with tc = 10" $ do
           (tc $ setTC 10 initialState) `shouldBe` 10 
       it "setTC 0 to a state with tc = 0 should return a state with tc = 0" $ do
           (tc $ setTC 0 initialState) `shouldBe` 0
       it "setTC should not return a state with diferent fields than the original other than tc" $ do
           setTC 0 initialState `shouldBe` initialState

   describe "setAcum/2:" $ do
       it "setAcum 10 to a state with acum = 0 should return a state with acum = 10" $ do
           (acum $ setAcum 10 initialState) `shouldBe` 10 
       it "setAcum 0 to a state with acum = 0 should return a state with acum = 0" $ do
           (acum $ setAcum 0 initialState) `shouldBe` 0
       it "setAcum should not return a state with diferent fields than the original other than acum" $ do
           setAcum 0 initialState `shouldBe` initialState

   describe "setActualTime/2" $ do
       it "setActualTime 10 to a state with acum = 0 should return a state with actualTime = 10" $ do
           (actualTime $ setActualTime 10 initialState) `shouldBe` 10 
       it "setActualTime 0 to a state with acum = 0 should return a state with actualTime = 0" $ do
           (actualTime $ setActualTime 0 initialState) `shouldBe` 0
       it "setActualTime should not return a state with diferent fields than the original other than actualTime" $ do
           setActualTime 0 initialState `shouldBe` initialState

   describe "setNextRequest/2" $ do
       it "setNextRequest 10 to a state with nextRequest = 0 should return a state with nextRequest = 10" $ do
           (nextRequest $ setNextRequest 10 initialState) `shouldBe` 10 
       it "setNextRequest 0 to a state with acum = 0 should return a state with nextRequest = 0" $ do
           (nextRequest $ setNextRequest 0 initialState) `shouldBe` 0
       it "setNextRequest should not return a state with diferent fields than the original other than nextRequest" $ do
           setNextRequest 0 initialState `shouldBe` initialState

   describe "addTC/2" $ do
       it "addTC 10 to a state with tc = 10 should return a state with tc = 20" $ do
           (tc $ addTC 10 (initialState { tc = 10 })) `shouldBe` 20
       it "addTC 0 to a state with tc = 0 should return an identical state" $ do
           addTC 0 initialState `shouldBe` initialState
