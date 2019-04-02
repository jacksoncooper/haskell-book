-- Chapter Exercises, Page 574

module Chapter14.CipherTesting where

import Test.Hspec
import Test.QuickCheck

import qualified Chapter09.Caesar as Caesar (decode, encode)
import qualified Chapter11.Vigenere as Vigenere (decode, encode)

newtype Key = Key String
  deriving Show

newtype Message = Message String
  deriving (Eq, Show)

instance Arbitrary Key where
  arbitrary = do
    key <- keyGenerator
    return $ Key key

instance Arbitrary Message where
  arbitrary = do
    message <- messageGenerator
    return $ Message message

uppercaseGenerator :: Gen Char
uppercaseGenerator = elements ['A'..'Z']

spaceGenerator :: Gen Char
spaceGenerator = return ' '

uppercaseAndSpaceGenerator :: Gen Char
uppercaseAndSpaceGenerator =
  frequency [(1, spaceGenerator), (4, uppercaseGenerator)]

keyGenerator :: Gen String
keyGenerator = listOf uppercaseGenerator

messageGenerator :: Gen String
messageGenerator = listOf uppercaseAndSpaceGenerator

caesarIdentity :: Int -> Message -> Message
caesarIdentity shiftAmount (Message message) =
  Message . Caesar.decode shiftAmount $ Caesar.encode shiftAmount message

vigenereIdentity :: Key -> Message -> Message
vigenereIdentity (Key key) (Message message) =
  Message . Vigenere.decode key $ Vigenere.encode key message

caesarIdentityProperty :: Int -> Message -> Bool
caesarIdentityProperty shiftAmount message =
  caesarIdentity shiftAmount message == stripMessage message

vigenereIdentityProperty :: Key -> Message -> Bool
vigenereIdentityProperty key message =
  vigenereIdentity key message == stripMessage message

stripMessage :: Message -> Message
stripMessage (Message message) = Message . unwords . words $ message

main :: IO ()
main = hspec $ do
  it "Testing Caesar identity property." $ do
    property caesarIdentityProperty
  it "Testing Vigenere identity property." $ do
    property vigenereIdentityProperty