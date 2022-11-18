{-# LANGUAGE OverloadedStrings #-}
module LibSpec
  ( spec
  ) where

import Data.String
import qualified Codec.Binary.UTF8.String as U
import qualified Data.ByteString as B
import Test.Main
import Test.Hspec
import Text.Show.Unicode
import Lib

newtype UString a = UString a deriving Eq

ustring :: B.ByteString -> UString String
ustring = UString . U.decode . B.unpack

instance IsString a => IsString (UString a) where
  fromString = UString . fromString
  
instance Show a => Show (UString a) where
  show (UString s) = ushow s

spec :: Spec
spec = describe "someFunc" $ do
  { it "「なんか関数」を標準出力に印字する." $ do
    { result <- captureProcessResult Lib.someFunc
    ; prExitCode result `shouldBe` ExitSuccess
    ; prStderr result `shouldSatisfy` B.null
    ; ustring (prStdout result) `shouldBe` "なんか関数\n"
    }
  }
