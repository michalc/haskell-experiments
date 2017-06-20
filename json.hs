{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Aeson
import Data.Decimal
import Data.Word8

parse precision (Number n) = return $ Data.Decimal.realFracToDecimal precision n
parse precision x = fail $ "Expectig a number in the JSON to parse to a Decimal. Received " ++ (show x)

newtype Decimal_1 = Decimal_1 Data.Decimal.Decimal
newtype Decimal_2 = Decimal_2 Data.Decimal.Decimal

instance ToJSON Decimal_1  where
  toJSON (Decimal_1 d) = toJSON $ show d

instance FromJSON Decimal_1  where
  parseJSON = Decimal_1 $ parse $ fromInteger 1

instance FromJSON Decimal_2  where
  parseJSON = Decimal_2 $ parse $ fromInteger 2

main = print (decode "4" :: Maybe Decimal_1)

