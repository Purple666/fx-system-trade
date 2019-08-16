module SecretData where

data SecretData = SecretData
  { tradePracticeBearer   :: String
  , tradePracticeUrl      :: String
  , tradeProductionBearer :: String
  , tradeProductionUrl    :: String
  }

sd :: SecretData
sd =
  SecretData { tradePracticeBearer   = "Bearer 041fff2f1e9950579315d9a8d629ef9f-5b7c44123e8fc34c65951f4d3332b96b"
             , tradePracticeUrl      = "https://api-fxpractice.oanda.com/v3/accounts/101-009-11751301-001"
             , tradeProductionBearer = "Bearer 041fff2f1e9950579315d9a8d629ef9f-5b7c44123e8fc34c65951f4d3332b96b"
             , tradeProductionUrl    = "https://api-fxpractice.oanda.com/v3/accounts/101-009-11751301-001"
             }
  
