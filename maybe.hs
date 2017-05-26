-- import Control.Monad

-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Identity
-- import Control.Applicative
-- import Data.Maybe



import Control.Monad.Reader

main :: IO ()
main = putStrLn . show $ runReader computation $ MyContext "hello" 1

example :: Maybe Int
example = do
  a <- Just 3
  b <- Nothing
  return $ a + b

data MyContext = MyContext
  { foo :: String
  , bar :: Int
  } deriving (Show)


computation :: Reader MyContext (Maybe String)
computation = do
  n <- asks bar
  x <- asks foo
  if n > 0
    then return (Just x)
    else return Nothing





-- maybeGetString :: MaybeT Identity String
-- maybeGetString = return "first" >>
--                  mzero >> 
--                  return "second"

-- maybeTNothing :: MaybeT Identity String
-- maybeTNothing = MaybeT $ return Nothing




-- maybeGetString :: MaybeT Identity String
-- maybeGetString = return "first" >>
--                  mzero >> 
--                  return "second"