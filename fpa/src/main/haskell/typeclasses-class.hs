-- Default type class instances e.g. provided by a library, is not what Haskell is all about but...

-- LIBRARY

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE IncoherentInstances   #-}

-- Now that we convinced the compiler to allow them we're ready to go.
module TypeClasses where

-- Mask (optionally) hides information when disclosing it as a string.
class Mask a where
  disclose :: a -> String

-- The Mask 'default' instance hides nothing, i.e. it delegates to show.
instance (Show a) => Mask a where
  disclose a = show a


-- CLIENT

data BankNumber = BankNumber String
  deriving (Show)

data Customer   = Customer String BankNumber
  deriving (Show)

instance Mask BankNumber where
  disclose _ = "BankNumber <masked>"

instance Mask Customer where
  disclose (Customer name number) = "Customer " ++ (disclose name) ++ " " ++ (disclose number)

-- USE-CASE e.g. logging.

main :: IO ()
main = do
  putStrLn $ disclose "<any-string>"
  putStrLn $ disclose 666
  putStrLn $ disclose (BankNumber "1234567890")
  putStrLn $ disclose (Customer "Name" (BankNumber "1234567890"))
  return ()
