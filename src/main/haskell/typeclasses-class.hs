-- Default type class instances is not what Haskell is all about but...
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE IncoherentInstances   #-}

-- Now that we confinced the compiler to allow them we're ready to go.
module TypeClasses where

-- Mask (optionally) hides information when disclosing it as a string.
class Mask a where
  disclose :: a -> String

-- The Mask 'default' instance hides nothing, i.e. it delegates to show,
-- note that this instance won't compile without the LANGUAGE pragmas at
-- the top of this file, and the compiler won't allow this 'default'
-- instance to be overlapped by e.g. the bank number mask below without
-- the OVERLAPPABLE pragma in its definition.
instance {-# OVERLAPPABLE #-} (Show a) => Mask a where
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

main :: IO ()
main = do
  putStrLn $ disclose "<any-string>"
  putStrLn $ disclose 666
  putStrLn $ disclose (BankNumber "1234567890")
  putStrLn $ disclose (Customer "Name" (BankNumber "1234567890"))
  return ()
