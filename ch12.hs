module Ch12 where

type Name = String

type Age = Integer

data Person =
  Person Name Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Eq)

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

-- mkPerson :: Name -> Age -> Either PersonInvalid Person
-- mkPerson name age
--   | name /= "" && age >= 0 = Right $ Person name age
--   | name == "" = Left NameEmpty
--   | otherwise = Left AgeTooLow

type ValidatePerson a =
  Either [PersonInvalid] a

ageOkay :: Age
        -> Either [PersonInvalid] Age
ageOkay age
        | age >= 0 = Right age
        | otherwise = Left [AgeTooLow]

nameOkay :: Name
        -> Either [PersonInvalid] Name
nameOkay name
        | name /= "" = Right name
        | otherwise = Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Right name) (Right age) = Right (Person name age)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge
