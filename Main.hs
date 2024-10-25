import Data.Char (isDigit)
import System.IO

-- Data Definitions --
data Gender = Male | Female deriving (Show)

data Person = Person {personName :: String, personGender :: Gender, personAge :: Int, personHeight :: Double, personWeight :: Double}

data BMICategory = Underweight | Healthy | Overweight | Obese deriving (Show)

data BMIResult = BMIResult {bmiValue :: Double, bmiCategory :: BMICategory}

data InputError = EmptyInput | InvalidInput String deriving (Show)

-- BMI Calculation --
calculateBMI :: Person -> BMIResult
calculateBMI person =
  let bmi = personWeight person / (personHeight person ^ 2)
      category = categorizeBMI bmi
   in BMIResult bmi category

categorizeBMI :: Double -> BMICategory
categorizeBMI bmi
  | bmi < 18.5 = Underweight
  | bmi < 25 = Healthy
  | bmi < 30 = Overweight
  | otherwise = Obese

-- Display BMI Information --
displayBMIInfo :: Person -> BMIResult -> IO ()
displayBMIInfo person result = do
  putStrLn $ "BMI Value for " ++ personName person ++ ": " ++ show (bmiValue result)
  putStrLn $ "BMI Category: " ++ show (bmiCategory result)
  putStrLn $ case bmiCategory result of
    Underweight -> "Advice: " ++ personName person ++ ", a BMI of less than 18.5 indicates that you are underweight. You may need to put on some weight. Consult with your doctor or a dietitian for advice."
    Healthy -> "Advice: " ++ personName person ++ ", a BMI of 18.5-24.9 indicates that you are at a healthy weight for your height. By maintaining a healthy weight, you lower your risk of developing serious health problems."
    Overweight -> "Advice: " ++ personName person ++ ", a BMI of 25-29.9 indicates that you are slightly overweight. You may be advised to lose some weight for health reasons. Consult with your doctor or a dietitian for advice."
    Obese -> "Advice: " ++ personName person ++ ", a BMI of 30 or more indicates that you are in the obese category. Your health may be at risk if you do not lose weight. Consult with your doctor or a dietitian for advice."

-- Display BMI Information Overview --
displayBMIInformation :: IO ()
displayBMIInformation = do
  putStrLn "Body mass index (BMI) combines weight and height to assess body size. It serves as a screening tool for identifying underweight, healthy weight, overweight, or obesity. Deviations from a healthy BMI range may increase health risks, such as diabetes and cardiovascular problems for those carrying excess weight, or malnutrition for those with low weight."

  putStrLn "While BMI doesn't measure body fat directly and doesn't consider age, sex, ethnicity, or muscle mass, it provides standard categories for doctors to track population weight status and individual health issues."

  putStrLn "Writer: Yvette Brazier, November 9, 2018."

-- User Interface --
displayMenu :: IO ()
displayMenu = do
  putStrLn "1. BMI Information"
  putStrLn "2. Calculate BMI"
  putStrLn "3. Exit"

-- Input Validation Functions --
getUserInputWithPrompt :: String -> (String -> Either InputError a) -> IO a
getUserInputWithPrompt prompt validator = do
  putStrLn prompt
  input <- getLine
  case validator input of
    Right value -> return value
    Left err -> do
      putStrLn $ "Invalid input: " ++ show err
      getUserInputWithPrompt prompt validator

readDouble :: String -> Either InputError Double
readDouble s
  | null s = Left EmptyInput
  | otherwise =
      case reads s of
        [(x, "")] -> if x <= 0 then Left (InvalidInput "Weight must be greater than 0.") else Right x
        _ -> Left (InvalidInput "Please enter a valid number.")

readInt :: String -> Either InputError Int
readInt s
  | null s = Left EmptyInput
  | otherwise =
      case reads s of
        [(x, "")] -> if x <= 0 then Left (InvalidInput "Age must be greater than 0.") else Right x
        _ -> Left (InvalidInput "Please enter a valid whole number for age.")

readGender :: String -> Either InputError Gender
readGender s =
  case s of
    "male" -> Right Male
    "female" -> Right Female
    _ -> Left (InvalidInput "Please enter 'male' or 'female'.")

validateHeight :: Double -> Either InputError Double
validateHeight height
  | height <= 0 = Left (InvalidInput "Height must be greater than 0.")
  | height > 3 = Left (InvalidInput "Please enter your height in meters.")
  | otherwise = Right height

validateName :: String -> Either InputError String
validateName name
  | null name = Left EmptyInput
  | any isDigit name = Left (InvalidInput "Name cannot contain numbers.")
  | otherwise = Right name

-- User Input --
getUserInput :: IO Person
getUserInput = do
  name <- getUserInputWithPrompt "Enter name:" validateName
  gender <- getUserInputWithPrompt "Enter gender (male/female):" readGender
  age <- getUserInputWithPrompt "Enter age:" readInt
  height <- getUserInputWithPrompt "Enter height (in meters):" $ \input -> readDouble input >>= validateHeight
  weight <- getUserInputWithPrompt "Enter weight (in kilograms):" readDouble

  return $ Person name gender age height weight

-- Main Program --
main = do
  putStrLn "BMI Calculator"
  let loop = do
        displayMenu
        putStrLn "Enter your choice:"
        choice <- getLine
        case choice of
          "1" -> displayBMIInformation >> loop
          "2" -> do
            person <- getUserInput
            let bmiResult = calculateBMI person
            displayBMIInfo person bmiResult
            loop
          "3" -> putStrLn "Exiting program"
          _ -> putStrLn "Invalid choice. Please try again." >> loop
  loop
