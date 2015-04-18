import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

main = do
    validationSample <- fmap parseRecords (BL.readFile "validationsample.csv")
    trainingSample   <- fmap parseRecords (BL.readFile "trainingsample.csv")

    case (validationSample, trainingSample) of
        (Right validation, Right training) -> runClassifier validation training
        _ -> putStrLn "Parsing error"

runClassifier :: Observations -> Observations -> IO ()
runClassifier validation training = do
    let n = V.length validation
        correct = V.map (isCorrect training) validation
    print (fromIntegral (V.sum correct) / fromIntegral n)

parseRecords :: BL.ByteString -> Either String (V.Vector Observation)
parseRecords = CSV.decode CSV.HasHeader

data Observation = Observation {
    label    :: Label,
    features :: Features
 } deriving (Show, Eq)

type Label = CSV.Field
type Features = VU.Vector Int
type Observations = V.Vector Observation

instance CSV.FromRecord Observation where
    parseRecord v = do
        pixels <- V.mapM CSV.parseField (V.tail v)
        return $ Observation (V.head v) (VU.convert pixels)

dist :: Observation -> Observation -> Int
dist o1 o2 = VU.sum $ VU.map (^2) $ VU.zipWith (-) x y where
    (x, y) = (features o1, features o2)

closestTo :: Observation -> Observation -> Observation -> Ordering
closestTo target o1 o2 = compare (dist target o1) (dist target o2)

classify :: Observations -> Observation -> Label
classify os o = label where
    (Observation label _) = V.minimumBy (closestTo o) os

isCorrect :: Observations -> Observation -> Int
isCorrect training example
    | label example == classify training example = 1
    | otherwise = 0
