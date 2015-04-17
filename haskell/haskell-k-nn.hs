{-# LANGUAGE BangPatterns #-}

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

main = do
    validationSample <- fmap parseRecords (BS.readFile "validationsample.csv")
    trainingSample   <- fmap parseRecords (BS.readFile "trainingsample.csv")

    case (validationSample, trainingSample) of
        (Right validation, Right training) -> runClassifier validation training
        _ -> putStrLn "Parsing error"

runClassifier :: Observations -> Observations -> IO ()
runClassifier validation training = do
    let n = V.length validation
    results <- V.forM validation $ \v -> do
        let c = checkCorrect training v
        print (label v, c)
        return c
    print (fromIntegral (V.sum results) / fromIntegral n)

parseRecords :: BS.ByteString -> Either String (V.Vector Observation)
parseRecords = CSV.decode CSV.HasHeader

data Observation = Observation {
    label    :: !Label,
    features :: !Features
 } deriving (Show, Eq)

type Label = CSV.Field
type Features = VU.Vector Int
type Observations = V.Vector Observation

instance CSV.FromRecord Observation where
    parseRecord !v = do
        pixels <- V.mapM CSV.parseField (V.tail v)
        return $ Observation (V.head v) (VU.convert pixels)

dist :: Features -> Features -> Int
dist !x !y = VU.sum $ VU.map (^2) $ VU.zipWith (-) x y

closerTo :: Features -> Observation -> Observation -> Ordering
closerTo !target !o1 !o2 = compare (dist target (features o1)) (dist target (features o2))

classify :: Observations -> Features -> Label
classify !os !fs = label where
    (Observation label _) = V.minimumBy (closerTo fs) os

checkCorrect :: Observations -> Observation -> Int
checkCorrect !training (Observation label features)
    | label == classify training features = 1
    | otherwise = 0
