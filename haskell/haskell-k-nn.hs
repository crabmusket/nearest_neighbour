{-# LANGUAGE BangPatterns #-}

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Control.Parallel.Strategies as P

main = do
    validationSample <- fmap parseRecords (BL.readFile "validationsample.csv")
    trainingSample   <- fmap parseRecords (BL.readFile "trainingsample.csv")

    case (validationSample, trainingSample) of
        (Right validation, Right training) -> runClassifier validation training
        _ -> putStrLn "Parsing error"

runClassifier :: Observations -> Observations -> IO ()
runClassifier validation training =
    let n = V.length validation
        inParallel = P.withStrategy (P.parTraversable P.rpar)
        results = inParallel $ V.map (classify training) validation
        score l o = if l == label o then 1 else 0
        correct = V.zipWith score results validation
     in print (fromIntegral (V.sum correct) / fromIntegral n)

parseRecords :: BL.ByteString -> Either String Observations
parseRecords = CSV.decode CSV.HasHeader

data Observation = Observation {
    label    :: !Label,
    features :: !Features
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
classify training obs = label closest where
    closest = V.minimumBy (closestTo obs) training
