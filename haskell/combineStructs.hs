import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Util (lengthExceeds)

-- Create Map from list of pairs, making dup vals into a list
fromListWithDuplicates :: Ord k => [(k, v)] -> Map k [v]
fromListWithDuplicates pairs = 
    Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]

data Person = Person { 
    name :: String 
    } deriving (Show, Eq)

households = [[Person "Alice", Person "Bob"],
              [Person "Carlos"],
              [Person "Dabir", Person "Eashan"],
              [Person "Fatima"] ]

interviewers = [("Agent1", [0]), ("Agent2", [1,2]), ("Agent3", [3])]

multiInterviews households interviewers =
    let assignments = [(agent, name person) | 
                       (agent, houseIndices) <- interviewers,
                       index <- houseIndices,
                       person <- (households !! index) ]
    in filter (flip lengthExceeds 1 . snd)
        (Map.assocs $ fromListWithDuplicates assignments)

main :: IO ()
main = do
    putStrLn $ show (multiInterviews households interviewers)


