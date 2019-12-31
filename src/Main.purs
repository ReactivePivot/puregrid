module Main where

import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Treap.Print (printR)
import Data.Treap.Random (rItemAtIndex, rRange, rTreapAdd)
import Effect (Effect)
import Effect.Aff (Fiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Grid.Axis (AxisEntry(..), AxisSize(..), AxisTreap, mkAxisTreap)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Prelude (Unit, bind, mempty, show, ($), (<$>), (<>))
import Simple.JSON as JSON

type AxisJson = { key :: String, width :: Number }

loadJson :: String -> Maybe (List AxisJson)
loadJson j = 
  case JSON.readJSON j of
    Right (r :: Array AxisJson) -> Just $ fromFoldable r
    Left e -> Nothing

fromAxisJson :: AxisJson -> AxisEntry String
fromAxisJson j = AxEntry j.key (AxSize j.width 1)

mkFromList :: Int -> List AxisJson -> AxisTreap String
mkFromList i l = fromList (mkAxisTreap i) l

fromList :: AxisTreap String -> List AxisJson -> AxisTreap String
fromList t Nil = t
fromList t (Cons x xs) = fromList (rTreapAdd (fromAxisJson x) t) xs

addszr :: AxisEntry String -> String -> String
addszr (AxEntry k (AxSize _ c)) i = i <> k

getWidth :: AxisSize -> Additive Number
getWidth (AxSize w _) = Additive w

getKey :: forall a. AxisEntry a -> a
getKey (AxEntry k _) = k

main :: Effect (Fiber Unit)
main = launchAff do
  input <- (readTextFile UTF8 "src\\data.json")
  
  let t = mkFromList 22 <$> loadJson input
  let find_val = Additive 170.0
  let to_val = Additive 369.0

  liftEffect do
    _ <- log $ "Range: " <> show find_val <> "-" <> show to_val    
    case t of 
        Nothing -> do
          log "Error"
        Just x -> do 
          _ <- log $ show $ 
            foldr (<>) mempty $ 
              getKey <$> (rRange getWidth find_val to_val x)
          _ <- log $ printR x
          log $ show $ rItemAtIndex 5 x
          --log $ show $ (foldl (flip addszr) "" x)