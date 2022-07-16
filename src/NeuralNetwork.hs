module NeuralNetwork where

import           Lib
import           Matrix
import           Vector

data ActivationFunction a where
  ActivationFunction :: (Num a) => (a -> a) -> String -> ActivationFunction a

instance Show (ActivationFunction a) where
  show (ActivationFunction _ s) = "ActivationFunction " ++ s

sigmoid :: Floating a => ActivationFunction a
sigmoid = ActivationFunction (\x -> 1 / (1 + exp (-x))) "sigmoid"

-- data Layer n o m = SingleLayer (Matrix n m Double) | Layer (Matrix n o Double) (Matrix o m Double) deriving (Show, Eq)
data Layer (n :: Nat) (m :: Nat) a where
  SingleLayer :: (Num a) => Matrix n m a -> ActivationFunction a -> Layer n m a
  NestedLayer
    :: (Num a)
    => Matrix n m a
    -> ActivationFunction a
    -> Layer m o a
    -> Layer n o a

deriving instance (Show a) => Show (Layer n m a)

showLayers' :: Show a => Int -> Layer n m a -> String
showLayers' number (SingleLayer mat (ActivationFunction _ s)) =
  "Layer " ++ show number ++ " (activation function " ++ s ++ "):\n" ++
  showMatrix mat
showLayers' number (NestedLayer mat (ActivationFunction _ s) layers) =
  "Layer " ++ show number ++ " (activation function " ++ s ++ "):\n" ++
  showMatrix mat ++
  "\n" ++
  showLayers' (number + 1) layers

showLayers :: Show a => Layer n m a -> String
showLayers = showLayers' 1

printLayers :: Show a => Layer n m a -> IO ()
printLayers = putStrLn . showLayers

applyLayerMatrix :: Matrix 'One n a -> Layer n m a -> Matrix 'One m a
applyLayerMatrix mat (SingleLayer mat' (ActivationFunction f _)) =
  fmap f $ mat *.* mat'
applyLayerMatrix mat (NestedLayer mat' (ActivationFunction f _) nextLayer) =
  applyLayerMatrix (fmap f $ mat *.* mat') nextLayer

applyLayer :: Vector n a -> Layer n m a -> Vector m a
applyLayer vec = vecHead . getVec . applyLayerMatrix (Mat $ VecSing vec)

normaliseCols :: Fractional a => Matrix n m a -> Matrix n m a
normaliseCols mat = Mat $ Vector.vTranspose normalised
  where
    (Mat vs) = Matrix.transpose mat
    colSums = fmap (sum . fmap abs) vs
    normalised = Lib.zipWith (\a b -> fmap (/ a) b) colSums vs

normaliseLayers :: Fractional a => Layer n m a -> Layer n m a
normaliseLayers (SingleLayer mat af) = SingleLayer (normaliseCols mat) af
normaliseLayers (NestedLayer mat af layer) =
  NestedLayer (normaliseCols mat) af (normaliseLayers layer)

v1 :: (Floating a) => Vector Five a
v1 = 1 :+ 2 :+ 3 :+ 4 :+ singleton 5

m1 :: (Floating a) => Matrix Two Five a
m1 = Mat $ v1 :+ singleton v1

layer1 :: (Floating a) => Layer Two Five a
layer1 = normaliseLayers $ SingleLayer m1 sigmoid

layer2 :: (Floating a) => Layer Five Five a
layer2 = normaliseLayers $ NestedLayer (Matrix.transpose m1) sigmoid layer1
