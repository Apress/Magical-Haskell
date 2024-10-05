{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Misc.TypeFam 
where
import Data.Kind (Type)
import Data.Word (Word8)
import GHC.TypeLits (Symbol)

import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST)
import Control.Monad (forM_)
import Data.STRef ( modifySTRef, newSTRef, readSTRef )


squareElements :: (Num a, GV.Vector v a) => v a -> v a
squareElements = GV.map (^2)

boxedVecExample :: V.Vector Int
boxedVecExample = squareElements (V.fromList [1, 2, 3, 4])

unboxedVecExample :: UV.Vector Int
unboxedVecExample = squareElements (UV.fromList [1, 2, 3, 4])

incrementVector :: Num a => [a] -> [a]
incrementVector lst = runST $ do
   -- Convert the list to a mutable vector
  mVec <- MV.new (length lst)
  -- Write the input list into the mutable vector
  forM_ (zip [0..] lst) $ \(i, value) -> MV.write mVec i value
  -- Increment each element in the mutable vector
  forM_ [0..(length lst - 1)] $ \i -> do
    val <- MV.read mVec i
    MV.write mVec i (val + 1)
  -- Freeze the mutable vector to make it immutable and then convert to a list
  frozenVec <- V.freeze mVec
  return (V.toList frozenVec)

-- A function that takes a polymorphic function and applies it to a pair of values.
applyToBoth :: (forall a. a -> b) -> (b, b)
applyToBoth f = (f 1, f 'a')

-- A function f from above must work for ALL types uniformly; it means
-- it cannot depend on the input, so can be a constant function for instance
exampleF :: forall a. a -> Int
exampleF _ = 42

res :: (Int, Int)
res = applyToBoth exampleF


data Showable = forall a. Show a => MkShowable a
-- Now ShowableList can be a list of these wrappers
type ShowableList = [Showable]

slist :: ShowableList
slist = [MkShowable 1, MkShowable "hello", MkShowable (23, "John")]


sumST :: Num a => [a] -> a
sumST xs = runST $ do
  sumRef <- newSTRef 0  -- Create a new mutable reference
  mapM_ (modifySTRef sumRef . (+)) xs
  readSTRef sumRef  -- Return the pure result

result :: Int
result = sumST [1, 2, 3, 4, 5]  -- usage




type family AddTypes a b :: Type
type instance AddTypes Int Double = Double
type instance AddTypes Int String = Bool

-- then we can write:
func :: String -> AddTypes Int String
func "hello" = True


data family SuperList a :: Type
data instance SuperList Char = SCons !Char !(SuperList Char) | SNil
newtype instance SuperList () = SListUnit Int
type Bit = Bool
data instance SuperList Bit = SBitList [Word8]

class SLength a where
    slength :: SuperList a -> Int

class SMap a b where
    smap :: (a -> b) -> SuperList a -> SuperList b
    
instance SMap Char Char where
    smap _ SNil = SNil
    smap f (SCons c cs) = SCons (f c) (smap f cs)

instance SLength Char where
    slength SNil = 0
    slength (SCons _ xs) = 1 + slength xs

-- Instance for SuperList () (unmappable, no meaningful transformation)
instance SMap () () where
    smap _ (SListUnit n) = SListUnit n

instance SLength () where
    slength (SListUnit n) = n

instance SMap Char () where
    smap _ lst = SListUnit (slength lst)

class Database a where
       type TableName a :: Symbol
       type Record a :: Type
   
data User = User { userId :: Int, userName :: String }

instance Database User where
    type TableName User = "users"
    type Record User = User

class FileSystem fs where
    data FileHandle fs
    data MyFilePath fs
    openFile :: MyFilePath fs -> IO (FileHandle fs)
    closeFile :: FileHandle fs -> IO ()

data LocalFS
data CloudFS

instance FileSystem LocalFS where
    data FileHandle LocalFS = LocalFileHandle Int  -- File descriptor
    data MyFilePath LocalFS = LocalPath String
    openFile (LocalPath path) = putStrLn ("Opening local file at " ++ path) >> return (LocalFileHandle 1)
    closeFile (LocalFileHandle fd) = putStrLn ("Closing local file with descriptor " ++ show fd)

instance FileSystem CloudFS where
    data FileHandle CloudFS = CloudFileHandle Int
    data MyFilePath CloudFS = CloudPath String
    openFile (CloudPath path) = putStrLn ("Opening cloud file at " ++ path) >> return (CloudFileHandle 1)
    closeFile (CloudFileHandle fd) = putStrLn ("Closing cloud file with descriptor " ++ show fd)