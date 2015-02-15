import System.Environment (getArgs)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad.Par
import Control.DeepSeq

data IList a = Nil | Cons a (IVar (IList a))

type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons h t) = rnf h `seq` rnf t

streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = do
  var <- new
  fork $ loop xs var
  return var
  where
    loop [] st = put st Nil
    loop (x:xs') st = do
      tail <- new
      put st (Cons x tail)
      loop xs' tail

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold f acc st = do
  iList <- get st
  case iList of
    Nil -> return acc
    Cons h t -> streamFold f (f acc h) t

streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap f inSt = do
  outSt <- new
  fork $ loop inSt outSt
  return outSt
  where
    loop inSt' outSt' = do
      iList <- get inSt'
      case iList of
        Nil -> put outSt' Nil
        Cons h t -> do
          outStTail <- new
          put outSt' $ Cons (f h) outStTail
          loop t outStTail
      
pipeline :: ByteString -> ByteString
pipeline ss = runPar $ do
  s0 <- streamFromList $ chunk 1000 ss
  s1 <- streamMap (byChar encode) s0
  s2 <- streamMap (byChar decode) s1
  xs <- streamFold (\acc x -> x : acc) [] s2
  return (B.unlines $ reverse xs)

p, q, e, n, d, l :: Integer
p = 17
q = 11
e = l - 1 
n = p * q
d = let (d',_) = gcdEx e l in if d' < 0 then d' + l else d'
l = (p-1) * (q-1) `div` gcd (p-1) (q-1)

gcdEx :: Integer -> Integer -> (Integer,Integer)
gcdEx a b
  | b == 0 = (1,0)
  | otherwise = 
    let q = a `div` b
        r = a `mod` b
        (x,y) = gcdEx b r
    in (y, x - q * y)

byChar :: (Integer -> Integer) -> ByteString -> ByteString
byChar f = B.pack 
           . map (toEnum . fromIntegral . f . fromIntegral . fromEnum)
           . B.unpack

byLine :: ([ByteString] -> [ByteString]) -> ByteString -> ByteString
byLine f = B.unlines . f . B.lines

encrypt_seq :: ByteString -> ByteString
encrypt_seq = B.unlines 
            . map (byChar encode)
            . chunk 1000 

decrypt_seq :: ByteString -> ByteString
decrypt_seq = byLine $ map (byChar decode)

encode :: Integer -> Integer
encode x = x ^ e `mod` n

decode :: Integer -> Integer
decode x = x ^ d `mod` n

chunk :: Int -> ByteString -> [ByteString]
chunk n bs 
  | B.length bs < n = [bs]
  | otherwise = h : chunk n t
  where 
    (h, t) = B.splitAt n bs

main :: IO ()
main = do
  [command, input] <- getArgs
  content <- case input of
    "-" -> B.getContents
    x -> B.readFile input
  case command of
    "enc_seq" -> B.putStr . encrypt_seq $ content
    "dec_seq" -> B.putStr . decrypt_seq $ content
    "encdec_seq" -> B.putStr . decrypt_seq . encrypt_seq $ content
    "pipeline"     -> B.putStr . pipeline $ content
    _ -> error "pass `enc_seq` or `dec_seq` or `enc` as first argument"
