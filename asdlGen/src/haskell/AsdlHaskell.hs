module AsdlHaskell(Identifier, write_string, read_string, write_identifier, 
		   read_identifier, write_list, read_list, write_option,
	           read_tag,write_tag,read_option, write_int, read_int, die)
  where

import IO
import BitSet

type Identifier = String

write_string :: String -> Handle -> IO ()
write_string str s = do
		      write_int (length str) s
		      hPutStr s str

read_string :: Handle -> IO String
read_string s = do
		  sz  <- read_int s
		  str <- readN s sz
		  return str 

write_identifier = write_string 
read_identifier = read_string 

readN :: Handle -> Int -> IO [Char]
readN s n = if n == (0::Int) then return ""
	              else do {c  <- hGetChar s;
			       cs <- readN s (n-1);
			       return (c:cs)}

write_list :: (a -> Handle -> IO b) -> [a] -> Handle -> IO ()
write_list f xs s = do 
		     write_int (length xs) s
		     sequence (map (\x -> f x s) xs)

read_list :: (Handle -> IO a) -> Handle -> IO [a]
read_list f s = do
		 len <- read_int s
		 if len < 0 then error "neg size" else 
		    accumulate (replicate len (f s))
	

write_option :: (a -> Handle -> IO ()) -> Maybe a -> Handle -> IO ()
write_option f Nothing s  = write_int 0 s
write_option f (Just x) s = do 
			      write_int 1 s
			      f x s
die :: a
die = error "Pickler error"

read_option :: (Handle -> IO a) -> Handle -> IO (Maybe a)
read_option f s = do
		   n <- read_int s
		   case n of
		      0 -> return Nothing
		      1 -> do { o <- f s ; return (Just o)}
		      _ -> die



write_int :: Int -> Handle -> IO ()
write_int n s = loop (abs n)
	        where 
		loop x | x <= 63   = hPutChar s (chr (finish (n<0) x))
		       | otherwise = do 
				      hPutChar s (chr (nibble x))
				      loop (x `shiftr` 7)
	        nibble n = ((n `andb` 0x7f) `orb` 0x80) `andb` 255
		finish false n = n `andb` 255
		finish true  n = (n `orb` 0x40) `andb` 255 


continue_bit_set :: Int -> Bool
continue_bit_set w = (w `andb` 0x80) /= 0

neg_bit_set :: Int -> Bool
neg_bit_set w      = (w `andb` 0x40) /= 0

read_int :: Handle -> IO Int
read_int s  
 = do {
       c  <- hGetChar s;
       loop ((ord c) `andb` 255) 0 0 
   }
   where
   loop n acc shift 
     = if (continue_bit_set n) then
	do {
	  c <- hGetChar s;
	  loop ((ord c) `andb` 255) 
		 (acc `orb` ((n `andb` 0x7f) `shiftl` shift)) 
		 (shift+7)
	}
       else 
	let acc' = acc `orb` ((n `andb` 0x3f) `shiftl` shift) 
	in 
	  return (if neg_bit_set n then (negate acc') else  acc')


write_tag :: Int -> Handle -> IO ()
write_tag = write_int
read_tag :: Handle -> IO Int
rad_tag = read_int