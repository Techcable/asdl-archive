{- 

HaskellBase.hs: Pickling/unpickling primitives for Haskell

Fermin Reig Galilea
University of Glasgow
http://www.dcs.gla.ac.uk/~reig/

13-1-1998
Modified by Daniel Wang for new asdlGen naming convention.
-}

module StdPrimsUtil(write_int, read_int, write_string, read_string, 
                    write_identifier, read_identifier)  where

import Prelude
import IO
import qualified StdPkl
import Char (chr, ord)
type Identifier = String

write_int :: Int -> StdPkl.Outstream -> IO ()
write_int = StdPkl.write_tag

read_int :: StdPkl.Instream -> IO Int
read_int = read_int

write_string :: String -> StdPkl.Outstream -> IO ()
write_string str s = do
		      StdPkl.write_tag (length str) s
		      hPutStr s str

read_string :: StdPkl.Instream -> IO String
read_string s = do
		  sz  <- StdPkl.read_tag s
		  str <- readN s sz
		  return str 

write_identifier :: Identifier -> StdPkl.Outstream -> IO ()
write_identifier = write_string

read_identifier :: StdPkl.Instream -> IO Identifier
read_identifier = read_string 

readN :: StdPkl.Instream -> Int -> IO [Char]
readN s n = if n == (0::Int) then return ""
	              else do {c  <- hGetChar s;
			       cs <- readN s (n-1);
			       return (c:cs)}






