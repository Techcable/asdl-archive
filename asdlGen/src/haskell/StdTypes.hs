{- Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_import HaskellBase
  --line_width 74
  --no_action false
  --output_directory ../asdl
  --view Haskell
  -}
module StdTypes (Bool(..), Nat, Int8, Ieee_real, Int16, Int32, Int64,
Uint8, Uint16, Uint32, Uint64, write_nat, write_tagged_nat, read_nat,
read_tagged_nat, write_bool, write_tagged_bool, read_bool,
read_tagged_bool, write_int8, write_tagged_int8, read_int8,
read_tagged_int8, write_ieee_real, write_tagged_ieee_real, read_ieee_real,
read_tagged_ieee_real, write_int16, write_tagged_int16, read_int16,
read_tagged_int16, write_int32, write_tagged_int32, read_int32,
read_tagged_int32, write_int64, write_tagged_int64, read_int64,
read_tagged_int64, write_uint8, write_tagged_uint8, read_uint8,
read_tagged_uint8, write_uint16, write_tagged_uint16, read_uint16,
read_tagged_uint16, write_uint32, write_tagged_uint32, read_uint32,
read_tagged_uint32, write_uint64, write_tagged_uint64, read_uint64,
read_tagged_uint64) where

import qualified Prelude
import HaskellBase


data Bool = TRUE | FALSE
type Nat = (Int)
type Int8 = (Int)
type Ieee_real = (String)
type Int16 = (Int)
type Int32 = (Int)
type Int64 = (Int)
type Uint8 = (Int)
type Uint16 = (Int)
type Uint32 = (Int)
type Uint64 = (Int)

write_nat :: Nat -> Handle -> IO ()
write_tagged_nat :: Nat -> Handle -> IO ()
read_nat :: Handle -> IO Nat
read_tagged_nat :: Handle -> IO Nat
write_bool :: Bool -> Handle -> IO ()
write_tagged_bool :: Bool -> Handle -> IO ()
read_bool :: Handle -> IO Bool
read_tagged_bool :: Handle -> IO Bool
write_int8 :: Int8 -> Handle -> IO ()
write_tagged_int8 :: Int8 -> Handle -> IO ()
read_int8 :: Handle -> IO Int8
read_tagged_int8 :: Handle -> IO Int8
write_ieee_real :: Ieee_real -> Handle -> IO ()
write_tagged_ieee_real :: Ieee_real -> Handle -> IO ()
read_ieee_real :: Handle -> IO Ieee_real
read_tagged_ieee_real :: Handle -> IO Ieee_real
write_int16 :: Int16 -> Handle -> IO ()
write_tagged_int16 :: Int16 -> Handle -> IO ()
read_int16 :: Handle -> IO Int16
read_tagged_int16 :: Handle -> IO Int16
write_int32 :: Int32 -> Handle -> IO ()
write_tagged_int32 :: Int32 -> Handle -> IO ()
read_int32 :: Handle -> IO Int32
read_tagged_int32 :: Handle -> IO Int32
write_int64 :: Int64 -> Handle -> IO ()
write_tagged_int64 :: Int64 -> Handle -> IO ()
read_int64 :: Handle -> IO Int64
read_tagged_int64 :: Handle -> IO Int64
write_uint8 :: Uint8 -> Handle -> IO ()
write_tagged_uint8 :: Uint8 -> Handle -> IO ()
read_uint8 :: Handle -> IO Uint8
read_tagged_uint8 :: Handle -> IO Uint8
write_uint16 :: Uint16 -> Handle -> IO ()
write_tagged_uint16 :: Uint16 -> Handle -> IO ()
read_uint16 :: Handle -> IO Uint16
read_tagged_uint16 :: Handle -> IO Uint16
write_uint32 :: Uint32 -> Handle -> IO ()
write_tagged_uint32 :: Uint32 -> Handle -> IO ()
read_uint32 :: Handle -> IO Uint32
read_tagged_uint32 :: Handle -> IO Uint32
write_uint64 :: Uint64 -> Handle -> IO ()
write_tagged_uint64 :: Uint64 -> Handle -> IO ()
read_uint64 :: Handle -> IO Uint64
read_tagged_uint64 :: Handle -> IO Uint64

write_nat x s = 
    case (x) of 
          (nat_int1) -> 
            do
             write_int nat_int1 s
             
write_tagged_nat x s = 
    do
     write_tag 4 s
     write_nat x s
     
read_nat s = 
    do 
        nat_int1 <-  read_int s
        return ((nat_int1))
        
read_tagged_nat s = 
    do
     i <- read_tag s
     let x = (case i of
              4 -> 
                read_nat s
              _ -> 
                die)
              
     x
     
write_bool x s = 
    case (x) of 
          TRUE -> 
            do
             write_tag 1 s
             
          FALSE -> 
            do
             write_tag 2 s
             
write_tagged_bool x s = 
    do
     write_tag 5 s
     write_bool x s
     
read_bool s = 
    do
     i <- read_tag s
     let x = (case i of
              1 -> 
                return (TRUE)
              2 -> 
                return (FALSE)
              _ -> 
                die)
              
     x
     
read_tagged_bool s = 
    do
     i <- read_tag s
     let x = (case i of
              5 -> 
                read_bool s
              _ -> 
                die)
              
     x
     
write_int8 x s = 
    case (x) of 
          (int8_int1) -> 
            do
             write_int int8_int1 s
             
write_tagged_int8 x s = 
    do
     write_tag 6 s
     write_int8 x s
     
read_int8 s = 
    do 
        int8_int1 <-  read_int s
        return ((int8_int1))
        
read_tagged_int8 s = 
    do
     i <- read_tag s
     let x = (case i of
              6 -> 
                read_int8 s
              _ -> 
                die)
              
     x
     
write_ieee_real x s = 
    case (x) of 
          (string1) -> 
            do
             write_string string1 s
             
write_tagged_ieee_real x s = 
    do
     write_tag 14 s
     write_ieee_real x s
     
read_ieee_real s = 
    do 
        string1 <-  read_string s
        return ((string1))
        
read_tagged_ieee_real s = 
    do
     i <- read_tag s
     let x = (case i of
              14 -> 
                read_ieee_real s
              _ -> 
                die)
              
     x
     
write_int16 x s = 
    case (x) of 
          (int16_int1) -> 
            do
             write_int int16_int1 s
             
write_tagged_int16 x s = 
    do
     write_tag 7 s
     write_int16 x s
     
read_int16 s = 
    do 
        int16_int1 <-  read_int s
        return ((int16_int1))
        
read_tagged_int16 s = 
    do
     i <- read_tag s
     let x = (case i of
              7 -> 
                read_int16 s
              _ -> 
                die)
              
     x
     
write_int32 x s = 
    case (x) of 
          (int32_int1) -> 
            do
             write_int int32_int1 s
             
write_tagged_int32 x s = 
    do
     write_tag 8 s
     write_int32 x s
     
read_int32 s = 
    do 
        int32_int1 <-  read_int s
        return ((int32_int1))
        
read_tagged_int32 s = 
    do
     i <- read_tag s
     let x = (case i of
              8 -> 
                read_int32 s
              _ -> 
                die)
              
     x
     
write_int64 x s = 
    case (x) of 
          (int64_int1) -> 
            do
             write_int int64_int1 s
             
write_tagged_int64 x s = 
    do
     write_tag 9 s
     write_int64 x s
     
read_int64 s = 
    do 
        int64_int1 <-  read_int s
        return ((int64_int1))
        
read_tagged_int64 s = 
    do
     i <- read_tag s
     let x = (case i of
              9 -> 
                read_int64 s
              _ -> 
                die)
              
     x
     
write_uint8 x s = 
    case (x) of 
          (uint8_int1) -> 
            do
             write_int uint8_int1 s
             
write_tagged_uint8 x s = 
    do
     write_tag 10 s
     write_uint8 x s
     
read_uint8 s = 
    do 
        uint8_int1 <-  read_int s
        return ((uint8_int1))
        
read_tagged_uint8 s = 
    do
     i <- read_tag s
     let x = (case i of
              10 -> 
                read_uint8 s
              _ -> 
                die)
              
     x
     
write_uint16 x s = 
    case (x) of 
          (uint16_int1) -> 
            do
             write_int uint16_int1 s
             
write_tagged_uint16 x s = 
    do
     write_tag 11 s
     write_uint16 x s
     
read_uint16 s = 
    do 
        uint16_int1 <-  read_int s
        return ((uint16_int1))
        
read_tagged_uint16 s = 
    do
     i <- read_tag s
     let x = (case i of
              11 -> 
                read_uint16 s
              _ -> 
                die)
              
     x
     
write_uint32 x s = 
    case (x) of 
          (uint32_int1) -> 
            do
             write_int uint32_int1 s
             
write_tagged_uint32 x s = 
    do
     write_tag 12 s
     write_uint32 x s
     
read_uint32 s = 
    do 
        uint32_int1 <-  read_int s
        return ((uint32_int1))
        
read_tagged_uint32 s = 
    do
     i <- read_tag s
     let x = (case i of
              12 -> 
                read_uint32 s
              _ -> 
                die)
              
     x
     
write_uint64 x s = 
    case (x) of 
          (uint64_int1) -> 
            do
             write_int uint64_int1 s
             
write_tagged_uint64 x s = 
    do
     write_tag 13 s
     write_uint64 x s
     
read_uint64 s = 
    do 
        uint64_int1 <-  read_int s
        return ((uint64_int1))
        
read_tagged_uint64 s = 
    do
     i <- read_tag s
     let x = (case i of
              13 -> 
                read_uint64 s
              _ -> 
                die)
              
     x
     

