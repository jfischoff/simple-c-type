{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Language.C.Simple.CType.Types where
import Data.Data
import Data.Typeable
import GHC.Generics

-- |Primitive C Types
data PrimitiveType = TChar
                   | TSChar
                   | TUChar
                   | TShort
                   | TUShort
                   | TInt
                   | TUInt
                   | TLong
                   | TULong
                   | TPtrdiff
                   | TSize
                   | TWchar
                   | TSigAtomic
                   | TLLong
                   | TULLong
                   | TIntPtr
                   | TUIntPtr
                   | TIntMax
                   | TUIntMax
                   | TClock
                   | TTime
                   | TUSeconds
                   | TSUSeconds
                   | TFloat
                   | TDouble
                   | TVoid
                   deriving(Show, Eq, Read, Ord, Data, Typeable, Generic)

-- |A simplified C data type AST
data CType = TStruct String [CType]
           | TUnion  String [CType]
           | TPrimitive PrimitiveType
           | TArray Int CType
           | TPointer CType
           | TVariable CType
           | TMember String CType
           | TNamed String
           | TFuncPointer [CType]
           | TEnum String [String]
           deriving(Show, Eq, Read, Ord, Data, Typeable, Generic)

-- | A helper type for conversion           
data FixedArray n a = FixedArray n [a]
    deriving(Eq, Show, Read, Ord, Data, Typeable, Generic)
           
