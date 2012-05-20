{-# LANGUAGE DeriveGeneric, KindSignatures, 
    FlexibleInstances, TypeOperators, TypeSynonymInstances,
    MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances,
    ScopedTypeVariables, EmptyDataDecls, DefaultSignatures,
    UndecidableInstances, FlexibleContexts, StandaloneDeriving, IncoherentInstances,
    DeriveDataTypeable #-}
module Language.C.Simple.CType where
import Data.Data
import Data.Typeable
import TypeLevel.NaturalNumber
import Foreign.C.Types



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
                   deriving(Show, Eq, Data, Typeable)

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
           deriving(Show, Eq, Data, Typeable)
           
-----------------------------------------------------------------------------------------
-- A Helper Type for Conversion
-----------------------------------------------------------------------------------------
data FixedArray n a = FixedArray n [a]
    deriving(Eq, Show)

-----------------------------------------------------------------------------------------
-- | ToCType Class --------------------------------------
-----------------------------------------------------------------------------------------
class ToCType a where
   toCType :: a -> CType
   
-------------------------------------- Instances ----------------------------------------
instance ToCType CChar where
    toCType = const (TPrimitive TChar)
    
instance ToCType CSChar where
    toCType = const (TPrimitive TSChar)    
    
instance ToCType CUChar where
    toCType = const (TPrimitive TUChar)    
    
instance ToCType CShort where
    toCType = const (TPrimitive TShort)    
    
instance ToCType CUShort where
    toCType = const (TPrimitive TUShort)    
    
instance ToCType CInt where
    toCType = const (TPrimitive TInt)    
    
instance ToCType CUInt where
    toCType = const (TPrimitive TUInt)    
    
instance ToCType CLong where
    toCType = const (TPrimitive TLong)    
    
instance ToCType CULong where
    toCType = const (TPrimitive TULong)    
    
instance ToCType CPtrdiff where
    toCType = const (TPrimitive TPtrdiff)    
    
instance ToCType CSize where
    toCType = const (TPrimitive TSize)    
    
instance ToCType CWchar where
    toCType = const (TPrimitive TWchar)    
    
instance ToCType CSigAtomic where
    toCType = const (TPrimitive TSigAtomic)    
    
instance ToCType CLLong where
    toCType = const (TPrimitive TLLong)    
    
instance ToCType CULLong where
    toCType = const (TPrimitive TULLong)    
    
instance ToCType CIntPtr where
    toCType = const (TPrimitive TIntPtr)    
    
instance ToCType CUIntPtr where
    toCType = const (TPrimitive TUIntPtr)    
    
instance ToCType CIntMax where
    toCType = const (TPrimitive TIntMax)    
    
instance ToCType CUIntMax where
    toCType = const (TPrimitive TUIntMax)    
    
instance ToCType CClock where
    toCType = const (TPrimitive TClock)    
    
instance ToCType CTime where
    toCType = const (TPrimitive TTime)    
    
instance ToCType CUSeconds where
    toCType = const (TPrimitive TUSeconds)    
    
instance ToCType CSUSeconds where
    toCType = const (TPrimitive TSUSeconds)    
    
instance ToCType CFloat where
    toCType = const (TPrimitive TFloat)    
    
instance ToCType CDouble where
    toCType = const (TPrimitive TDouble)

instance (NaturalNumber n, ToCType a) => ToCType (FixedArray n a) where
    toCType = const $ TArray (naturalNumberAsInt (undefined :: n)) $ 
                    toCType (undefined :: a)



    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
