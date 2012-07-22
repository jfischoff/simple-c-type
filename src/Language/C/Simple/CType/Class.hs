{-# LANGUAGE ScopedTypeVariables #-}
module Language.C.Simple.CType.Class where
import TypeLevel.NaturalNumber
import Foreign.C.Types
import Language.C.Simple.CType.Types

-- | A class to make conversion to CType easier
class ToCType a where
   toCType :: a -> CType
   
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