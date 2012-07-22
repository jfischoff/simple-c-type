module Language.C.Simple.CType.Build where
import Language.C.Simple.CType.Types
import Language.C.Simple.CType.Class

-- | Use a string and value to make a TMember
mkStructMember :: ToCType a => String -> a -> CType
mkStructMember name typ = TMember name $ toCType typ

-- | Wrap a PrimitiveType in TPrimitive and make a member out of it
mkPrimitiveMember :: String -> PrimitiveType -> CType
mkPrimitiveMember name primitive = TMember name $ TPrimitive primitive

-- | Create a Union type (sum type) from a name and collection of anonymous 
--   members
mkRecordCType :: String -> [CType] -> CType
mkRecordCType name members = TUnion name [TMember name $ TStruct "" members]

-- | Use a value and a count to make an TArray
mkArray :: ToCType a => Int -> a -> CType
mkArray count typ = TArray count (toCType typ)


-- | Alias for mkArray
(<||>) :: ToCType a => Int -> a -> CType
(<||>) = mkArray
infix 9 <||>

-- | Alias for mkStructMember
(<::>) :: ToCType a => String -> a -> CType
(<::>)  = mkStructMember
infix 9 <::>

-- | Alias for mkMember
(<:::>) :: String -> CType -> CType
(<:::>) = mkMember
infix 9 <:::>

mkMember name typ = TMember name typ


