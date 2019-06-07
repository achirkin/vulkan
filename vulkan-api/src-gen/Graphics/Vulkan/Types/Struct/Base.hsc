#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Base
       (VkBaseInStructure, VkBaseInStructure', VkBaseOutStructure, -- ' closing tick for hsc2hs
        VkBaseOutStructure') -- ' closing tick for hsc2hs
       where
import Foreign.Storable                         (Storable (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkBaseInStructure {
--   >     VkStructureType sType;
--   >     const struct VkBaseInStructure* pNext;
--   > } VkBaseInStructure;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBaseInStructure VkBaseInStructure registry at www.khronos.org>
type VkBaseInStructure = VulkanStruct VkBaseInStructure' -- ' closing tick for hsc2hs

data VkBaseInStructure' -- ' closing tick for hsc2hs

instance Eq VkBaseInStructure where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkBaseInStructure where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkBaseInStructure where
        sizeOf ~_ = #{size VkBaseInStructure}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBaseInStructure}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkBaseInStructure where
        type StructFields VkBaseInStructure = '["sType", "pNext"] -- ' closing tick for hsc2hs
        type CUnionType VkBaseInStructure = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBaseInStructure = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBaseInStructure = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkBaseInStructure
         where
        type FieldType "sType" VkBaseInStructure = VkStructureType
        type FieldOptional "sType" VkBaseInStructure = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBaseInStructure =
             #{offset VkBaseInStructure, sType}
        type FieldIsArray "sType" VkBaseInStructure = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBaseInStructure, sType}

instance {-# OVERLAPPING #-} CanReadField "sType" VkBaseInStructure
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBaseInStructure, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBaseInStructure, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBaseInStructure where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBaseInStructure, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkBaseInStructure
         where
        type FieldType "pNext" VkBaseInStructure = Ptr VkBaseInStructure
        type FieldOptional "pNext" VkBaseInStructure = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBaseInStructure =
             #{offset VkBaseInStructure, pNext}
        type FieldIsArray "pNext" VkBaseInStructure = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBaseInStructure, pNext}

instance {-# OVERLAPPING #-} CanReadField "pNext" VkBaseInStructure
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBaseInStructure, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBaseInStructure, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBaseInStructure where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBaseInStructure, pNext}

instance Show VkBaseInStructure where
        showsPrec d x
          = showString "VkBaseInStructure {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) . showChar '}'

-- | > typedef struct VkBaseOutStructure {
--   >     VkStructureType sType;
--   >     struct VkBaseOutStructure* pNext;
--   > } VkBaseOutStructure;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBaseOutStructure VkBaseOutStructure registry at www.khronos.org>
type VkBaseOutStructure = VulkanStruct VkBaseOutStructure' -- ' closing tick for hsc2hs

data VkBaseOutStructure' -- ' closing tick for hsc2hs

instance Eq VkBaseOutStructure where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkBaseOutStructure where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkBaseOutStructure where
        sizeOf ~_ = #{size VkBaseOutStructure}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBaseOutStructure}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkBaseOutStructure where
        type StructFields VkBaseOutStructure = '["sType", "pNext"] -- ' closing tick for hsc2hs
        type CUnionType VkBaseOutStructure = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBaseOutStructure = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBaseOutStructure = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkBaseOutStructure
         where
        type FieldType "sType" VkBaseOutStructure = VkStructureType
        type FieldOptional "sType" VkBaseOutStructure = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBaseOutStructure =
             #{offset VkBaseOutStructure, sType}
        type FieldIsArray "sType" VkBaseOutStructure = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBaseOutStructure, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBaseOutStructure where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBaseOutStructure, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBaseOutStructure, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBaseOutStructure where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBaseOutStructure, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkBaseOutStructure
         where
        type FieldType "pNext" VkBaseOutStructure = Ptr VkBaseOutStructure
        type FieldOptional "pNext" VkBaseOutStructure = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBaseOutStructure =
             #{offset VkBaseOutStructure, pNext}
        type FieldIsArray "pNext" VkBaseOutStructure = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBaseOutStructure, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBaseOutStructure where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBaseOutStructure, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBaseOutStructure, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBaseOutStructure where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBaseOutStructure, pNext}

instance Show VkBaseOutStructure where
        showsPrec d x
          = showString "VkBaseOutStructure {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) . showChar '}'
