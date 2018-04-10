#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryRequirements2
       (VkMemoryRequirements2(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Base                                          (Addr##,
                                                                    ByteArray##,
                                                                    byteArrayContents##,
                                                                    plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements (VkMemoryRequirements)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryRequirements2 {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkMemoryRequirements                                                 memoryRequirements;
--   > } VkMemoryRequirements2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkMemoryRequirements2 VkMemoryRequirements2 registry at www.khronos.org>
data VkMemoryRequirements2 = VkMemoryRequirements2## Addr##
                                                    ByteArray##

instance Eq VkMemoryRequirements2 where
        (VkMemoryRequirements2## a _) == x@(VkMemoryRequirements2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryRequirements2 where
        (VkMemoryRequirements2## a _) `compare`
          x@(VkMemoryRequirements2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryRequirements2 where
        sizeOf ~_ = #{size VkMemoryRequirements2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryRequirements2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryRequirements2 where
        unsafeAddr (VkMemoryRequirements2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryRequirements2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryRequirements2## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryRequirements2 where
        type StructFields VkMemoryRequirements2 =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryRequirements2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryRequirements2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryRequirements2
         where
        type FieldType "sType" VkMemoryRequirements2 = VkStructureType
        type FieldOptional "sType" VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryRequirements2 =
             #{offset VkMemoryRequirements2, sType}
        type FieldIsArray "sType" VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryRequirements2
         where
        type FieldType "pNext" VkMemoryRequirements2 = Ptr Void
        type FieldOptional "pNext" VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryRequirements2 =
             #{offset VkMemoryRequirements2, pNext}
        type FieldIsArray "pNext" VkMemoryRequirements2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkMemoryRequirements2 where
        type FieldType "memoryRequirements" VkMemoryRequirements2 =
             VkMemoryRequirements
        type FieldOptional "memoryRequirements" VkMemoryRequirements2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements" VkMemoryRequirements2 =
             #{offset VkMemoryRequirements2, memoryRequirements}
        type FieldIsArray "memoryRequirements" VkMemoryRequirements2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryRequirements2, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanReadField "memoryRequirements" VkMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2, memoryRequirements})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryRequirements" VkMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2, memoryRequirements}

instance Show VkMemoryRequirements2 where
        showsPrec d x
          = showString "VkMemoryRequirements2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryRequirements = " .
                            showsPrec d (getField @"memoryRequirements" x) . showChar '}'
