#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryRequirements2KHR
       (VkMemoryRequirements2KHR(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements (VkMemoryRequirements)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryRequirements2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkMemoryRequirements                                                 memoryRequirements;
--   > } VkMemoryRequirements2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryRequirements2KHR.html VkMemoryRequirements2KHR registry at www.khronos.org>
data VkMemoryRequirements2KHR = VkMemoryRequirements2KHR## Addr##
                                                          ByteArray##

instance Eq VkMemoryRequirements2KHR where
        (VkMemoryRequirements2KHR## a _) ==
          x@(VkMemoryRequirements2KHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryRequirements2KHR where
        (VkMemoryRequirements2KHR## a _) `compare`
          x@(VkMemoryRequirements2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryRequirements2KHR where
        sizeOf ~_ = #{size VkMemoryRequirements2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryRequirements2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryRequirements2KHR where
        unsafeAddr (VkMemoryRequirements2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryRequirements2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryRequirements2KHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryRequirements2KHR where
        type StructFields VkMemoryRequirements2KHR =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryRequirements2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryRequirements2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryRequirements2KHR where
        type FieldType "sType" VkMemoryRequirements2KHR = VkStructureType
        type FieldOptional "sType" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, sType}
        type FieldIsArray "sType" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryRequirements2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryRequirements2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryRequirements2KHR where
        type FieldType "pNext" VkMemoryRequirements2KHR = Ptr Void
        type FieldOptional "pNext" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, pNext}
        type FieldIsArray "pNext" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryRequirements2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryRequirements2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkMemoryRequirements2KHR where
        type FieldType "memoryRequirements" VkMemoryRequirements2KHR =
             VkMemoryRequirements
        type FieldOptional "memoryRequirements" VkMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, memoryRequirements}
        type FieldIsArray "memoryRequirements" VkMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryRequirements2KHR, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanReadField "memoryRequirements" VkMemoryRequirements2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, memoryRequirements})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryRequirements" VkMemoryRequirements2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, memoryRequirements}

instance Show VkMemoryRequirements2KHR where
        showsPrec d x
          = showString "VkMemoryRequirements2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryRequirements = " .
                            showsPrec d (getField @"memoryRequirements" x) . showChar '}'
