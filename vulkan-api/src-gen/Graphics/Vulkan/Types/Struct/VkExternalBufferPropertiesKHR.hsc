#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalBufferPropertiesKHR
       (VkExternalBufferPropertiesKHR(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR (VkExternalMemoryPropertiesKHR)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkExternalBufferPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryPropertiesKHR    externalMemoryProperties;
--   > } VkExternalBufferPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalBufferPropertiesKHR.html VkExternalBufferPropertiesKHR registry at www.khronos.org>
data VkExternalBufferPropertiesKHR = VkExternalBufferPropertiesKHR## Addr##
                                                                    ByteArray##

instance Eq VkExternalBufferPropertiesKHR where
        (VkExternalBufferPropertiesKHR## a _) ==
          x@(VkExternalBufferPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalBufferPropertiesKHR where
        (VkExternalBufferPropertiesKHR## a _) `compare`
          x@(VkExternalBufferPropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalBufferPropertiesKHR where
        sizeOf ~_ = #{size VkExternalBufferPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalBufferPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalBufferPropertiesKHR where
        unsafeAddr (VkExternalBufferPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalBufferPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalBufferPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalBufferPropertiesKHR where
        type StructFields VkExternalBufferPropertiesKHR =
             '["sType", "pNext", "externalMemoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalBufferPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalBufferPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalBufferPropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalBufferPropertiesKHR where
        type FieldType "sType" VkExternalBufferPropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalBufferPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalBufferPropertiesKHR =
             #{offset VkExternalBufferPropertiesKHR, sType}
        type FieldIsArray "sType" VkExternalBufferPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalBufferPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferPropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalBufferPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalBufferPropertiesKHR where
        type FieldType "pNext" VkExternalBufferPropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkExternalBufferPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalBufferPropertiesKHR =
             #{offset VkExternalBufferPropertiesKHR, pNext}
        type FieldIsArray "pNext" VkExternalBufferPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalBufferPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferPropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalBufferPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties" VkExternalBufferPropertiesKHR
         where
        type FieldType "externalMemoryProperties"
               VkExternalBufferPropertiesKHR
             = VkExternalMemoryPropertiesKHR
        type FieldOptional "externalMemoryProperties"
               VkExternalBufferPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryProperties"
               VkExternalBufferPropertiesKHR
             =
             #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties}
        type FieldIsArray "externalMemoryProperties"
               VkExternalBufferPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryProperties"
           VkExternalBufferPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryProperties"
           VkExternalBufferPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferPropertiesKHR, externalMemoryProperties}

instance Show VkExternalBufferPropertiesKHR where
        showsPrec d x
          = showString "VkExternalBufferPropertiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "externalMemoryProperties = " .
                            showsPrec d (getField @"externalMemoryProperties" x) . showChar '}'
