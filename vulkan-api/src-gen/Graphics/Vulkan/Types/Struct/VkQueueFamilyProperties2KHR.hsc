#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2KHR
       (VkQueueFamilyProperties2KHR(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties (VkQueueFamilyProperties)
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkQueueFamilyProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkQueueFamilyProperties          queueFamilyProperties;
--   > } VkQueueFamilyProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkQueueFamilyProperties2KHR.html VkQueueFamilyProperties2KHR registry at www.khronos.org>
data VkQueueFamilyProperties2KHR = VkQueueFamilyProperties2KHR## Addr##
                                                                ByteArray##

instance Eq VkQueueFamilyProperties2KHR where
        (VkQueueFamilyProperties2KHR## a _) ==
          x@(VkQueueFamilyProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkQueueFamilyProperties2KHR where
        (VkQueueFamilyProperties2KHR## a _) `compare`
          x@(VkQueueFamilyProperties2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkQueueFamilyProperties2KHR where
        sizeOf ~_ = #{size VkQueueFamilyProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkQueueFamilyProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkQueueFamilyProperties2KHR where
        unsafeAddr (VkQueueFamilyProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkQueueFamilyProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkQueueFamilyProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkQueueFamilyProperties2KHR where
        type StructFields VkQueueFamilyProperties2KHR =
             '["sType", "pNext", "queueFamilyProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkQueueFamilyProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkQueueFamilyProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkQueueFamilyProperties2KHR where
        type FieldType "sType" VkQueueFamilyProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkQueueFamilyProperties2KHR =
             #{offset VkQueueFamilyProperties2KHR, sType}
        type FieldIsArray "sType" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkQueueFamilyProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkQueueFamilyProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkQueueFamilyProperties2KHR where
        type FieldType "pNext" VkQueueFamilyProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkQueueFamilyProperties2KHR =
             #{offset VkQueueFamilyProperties2KHR, pNext}
        type FieldIsArray "pNext" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkQueueFamilyProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkQueueFamilyProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyProperties" VkQueueFamilyProperties2KHR where
        type FieldType "queueFamilyProperties" VkQueueFamilyProperties2KHR
             = VkQueueFamilyProperties
        type FieldOptional "queueFamilyProperties"
               VkQueueFamilyProperties2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyProperties"
               VkQueueFamilyProperties2KHR
             =
             #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}
        type FieldIsArray "queueFamilyProperties"
               VkQueueFamilyProperties2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyProperties" VkQueueFamilyProperties2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyProperties" VkQueueFamilyProperties2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

instance Show VkQueueFamilyProperties2KHR where
        showsPrec d x
          = showString "VkQueueFamilyProperties2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "queueFamilyProperties = " .
                            showsPrec d (getField @"queueFamilyProperties" x) . showChar '}'
