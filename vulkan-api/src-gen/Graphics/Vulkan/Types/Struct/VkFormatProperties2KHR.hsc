#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkFormatProperties2KHR
       (VkFormatProperties2KHR(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkFormatProperties (VkFormatProperties)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkFormatProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkFormatProperties               formatProperties;
--   > } VkFormatProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkFormatProperties2KHR.html VkFormatProperties2KHR registry at www.khronos.org>
data VkFormatProperties2KHR = VkFormatProperties2KHR## Addr##
                                                      ByteArray##

instance Eq VkFormatProperties2KHR where
        (VkFormatProperties2KHR## a _) == x@(VkFormatProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFormatProperties2KHR where
        (VkFormatProperties2KHR## a _) `compare`
          x@(VkFormatProperties2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFormatProperties2KHR where
        sizeOf ~_ = #{size VkFormatProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFormatProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFormatProperties2KHR where
        unsafeAddr (VkFormatProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFormatProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFormatProperties2KHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFormatProperties2KHR where
        type StructFields VkFormatProperties2KHR =
             '["sType", "pNext", "formatProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkFormatProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFormatProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkFormatProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkFormatProperties2KHR where
        type FieldType "sType" VkFormatProperties2KHR = VkStructureType
        type FieldOptional "sType" VkFormatProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFormatProperties2KHR =
             #{offset VkFormatProperties2KHR, sType}
        type FieldIsArray "sType" VkFormatProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkFormatProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkFormatProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkFormatProperties2KHR where
        type FieldType "pNext" VkFormatProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkFormatProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFormatProperties2KHR =
             #{offset VkFormatProperties2KHR, pNext}
        type FieldIsArray "pNext" VkFormatProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkFormatProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkFormatProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "formatProperties" VkFormatProperties2KHR where
        type FieldType "formatProperties" VkFormatProperties2KHR =
             VkFormatProperties
        type FieldOptional "formatProperties" VkFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "formatProperties" VkFormatProperties2KHR =
             #{offset VkFormatProperties2KHR, formatProperties}
        type FieldIsArray "formatProperties" VkFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFormatProperties2KHR, formatProperties}

instance {-# OVERLAPPING #-}
         CanReadField "formatProperties" VkFormatProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2KHR, formatProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2KHR, formatProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "formatProperties" VkFormatProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2KHR, formatProperties}

instance Show VkFormatProperties2KHR where
        showsPrec d x
          = showString "VkFormatProperties2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "formatProperties = " .
                            showsPrec d (getField @"formatProperties" x) . showChar '}'
