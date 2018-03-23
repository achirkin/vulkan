#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageFormatListCreateInfoKHR
       (VkImageFormatListCreateInfoKHR(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat            (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo (VkImageCreateInfo)
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkImageFormatListCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               viewFormatCount;
--   >     const VkFormat*      pViewFormats;
--   > } VkImageFormatListCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkImageFormatListCreateInfoKHR.html VkImageFormatListCreateInfoKHR registry at www.khronos.org>
data VkImageFormatListCreateInfoKHR = VkImageFormatListCreateInfoKHR## Addr##
                                                                      ByteArray##

instance Eq VkImageFormatListCreateInfoKHR where
        (VkImageFormatListCreateInfoKHR## a _) ==
          x@(VkImageFormatListCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatListCreateInfoKHR where
        (VkImageFormatListCreateInfoKHR## a _) `compare`
          x@(VkImageFormatListCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageFormatListCreateInfoKHR where
        sizeOf ~_ = #{size VkImageFormatListCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageFormatListCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageFormatListCreateInfoKHR where
        unsafeAddr (VkImageFormatListCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageFormatListCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageFormatListCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageFormatListCreateInfoKHR where
        type StructFields VkImageFormatListCreateInfoKHR =
             '["sType", "pNext", "viewFormatCount", "pViewFormats"] -- ' closing tick for hsc2hs
        type CUnionType VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageFormatListCreateInfoKHR =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageFormatListCreateInfoKHR where
        type FieldType "sType" VkImageFormatListCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, sType}
        type FieldIsArray "sType" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageFormatListCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageFormatListCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageFormatListCreateInfoKHR where
        type FieldType "pNext" VkImageFormatListCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkImageFormatListCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageFormatListCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageFormatListCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "viewFormatCount" VkImageFormatListCreateInfoKHR where
        type FieldType "viewFormatCount" VkImageFormatListCreateInfoKHR =
             Word32
        type FieldOptional "viewFormatCount" VkImageFormatListCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "viewFormatCount" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}
        type FieldIsArray "viewFormatCount" VkImageFormatListCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

instance {-# OVERLAPPING #-}
         CanReadField "viewFormatCount" VkImageFormatListCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, viewFormatCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

instance {-# OVERLAPPING #-}
         CanWriteField "viewFormatCount" VkImageFormatListCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, viewFormatCount}

instance {-# OVERLAPPING #-}
         HasField "pViewFormats" VkImageFormatListCreateInfoKHR where
        type FieldType "pViewFormats" VkImageFormatListCreateInfoKHR =
             Ptr VkFormat
        type FieldOptional "pViewFormats" VkImageFormatListCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewFormats" VkImageFormatListCreateInfoKHR =
             #{offset VkImageFormatListCreateInfoKHR, pViewFormats}
        type FieldIsArray "pViewFormats" VkImageFormatListCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

instance {-# OVERLAPPING #-}
         CanReadField "pViewFormats" VkImageFormatListCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatListCreateInfoKHR, pViewFormats})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewFormats" VkImageFormatListCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatListCreateInfoKHR, pViewFormats}

instance Show VkImageFormatListCreateInfoKHR where
        showsPrec d x
          = showString "VkImageFormatListCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "viewFormatCount = " .
                            showsPrec d (getField @"viewFormatCount" x) .
                              showString ", " .
                                showString "pViewFormats = " .
                                  showsPrec d (getField @"pViewFormats" x) . showChar '}'
