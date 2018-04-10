#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewProperties
       (VkPhysicalDeviceMultiviewProperties(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMultiviewProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxMultiviewViewCount;
--   >     uint32_t                         maxMultiviewInstanceIndex;
--   > } VkPhysicalDeviceMultiviewProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceMultiviewProperties VkPhysicalDeviceMultiviewProperties registry at www.khronos.org>
data VkPhysicalDeviceMultiviewProperties = VkPhysicalDeviceMultiviewProperties## Addr##
                                                                                ByteArray##

instance Eq VkPhysicalDeviceMultiviewProperties where
        (VkPhysicalDeviceMultiviewProperties## a _) ==
          x@(VkPhysicalDeviceMultiviewProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewProperties where
        (VkPhysicalDeviceMultiviewProperties## a _) `compare`
          x@(VkPhysicalDeviceMultiviewProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewProperties where
        sizeOf ~_ = #{size VkPhysicalDeviceMultiviewProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMultiviewProperties
         where
        unsafeAddr (VkPhysicalDeviceMultiviewProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMultiviewProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewProperties where
        type StructFields VkPhysicalDeviceMultiviewProperties =
             '["sType", "pNext", "maxMultiviewViewCount", -- ' closing tick for hsc2hs
               "maxMultiviewInstanceIndex"]
        type CUnionType VkPhysicalDeviceMultiviewProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMultiviewProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMultiviewProperties =
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMultiviewProperties where
        type FieldType "sType" VkPhysicalDeviceMultiviewProperties =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMultiviewProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMultiviewProperties =
             #{offset VkPhysicalDeviceMultiviewProperties, sType}
        type FieldIsArray "sType" VkPhysicalDeviceMultiviewProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceMultiviewProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceMultiviewProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMultiviewProperties where
        type FieldType "pNext" VkPhysicalDeviceMultiviewProperties =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMultiviewProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMultiviewProperties =
             #{offset VkPhysicalDeviceMultiviewProperties, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceMultiviewProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceMultiviewProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceMultiviewProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewProperties
         where
        type FieldType "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewProperties
             = Word32
        type FieldOptional "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewProperties
             =
             #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewViewCount}
        type FieldIsArray "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewViewCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewViewCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewViewCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewViewCount}

instance {-# OVERLAPPING #-}
         HasField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewProperties
         where
        type FieldType "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewProperties
             = Word32
        type FieldOptional "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewProperties
             =
             #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewInstanceIndex}
        type FieldIsArray "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewInstanceIndex}

instance {-# OVERLAPPING #-}
         CanReadField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewInstanceIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewInstanceIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewInstanceIndex}

instance Show VkPhysicalDeviceMultiviewProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceMultiviewProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxMultiviewViewCount = " .
                            showsPrec d (getField @"maxMultiviewViewCount" x) .
                              showString ", " .
                                showString "maxMultiviewInstanceIndex = " .
                                  showsPrec d (getField @"maxMultiviewInstanceIndex" x) .
                                    showChar '}'
