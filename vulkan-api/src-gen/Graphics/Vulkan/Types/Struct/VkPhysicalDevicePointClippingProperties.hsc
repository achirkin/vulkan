#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDevicePointClippingProperties
       (VkPhysicalDevicePointClippingProperties(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkPointClippingBehavior       (VkPointClippingBehavior)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDevicePointClippingProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPointClippingBehavior      pointClippingBehavior;
--   > } VkPhysicalDevicePointClippingProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDevicePointClippingProperties VkPhysicalDevicePointClippingProperties registry at www.khronos.org>
data VkPhysicalDevicePointClippingProperties = VkPhysicalDevicePointClippingProperties## Addr##
                                                                                        ByteArray##

instance Eq VkPhysicalDevicePointClippingProperties where
        (VkPhysicalDevicePointClippingProperties## a _) ==
          x@(VkPhysicalDevicePointClippingProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevicePointClippingProperties where
        (VkPhysicalDevicePointClippingProperties## a _) `compare`
          x@(VkPhysicalDevicePointClippingProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevicePointClippingProperties where
        sizeOf ~_
          = #{size VkPhysicalDevicePointClippingProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevicePointClippingProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDevicePointClippingProperties
         where
        unsafeAddr (VkPhysicalDevicePointClippingProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDevicePointClippingProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDevicePointClippingProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDevicePointClippingProperties
         where
        type StructFields VkPhysicalDevicePointClippingProperties =
             '["sType", "pNext", "pointClippingBehavior"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDevicePointClippingProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDevicePointClippingProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDevicePointClippingProperties =
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDevicePointClippingProperties where
        type FieldType "sType" VkPhysicalDevicePointClippingProperties =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDevicePointClippingProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDevicePointClippingProperties =
             #{offset VkPhysicalDevicePointClippingProperties, sType}
        type FieldIsArray "sType" VkPhysicalDevicePointClippingProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDevicePointClippingProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDevicePointClippingProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDevicePointClippingProperties where
        type FieldType "pNext" VkPhysicalDevicePointClippingProperties =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDevicePointClippingProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDevicePointClippingProperties =
             #{offset VkPhysicalDevicePointClippingProperties, pNext}
        type FieldIsArray "pNext" VkPhysicalDevicePointClippingProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDevicePointClippingProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDevicePointClippingProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "pointClippingBehavior"
           VkPhysicalDevicePointClippingProperties
         where
        type FieldType "pointClippingBehavior"
               VkPhysicalDevicePointClippingProperties
             = VkPointClippingBehavior
        type FieldOptional "pointClippingBehavior"
               VkPhysicalDevicePointClippingProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pointClippingBehavior"
               VkPhysicalDevicePointClippingProperties
             =
             #{offset VkPhysicalDevicePointClippingProperties, pointClippingBehavior}
        type FieldIsArray "pointClippingBehavior"
               VkPhysicalDevicePointClippingProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingProperties, pointClippingBehavior}

instance {-# OVERLAPPING #-}
         CanReadField "pointClippingBehavior"
           VkPhysicalDevicePointClippingProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingProperties, pointClippingBehavior})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingProperties, pointClippingBehavior}

instance {-# OVERLAPPING #-}
         CanWriteField "pointClippingBehavior"
           VkPhysicalDevicePointClippingProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingProperties, pointClippingBehavior}

instance Show VkPhysicalDevicePointClippingProperties where
        showsPrec d x
          = showString "VkPhysicalDevicePointClippingProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pointClippingBehavior = " .
                            showsPrec d (getField @"pointClippingBehavior" x) . showChar '}'
