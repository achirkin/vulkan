#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR
       (VkPhysicalDeviceFeatures2KHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo       (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceFeatures2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceFeatures         features;
--   > } VkPhysicalDeviceFeatures2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceFeatures2KHR.html VkPhysicalDeviceFeatures2KHR registry at www.khronos.org>
data VkPhysicalDeviceFeatures2KHR = VkPhysicalDeviceFeatures2KHR## Addr##
                                                                  ByteArray##

instance Eq VkPhysicalDeviceFeatures2KHR where
        (VkPhysicalDeviceFeatures2KHR## a _) ==
          x@(VkPhysicalDeviceFeatures2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceFeatures2KHR where
        (VkPhysicalDeviceFeatures2KHR## a _) `compare`
          x@(VkPhysicalDeviceFeatures2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceFeatures2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceFeatures2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceFeatures2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceFeatures2KHR where
        unsafeAddr (VkPhysicalDeviceFeatures2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceFeatures2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceFeatures2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceFeatures2KHR where
        type StructFields VkPhysicalDeviceFeatures2KHR =
             '["sType", "pNext", "features"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceFeatures2KHR =
             '[VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceFeatures2KHR where
        type VkSTypeMType VkPhysicalDeviceFeatures2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceFeatures2KHR where
        type FieldType "sType" VkPhysicalDeviceFeatures2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceFeatures2KHR =
             #{offset VkPhysicalDeviceFeatures2KHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures2KHR, sType}

instance CanReadField "sType" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceFeatures2KHR where
        type VkPNextMType VkPhysicalDeviceFeatures2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceFeatures2KHR where
        type FieldType "pNext" VkPhysicalDeviceFeatures2KHR = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceFeatures2KHR =
             #{offset VkPhysicalDeviceFeatures2KHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures2KHR, pNext}

instance CanReadField "pNext" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFeatures VkPhysicalDeviceFeatures2KHR where
        type VkFeaturesMType VkPhysicalDeviceFeatures2KHR =
             VkPhysicalDeviceFeatures

        {-# NOINLINE vkFeatures #-}
        vkFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2KHR, features})

        {-# INLINE vkFeaturesByteOffset #-}
        vkFeaturesByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures2KHR, features}

        {-# INLINE readVkFeatures #-}
        readVkFeatures p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2KHR, features}

        {-# INLINE writeVkFeatures #-}
        writeVkFeatures p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2KHR, features}

instance {-# OVERLAPPING #-}
         HasField "features" VkPhysicalDeviceFeatures2KHR where
        type FieldType "features" VkPhysicalDeviceFeatures2KHR =
             VkPhysicalDeviceFeatures
        type FieldOptional "features" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "features" VkPhysicalDeviceFeatures2KHR =
             #{offset VkPhysicalDeviceFeatures2KHR, features}
        type FieldIsArray "features" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures2KHR, features}

instance CanReadField "features" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE getField #-}
        getField = vkFeatures

        {-# INLINE readField #-}
        readField = readVkFeatures

instance CanWriteField "features" VkPhysicalDeviceFeatures2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFeatures

instance Show VkPhysicalDeviceFeatures2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceFeatures2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFeatures = " .
                            showsPrec d (vkFeatures x) . showChar '}'
