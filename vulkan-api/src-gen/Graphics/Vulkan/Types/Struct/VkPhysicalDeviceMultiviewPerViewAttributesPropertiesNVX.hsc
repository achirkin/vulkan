#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
       (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         perViewPositionAllComponents;
--   > } VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX.html VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX registry at www.khronos.org>
data VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## Addr##
                                                                                                                        ByteArray##

instance Eq VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _) ==
          x@(VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _)
          `compare`
          x@(VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        unsafeAddr
          (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type StructFields
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = '["sType", "pNext", "perViewPositionAllComponents"] -- ' closing tick for hsc2hs
        type CUnionType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'True -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type VkSTypeMType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

instance CanReadField "sType"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type VkPNextMType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPerViewPositionAllComponents
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type VkPerViewPositionAllComponentsMType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkBool32

        {-# NOINLINE vkPerViewPositionAllComponents #-}
        vkPerViewPositionAllComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents})

        {-# INLINE vkPerViewPositionAllComponentsByteOffset #-}
        vkPerViewPositionAllComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

        {-# INLINE readVkPerViewPositionAllComponents #-}
        readVkPerViewPositionAllComponents p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

        {-# INLINE writeVkPerViewPositionAllComponents #-}
        writeVkPerViewPositionAllComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

instance {-# OVERLAPPING #-}
         HasField "perViewPositionAllComponents"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkBool32
        type FieldOptional "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}
        type FieldIsArray "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

instance CanReadField "perViewPositionAllComponents"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE getField #-}
        getField = vkPerViewPositionAllComponents

        {-# INLINE readField #-}
        readField = readVkPerViewPositionAllComponents

instance CanWriteField "perViewPositionAllComponents"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPerViewPositionAllComponents

instance Show
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        showsPrec d x
          = showString
              "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPerViewPositionAllComponents = " .
                            showsPrec d (vkPerViewPositionAllComponents x) . showChar '}'
