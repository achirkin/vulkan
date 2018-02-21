#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupRenderPassBeginInfoKHX
       (VkDeviceGroupRenderPassBeginInfoKHX(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkRect2D              (VkRect2D)
import           Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo (VkRenderPassBeginInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupRenderPassBeginInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   >     uint32_t         deviceRenderAreaCount;
--   >     const VkRect2D*  pDeviceRenderAreas;
--   > } VkDeviceGroupRenderPassBeginInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupRenderPassBeginInfoKHX.html VkDeviceGroupRenderPassBeginInfoKHX registry at www.khronos.org>
data VkDeviceGroupRenderPassBeginInfoKHX = VkDeviceGroupRenderPassBeginInfoKHX## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupRenderPassBeginInfoKHX where
        (VkDeviceGroupRenderPassBeginInfoKHX## a _) ==
          x@(VkDeviceGroupRenderPassBeginInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupRenderPassBeginInfoKHX where
        (VkDeviceGroupRenderPassBeginInfoKHX## a _) `compare`
          x@(VkDeviceGroupRenderPassBeginInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupRenderPassBeginInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupRenderPassBeginInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupRenderPassBeginInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupRenderPassBeginInfoKHX
         where
        unsafeAddr (VkDeviceGroupRenderPassBeginInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupRenderPassBeginInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupRenderPassBeginInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupRenderPassBeginInfoKHX where
        type StructFields VkDeviceGroupRenderPassBeginInfoKHX =
             '["sType", "pNext", "deviceMask", "deviceRenderAreaCount", -- ' closing tick for hsc2hs
               "pDeviceRenderAreas"]
        type CUnionType VkDeviceGroupRenderPassBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupRenderPassBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupRenderPassBeginInfoKHX =
             '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupRenderPassBeginInfoKHX where
        type VkSTypeMType VkDeviceGroupRenderPassBeginInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupRenderPassBeginInfoKHX where
        type FieldType "sType" VkDeviceGroupRenderPassBeginInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupRenderPassBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupRenderPassBeginInfoKHX =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupRenderPassBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

instance CanReadField "sType" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupRenderPassBeginInfoKHX where
        type VkPNextMType VkDeviceGroupRenderPassBeginInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupRenderPassBeginInfoKHX where
        type FieldType "pNext" VkDeviceGroupRenderPassBeginInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupRenderPassBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupRenderPassBeginInfoKHX =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupRenderPassBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceMask VkDeviceGroupRenderPassBeginInfoKHX where
        type VkDeviceMaskMType VkDeviceGroupRenderPassBeginInfoKHX = Word32

        {-# NOINLINE vkDeviceMask #-}
        vkDeviceMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask})

        {-# INLINE vkDeviceMaskByteOffset #-}
        vkDeviceMaskByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

        {-# INLINE readVkDeviceMask #-}
        readVkDeviceMask p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

        {-# INLINE writeVkDeviceMask #-}
        writeVkDeviceMask p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX where
        type FieldType "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX =
             Word32
        type FieldOptional "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}
        type FieldIsArray "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

instance CanReadField "deviceMask"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceMask

        {-# INLINE readField #-}
        readField = readVkDeviceMask

instance CanWriteField "deviceMask"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceMask

instance {-# OVERLAPPING #-}
         HasVkDeviceRenderAreaCount VkDeviceGroupRenderPassBeginInfoKHX
         where
        type VkDeviceRenderAreaCountMType
               VkDeviceGroupRenderPassBeginInfoKHX
             = Word32

        {-# NOINLINE vkDeviceRenderAreaCount #-}
        vkDeviceRenderAreaCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount})

        {-# INLINE vkDeviceRenderAreaCountByteOffset #-}
        vkDeviceRenderAreaCountByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

        {-# INLINE readVkDeviceRenderAreaCount #-}
        readVkDeviceRenderAreaCount p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

        {-# INLINE writeVkDeviceRenderAreaCount #-}
        writeVkDeviceRenderAreaCount p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         HasField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        type FieldType "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfoKHX
             = Word32
        type FieldOptional "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfoKHX
             =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}
        type FieldIsArray "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

instance CanReadField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceRenderAreaCount

        {-# INLINE readField #-}
        readField = readVkDeviceRenderAreaCount

instance CanWriteField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceRenderAreaCount

instance {-# OVERLAPPING #-}
         HasVkPDeviceRenderAreas VkDeviceGroupRenderPassBeginInfoKHX where
        type VkPDeviceRenderAreasMType VkDeviceGroupRenderPassBeginInfoKHX
             = Ptr VkRect2D

        {-# NOINLINE vkPDeviceRenderAreas #-}
        vkPDeviceRenderAreas x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas})

        {-# INLINE vkPDeviceRenderAreasByteOffset #-}
        vkPDeviceRenderAreasByteOffset ~_
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

        {-# INLINE readVkPDeviceRenderAreas #-}
        readVkPDeviceRenderAreas p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

        {-# INLINE writeVkPDeviceRenderAreas #-}
        writeVkPDeviceRenderAreas p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

instance {-# OVERLAPPING #-}
         HasField "pDeviceRenderAreas" VkDeviceGroupRenderPassBeginInfoKHX
         where
        type FieldType "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfoKHX
             = Ptr VkRect2D
        type FieldOptional "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfoKHX
             =
             #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}
        type FieldIsArray "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

instance CanReadField "pDeviceRenderAreas"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPDeviceRenderAreas

        {-# INLINE readField #-}
        readField = readVkPDeviceRenderAreas

instance CanWriteField "pDeviceRenderAreas"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDeviceRenderAreas

instance Show VkDeviceGroupRenderPassBeginInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupRenderPassBeginInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceMask = " .
                            showsPrec d (vkDeviceMask x) .
                              showString ", " .
                                showString "vkDeviceRenderAreaCount = " .
                                  showsPrec d (vkDeviceRenderAreaCount x) .
                                    showString ", " .
                                      showString "vkPDeviceRenderAreas = " .
                                        showsPrec d (vkPDeviceRenderAreas x) . showChar '}'
