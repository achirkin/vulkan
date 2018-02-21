#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImageMemoryDeviceGroupInfoKHX
       (VkBindImageMemoryDeviceGroupInfoKHX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR (VkBindImageMemoryInfoKHR)
import           Graphics.Vulkan.Types.Struct.VkRect2D                 (VkRect2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkBindImageMemoryDeviceGroupInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   >     uint32_t         SFRRectCount;
--   >     const VkRect2D*  pSFRRects;
--   > } VkBindImageMemoryDeviceGroupInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindImageMemoryDeviceGroupInfoKHX.html VkBindImageMemoryDeviceGroupInfoKHX registry at www.khronos.org>
data VkBindImageMemoryDeviceGroupInfoKHX = VkBindImageMemoryDeviceGroupInfoKHX## Addr##
                                                                                ByteArray##

instance Eq VkBindImageMemoryDeviceGroupInfoKHX where
        (VkBindImageMemoryDeviceGroupInfoKHX## a _) ==
          x@(VkBindImageMemoryDeviceGroupInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryDeviceGroupInfoKHX where
        (VkBindImageMemoryDeviceGroupInfoKHX## a _) `compare`
          x@(VkBindImageMemoryDeviceGroupInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryDeviceGroupInfoKHX where
        sizeOf ~_ = #{size VkBindImageMemoryDeviceGroupInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImageMemoryDeviceGroupInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemoryDeviceGroupInfoKHX
         where
        unsafeAddr (VkBindImageMemoryDeviceGroupInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemoryDeviceGroupInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemoryDeviceGroupInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemoryDeviceGroupInfoKHX where
        type StructFields VkBindImageMemoryDeviceGroupInfoKHX =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices", -- ' closing tick for hsc2hs
               "SFRRectCount", "pSFRRects"]
        type CUnionType VkBindImageMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemoryDeviceGroupInfoKHX =
             '[VkBindImageMemoryInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkBindImageMemoryDeviceGroupInfoKHX where
        type VkSTypeMType VkBindImageMemoryDeviceGroupInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "sType" VkBindImageMemoryDeviceGroupInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemoryDeviceGroupInfoKHX =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}
        type FieldIsArray "sType" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

instance CanReadField "sType" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkBindImageMemoryDeviceGroupInfoKHX where
        type VkPNextMType VkBindImageMemoryDeviceGroupInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "pNext" VkBindImageMemoryDeviceGroupInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemoryDeviceGroupInfoKHX =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}
        type FieldIsArray "pNext" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

instance CanReadField "pNext" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceIndexCount VkBindImageMemoryDeviceGroupInfoKHX where
        type VkDeviceIndexCountMType VkBindImageMemoryDeviceGroupInfoKHX =
             Word32

        {-# NOINLINE vkDeviceIndexCount #-}
        vkDeviceIndexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount})

        {-# INLINE vkDeviceIndexCountByteOffset #-}
        vkDeviceIndexCountByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

        {-# INLINE readVkDeviceIndexCount #-}
        readVkDeviceIndexCount p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

        {-# INLINE writeVkDeviceIndexCount #-}
        writeVkDeviceIndexCount p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfoKHX
         where
        type FieldType "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance CanReadField "deviceIndexCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceIndexCount

        {-# INLINE readField #-}
        readField = readVkDeviceIndexCount

instance CanWriteField "deviceIndexCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceIndexCount

instance {-# OVERLAPPING #-}
         HasVkPDeviceIndices VkBindImageMemoryDeviceGroupInfoKHX where
        type VkPDeviceIndicesMType VkBindImageMemoryDeviceGroupInfoKHX =
             Ptr Word32

        {-# NOINLINE vkPDeviceIndices #-}
        vkPDeviceIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices})

        {-# INLINE vkPDeviceIndicesByteOffset #-}
        vkPDeviceIndicesByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

        {-# INLINE readVkPDeviceIndices #-}
        readVkPDeviceIndices p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

        {-# INLINE writeVkPDeviceIndices #-}
        writeVkPDeviceIndices p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "pDeviceIndices" VkBindImageMemoryDeviceGroupInfoKHX
             = Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices"
               VkBindImageMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}
        type FieldIsArray "pDeviceIndices"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance CanReadField "pDeviceIndices"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPDeviceIndices

        {-# INLINE readField #-}
        readField = readVkPDeviceIndices

instance CanWriteField "pDeviceIndices"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDeviceIndices

instance {-# OVERLAPPING #-}
         HasVkSFRRectCount VkBindImageMemoryDeviceGroupInfoKHX where
        type VkSFRRectCountMType VkBindImageMemoryDeviceGroupInfoKHX =
             Word32

        {-# NOINLINE vkSFRRectCount #-}
        vkSFRRectCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount})

        {-# INLINE vkSFRRectCountByteOffset #-}
        vkSFRRectCountByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

        {-# INLINE readVkSFRRectCount #-}
        readVkSFRRectCount p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

        {-# INLINE writeVkSFRRectCount #-}
        writeVkSFRRectCount p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

instance {-# OVERLAPPING #-}
         HasField "SFRRectCount" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "SFRRectCount" VkBindImageMemoryDeviceGroupInfoKHX =
             Word32
        type FieldOptional "SFRRectCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "SFRRectCount" VkBindImageMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}
        type FieldIsArray "SFRRectCount"
               VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

instance CanReadField "SFRRectCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSFRRectCount

        {-# INLINE readField #-}
        readField = readVkSFRRectCount

instance CanWriteField "SFRRectCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSFRRectCount

instance {-# OVERLAPPING #-}
         HasVkPSFRRects VkBindImageMemoryDeviceGroupInfoKHX where
        type VkPSFRRectsMType VkBindImageMemoryDeviceGroupInfoKHX =
             Ptr VkRect2D

        {-# NOINLINE vkPSFRRects #-}
        vkPSFRRects x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects})

        {-# INLINE vkPSFRRectsByteOffset #-}
        vkPSFRRectsByteOffset ~_
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

        {-# INLINE readVkPSFRRects #-}
        readVkPSFRRects p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

        {-# INLINE writeVkPSFRRects #-}
        writeVkPSFRRects p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

instance {-# OVERLAPPING #-}
         HasField "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX where
        type FieldType "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX =
             Ptr VkRect2D
        type FieldOptional "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX =
             #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}
        type FieldIsArray "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

instance CanReadField "pSFRRects"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPSFRRects

        {-# INLINE readField #-}
        readField = readVkPSFRRects

instance CanWriteField "pSFRRects"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSFRRects

instance Show VkBindImageMemoryDeviceGroupInfoKHX where
        showsPrec d x
          = showString "VkBindImageMemoryDeviceGroupInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceIndexCount = " .
                            showsPrec d (vkDeviceIndexCount x) .
                              showString ", " .
                                showString "vkPDeviceIndices = " .
                                  showsPrec d (vkPDeviceIndices x) .
                                    showString ", " .
                                      showString "vkSFRRectCount = " .
                                        showsPrec d (vkSFRRectCount x) .
                                          showString ", " .
                                            showString "vkPSFRRects = " .
                                              showsPrec d (vkPSFRRects x) . showChar '}'
