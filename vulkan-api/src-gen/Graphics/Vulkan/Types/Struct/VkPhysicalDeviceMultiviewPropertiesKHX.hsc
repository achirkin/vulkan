#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewPropertiesKHX
       (VkPhysicalDeviceMultiviewPropertiesKHX(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMultiviewPropertiesKHX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxMultiviewViewCount;
--   >     uint32_t                         maxMultiviewInstanceIndex;
--   > } VkPhysicalDeviceMultiviewPropertiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceMultiviewPropertiesKHX.html VkPhysicalDeviceMultiviewPropertiesKHX registry at www.khronos.org>
data VkPhysicalDeviceMultiviewPropertiesKHX = VkPhysicalDeviceMultiviewPropertiesKHX## Addr##
                                                                                      ByteArray##

instance Eq VkPhysicalDeviceMultiviewPropertiesKHX where
        (VkPhysicalDeviceMultiviewPropertiesKHX## a _) ==
          x@(VkPhysicalDeviceMultiviewPropertiesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewPropertiesKHX where
        (VkPhysicalDeviceMultiviewPropertiesKHX## a _) `compare`
          x@(VkPhysicalDeviceMultiviewPropertiesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewPropertiesKHX where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewPropertiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewPropertiesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMultiviewPropertiesKHX
         where
        unsafeAddr (VkPhysicalDeviceMultiviewPropertiesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMultiviewPropertiesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewPropertiesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewPropertiesKHX where
        type StructFields VkPhysicalDeviceMultiviewPropertiesKHX =
             '["sType", "pNext", "maxMultiviewViewCount", -- ' closing tick for hsc2hs
               "maxMultiviewInstanceIndex"]
        type CUnionType VkPhysicalDeviceMultiviewPropertiesKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMultiviewPropertiesKHX = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMultiviewPropertiesKHX =
             '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceMultiviewPropertiesKHX where
        type VkSTypeMType VkPhysicalDeviceMultiviewPropertiesKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMultiviewPropertiesKHX where
        type FieldType "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}
        type FieldIsArray "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

instance CanReadField "sType"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceMultiviewPropertiesKHX where
        type VkPNextMType VkPhysicalDeviceMultiviewPropertiesKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMultiviewPropertiesKHX where
        type FieldType "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMaxMultiviewViewCount VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type VkMaxMultiviewViewCountMType
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32

        {-# NOINLINE vkMaxMultiviewViewCount #-}
        vkMaxMultiviewViewCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount})

        {-# INLINE vkMaxMultiviewViewCountByteOffset #-}
        vkMaxMultiviewViewCountByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

        {-# INLINE readVkMaxMultiviewViewCount #-}
        readVkMaxMultiviewViewCount p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

        {-# INLINE writeVkMaxMultiviewViewCount #-}
        writeVkMaxMultiviewViewCount p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

instance {-# OVERLAPPING #-}
         HasField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type FieldType "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32
        type FieldOptional "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}
        type FieldIsArray "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

instance CanReadField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkMaxMultiviewViewCount

        {-# INLINE readField #-}
        readField = readVkMaxMultiviewViewCount

instance CanWriteField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxMultiviewViewCount

instance {-# OVERLAPPING #-}
         HasVkMaxMultiviewInstanceIndex
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type VkMaxMultiviewInstanceIndexMType
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32

        {-# NOINLINE vkMaxMultiviewInstanceIndex #-}
        vkMaxMultiviewInstanceIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex})

        {-# INLINE vkMaxMultiviewInstanceIndexByteOffset #-}
        vkMaxMultiviewInstanceIndexByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

        {-# INLINE readVkMaxMultiviewInstanceIndex #-}
        readVkMaxMultiviewInstanceIndex p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

        {-# INLINE writeVkMaxMultiviewInstanceIndex #-}
        writeVkMaxMultiviewInstanceIndex p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

instance {-# OVERLAPPING #-}
         HasField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type FieldType "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32
        type FieldOptional "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}
        type FieldIsArray "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

instance CanReadField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkMaxMultiviewInstanceIndex

        {-# INLINE readField #-}
        readField = readVkMaxMultiviewInstanceIndex

instance CanWriteField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxMultiviewInstanceIndex

instance Show VkPhysicalDeviceMultiviewPropertiesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceMultiviewPropertiesKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxMultiviewViewCount = " .
                            showsPrec d (vkMaxMultiviewViewCount x) .
                              showString ", " .
                                showString "vkMaxMultiviewInstanceIndex = " .
                                  showsPrec d (vkMaxMultiviewInstanceIndex x) . showChar '}'
