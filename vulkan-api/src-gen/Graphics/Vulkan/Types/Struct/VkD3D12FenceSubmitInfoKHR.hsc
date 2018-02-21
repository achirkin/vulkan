#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkD3D12FenceSubmitInfoKHR
       (VkD3D12FenceSubmitInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo  (VkSubmitInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkD3D12FenceSubmitInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreValuesCount;
--   >     const uint64_t* pWaitSemaphoreValues;
--   >     uint32_t         signalSemaphoreValuesCount;
--   >     const uint64_t* pSignalSemaphoreValues;
--   > } VkD3D12FenceSubmitInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkD3D12FenceSubmitInfoKHR.html VkD3D12FenceSubmitInfoKHR registry at www.khronos.org>
data VkD3D12FenceSubmitInfoKHR = VkD3D12FenceSubmitInfoKHR## Addr##
                                                            ByteArray##

instance Eq VkD3D12FenceSubmitInfoKHR where
        (VkD3D12FenceSubmitInfoKHR## a _) ==
          x@(VkD3D12FenceSubmitInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkD3D12FenceSubmitInfoKHR where
        (VkD3D12FenceSubmitInfoKHR## a _) `compare`
          x@(VkD3D12FenceSubmitInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkD3D12FenceSubmitInfoKHR where
        sizeOf ~_ = #{size VkD3D12FenceSubmitInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkD3D12FenceSubmitInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkD3D12FenceSubmitInfoKHR where
        unsafeAddr (VkD3D12FenceSubmitInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkD3D12FenceSubmitInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkD3D12FenceSubmitInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkD3D12FenceSubmitInfoKHR where
        type StructFields VkD3D12FenceSubmitInfoKHR =
             '["sType", "pNext", "waitSemaphoreValuesCount", -- ' closing tick for hsc2hs
               "pWaitSemaphoreValues", "signalSemaphoreValuesCount",
               "pSignalSemaphoreValues"]
        type CUnionType VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkD3D12FenceSubmitInfoKHR = '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkD3D12FenceSubmitInfoKHR
         where
        type VkSTypeMType VkD3D12FenceSubmitInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkD3D12FenceSubmitInfoKHR where
        type FieldType "sType" VkD3D12FenceSubmitInfoKHR = VkStructureType
        type FieldOptional "sType" VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkD3D12FenceSubmitInfoKHR =
             #{offset VkD3D12FenceSubmitInfoKHR, sType}
        type FieldIsArray "sType" VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, sType}

instance CanReadField "sType" VkD3D12FenceSubmitInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkD3D12FenceSubmitInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkD3D12FenceSubmitInfoKHR
         where
        type VkPNextMType VkD3D12FenceSubmitInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkD3D12FenceSubmitInfoKHR where
        type FieldType "pNext" VkD3D12FenceSubmitInfoKHR = Ptr Void
        type FieldOptional "pNext" VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkD3D12FenceSubmitInfoKHR =
             #{offset VkD3D12FenceSubmitInfoKHR, pNext}
        type FieldIsArray "pNext" VkD3D12FenceSubmitInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, pNext}

instance CanReadField "pNext" VkD3D12FenceSubmitInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkD3D12FenceSubmitInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkWaitSemaphoreValuesCount VkD3D12FenceSubmitInfoKHR where
        type VkWaitSemaphoreValuesCountMType VkD3D12FenceSubmitInfoKHR =
             Word32

        {-# NOINLINE vkWaitSemaphoreValuesCount #-}
        vkWaitSemaphoreValuesCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount})

        {-# INLINE vkWaitSemaphoreValuesCountByteOffset #-}
        vkWaitSemaphoreValuesCountByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

        {-# INLINE readVkWaitSemaphoreValuesCount #-}
        readVkWaitSemaphoreValuesCount p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

        {-# INLINE writeVkWaitSemaphoreValuesCount #-}
        writeVkWaitSemaphoreValuesCount p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR where
        type FieldType "waitSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR
             = Word32
        type FieldOptional "waitSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             =
             #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}
        type FieldIsArray "waitSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

instance CanReadField "waitSemaphoreValuesCount"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkWaitSemaphoreValuesCount

        {-# INLINE readField #-}
        readField = readVkWaitSemaphoreValuesCount

instance CanWriteField "waitSemaphoreValuesCount"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkWaitSemaphoreValuesCount

instance {-# OVERLAPPING #-}
         HasVkPWaitSemaphoreValues VkD3D12FenceSubmitInfoKHR where
        type VkPWaitSemaphoreValuesMType VkD3D12FenceSubmitInfoKHR =
             Ptr Word64

        {-# NOINLINE vkPWaitSemaphoreValues #-}
        vkPWaitSemaphoreValues x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues})

        {-# INLINE vkPWaitSemaphoreValuesByteOffset #-}
        vkPWaitSemaphoreValuesByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

        {-# INLINE readVkPWaitSemaphoreValues #-}
        readVkPWaitSemaphoreValues p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

        {-# INLINE writeVkPWaitSemaphoreValues #-}
        writeVkPWaitSemaphoreValues p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR where
        type FieldType "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR =
             Ptr Word64
        type FieldOptional "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR =
             #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}
        type FieldIsArray "pWaitSemaphoreValues" VkD3D12FenceSubmitInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

instance CanReadField "pWaitSemaphoreValues"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPWaitSemaphoreValues

        {-# INLINE readField #-}
        readField = readVkPWaitSemaphoreValues

instance CanWriteField "pWaitSemaphoreValues"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPWaitSemaphoreValues

instance {-# OVERLAPPING #-}
         HasVkSignalSemaphoreValuesCount VkD3D12FenceSubmitInfoKHR where
        type VkSignalSemaphoreValuesCountMType VkD3D12FenceSubmitInfoKHR =
             Word32

        {-# NOINLINE vkSignalSemaphoreValuesCount #-}
        vkSignalSemaphoreValuesCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount})

        {-# INLINE vkSignalSemaphoreValuesCountByteOffset #-}
        vkSignalSemaphoreValuesCountByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

        {-# INLINE readVkSignalSemaphoreValuesCount #-}
        readVkSignalSemaphoreValuesCount p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

        {-# INLINE writeVkSignalSemaphoreValuesCount #-}
        writeVkSignalSemaphoreValuesCount p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreValuesCount" VkD3D12FenceSubmitInfoKHR
         where
        type FieldType "signalSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = Word32
        type FieldOptional "signalSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             =
             #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}
        type FieldIsArray "signalSemaphoreValuesCount"
               VkD3D12FenceSubmitInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

instance CanReadField "signalSemaphoreValuesCount"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSignalSemaphoreValuesCount

        {-# INLINE readField #-}
        readField = readVkSignalSemaphoreValuesCount

instance CanWriteField "signalSemaphoreValuesCount"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSignalSemaphoreValuesCount

instance {-# OVERLAPPING #-}
         HasVkPSignalSemaphoreValues VkD3D12FenceSubmitInfoKHR where
        type VkPSignalSemaphoreValuesMType VkD3D12FenceSubmitInfoKHR =
             Ptr Word64

        {-# NOINLINE vkPSignalSemaphoreValues #-}
        vkPSignalSemaphoreValues x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues})

        {-# INLINE vkPSignalSemaphoreValuesByteOffset #-}
        vkPSignalSemaphoreValuesByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

        {-# INLINE readVkPSignalSemaphoreValues #-}
        readVkPSignalSemaphoreValues p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

        {-# INLINE writeVkPSignalSemaphoreValues #-}
        writeVkPSignalSemaphoreValues p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphoreValues" VkD3D12FenceSubmitInfoKHR where
        type FieldType "pSignalSemaphoreValues" VkD3D12FenceSubmitInfoKHR =
             Ptr Word64
        type FieldOptional "pSignalSemaphoreValues"
               VkD3D12FenceSubmitInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphoreValues" VkD3D12FenceSubmitInfoKHR
             =
             #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}
        type FieldIsArray "pSignalSemaphoreValues"
               VkD3D12FenceSubmitInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

instance CanReadField "pSignalSemaphoreValues"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPSignalSemaphoreValues

        {-# INLINE readField #-}
        readField = readVkPSignalSemaphoreValues

instance CanWriteField "pSignalSemaphoreValues"
           VkD3D12FenceSubmitInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSignalSemaphoreValues

instance Show VkD3D12FenceSubmitInfoKHR where
        showsPrec d x
          = showString "VkD3D12FenceSubmitInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkWaitSemaphoreValuesCount = " .
                            showsPrec d (vkWaitSemaphoreValuesCount x) .
                              showString ", " .
                                showString "vkPWaitSemaphoreValues = " .
                                  showsPrec d (vkPWaitSemaphoreValues x) .
                                    showString ", " .
                                      showString "vkSignalSemaphoreValuesCount = " .
                                        showsPrec d (vkSignalSemaphoreValuesCount x) .
                                          showString ", " .
                                            showString "vkPSignalSemaphoreValues = " .
                                              showsPrec d (vkPSignalSemaphoreValues x) .
                                                showChar '}'
