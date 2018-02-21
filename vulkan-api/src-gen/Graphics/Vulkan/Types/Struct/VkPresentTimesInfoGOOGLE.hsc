#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPresentTimesInfoGOOGLE
       (VkPresentTimesInfoGOOGLE(..)) where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType       (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR    (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.VkPresentTimeGOOGLE (VkPresentTimeGOOGLE)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                 (unsafeDupablePerformIO)

-- | > typedef struct VkPresentTimesInfoGOOGLE {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentTimeGOOGLE*   pTimes;
--   > } VkPresentTimesInfoGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPresentTimesInfoGOOGLE.html VkPresentTimesInfoGOOGLE registry at www.khronos.org>
data VkPresentTimesInfoGOOGLE = VkPresentTimesInfoGOOGLE## Addr##
                                                          ByteArray##

instance Eq VkPresentTimesInfoGOOGLE where
        (VkPresentTimesInfoGOOGLE## a _) ==
          x@(VkPresentTimesInfoGOOGLE## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentTimesInfoGOOGLE where
        (VkPresentTimesInfoGOOGLE## a _) `compare`
          x@(VkPresentTimesInfoGOOGLE## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentTimesInfoGOOGLE where
        sizeOf ~_ = #{size VkPresentTimesInfoGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentTimesInfoGOOGLE}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentTimesInfoGOOGLE where
        unsafeAddr (VkPresentTimesInfoGOOGLE## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentTimesInfoGOOGLE## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentTimesInfoGOOGLE## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentTimesInfoGOOGLE where
        type StructFields VkPresentTimesInfoGOOGLE =
             '["sType", "pNext", "swapchainCount", "pTimes"] -- ' closing tick for hsc2hs
        type CUnionType VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentTimesInfoGOOGLE = '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkPresentTimesInfoGOOGLE
         where
        type VkSTypeMType VkPresentTimesInfoGOOGLE = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPresentTimesInfoGOOGLE, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPresentTimesInfoGOOGLE where
        type FieldType "sType" VkPresentTimesInfoGOOGLE = VkStructureType
        type FieldOptional "sType" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPresentTimesInfoGOOGLE =
             #{offset VkPresentTimesInfoGOOGLE, sType}
        type FieldIsArray "sType" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentTimesInfoGOOGLE, sType}

instance CanReadField "sType" VkPresentTimesInfoGOOGLE where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkPresentTimesInfoGOOGLE
         where
        type VkPNextMType VkPresentTimesInfoGOOGLE = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPresentTimesInfoGOOGLE, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPresentTimesInfoGOOGLE where
        type FieldType "pNext" VkPresentTimesInfoGOOGLE = Ptr Void
        type FieldOptional "pNext" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPresentTimesInfoGOOGLE =
             #{offset VkPresentTimesInfoGOOGLE, pNext}
        type FieldIsArray "pNext" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentTimesInfoGOOGLE, pNext}

instance CanReadField "pNext" VkPresentTimesInfoGOOGLE where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchainCount VkPresentTimesInfoGOOGLE where
        type VkSwapchainCountMType VkPresentTimesInfoGOOGLE = Word32

        {-# NOINLINE vkSwapchainCount #-}
        vkSwapchainCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, swapchainCount})

        {-# INLINE vkSwapchainCountByteOffset #-}
        vkSwapchainCountByteOffset ~_
          = #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

        {-# INLINE readVkSwapchainCount #-}
        readVkSwapchainCount p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

        {-# INLINE writeVkSwapchainCount #-}
        writeVkSwapchainCount p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkPresentTimesInfoGOOGLE where
        type FieldType "swapchainCount" VkPresentTimesInfoGOOGLE = Word32
        type FieldOptional "swapchainCount" VkPresentTimesInfoGOOGLE =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkPresentTimesInfoGOOGLE =
             #{offset VkPresentTimesInfoGOOGLE, swapchainCount}
        type FieldIsArray "swapchainCount" VkPresentTimesInfoGOOGLE =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

instance CanReadField "swapchainCount" VkPresentTimesInfoGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkSwapchainCount

        {-# INLINE readField #-}
        readField = readVkSwapchainCount

instance CanWriteField "swapchainCount" VkPresentTimesInfoGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchainCount

instance {-# OVERLAPPING #-} HasVkPTimes VkPresentTimesInfoGOOGLE
         where
        type VkPTimesMType VkPresentTimesInfoGOOGLE =
             Ptr VkPresentTimeGOOGLE

        {-# NOINLINE vkPTimes #-}
        vkPTimes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, pTimes})

        {-# INLINE vkPTimesByteOffset #-}
        vkPTimesByteOffset ~_
          = #{offset VkPresentTimesInfoGOOGLE, pTimes}

        {-# INLINE readVkPTimes #-}
        readVkPTimes p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, pTimes}

        {-# INLINE writeVkPTimes #-}
        writeVkPTimes p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, pTimes}

instance {-# OVERLAPPING #-}
         HasField "pTimes" VkPresentTimesInfoGOOGLE where
        type FieldType "pTimes" VkPresentTimesInfoGOOGLE =
             Ptr VkPresentTimeGOOGLE
        type FieldOptional "pTimes" VkPresentTimesInfoGOOGLE = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pTimes" VkPresentTimesInfoGOOGLE =
             #{offset VkPresentTimesInfoGOOGLE, pTimes}
        type FieldIsArray "pTimes" VkPresentTimesInfoGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentTimesInfoGOOGLE, pTimes}

instance CanReadField "pTimes" VkPresentTimesInfoGOOGLE where
        {-# INLINE getField #-}
        getField = vkPTimes

        {-# INLINE readField #-}
        readField = readVkPTimes

instance CanWriteField "pTimes" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField = writeVkPTimes

instance Show VkPresentTimesInfoGOOGLE where
        showsPrec d x
          = showString "VkPresentTimesInfoGOOGLE {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchainCount = " .
                            showsPrec d (vkSwapchainCount x) .
                              showString ", " .
                                showString "vkPTimes = " . showsPrec d (vkPTimes x) . showChar '}'
