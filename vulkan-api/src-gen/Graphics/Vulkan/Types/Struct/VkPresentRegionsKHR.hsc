#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPresentRegionsKHR
       (VkPresentRegionsKHR(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR   (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.VkPresentRegionKHR (VkPresentRegionKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkPresentRegionsKHR {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentRegionKHR*   pRegions;
--   > } VkPresentRegionsKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPresentRegionsKHR.html VkPresentRegionsKHR registry at www.khronos.org>
data VkPresentRegionsKHR = VkPresentRegionsKHR## Addr## ByteArray##

instance Eq VkPresentRegionsKHR where
        (VkPresentRegionsKHR## a _) == x@(VkPresentRegionsKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentRegionsKHR where
        (VkPresentRegionsKHR## a _) `compare` x@(VkPresentRegionsKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentRegionsKHR where
        sizeOf ~_ = #{size VkPresentRegionsKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentRegionsKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentRegionsKHR where
        unsafeAddr (VkPresentRegionsKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentRegionsKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentRegionsKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentRegionsKHR where
        type StructFields VkPresentRegionsKHR =
             '["sType", "pNext", "swapchainCount", "pRegions"] -- ' closing tick for hsc2hs
        type CUnionType VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentRegionsKHR = '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkPresentRegionsKHR where
        type VkSTypeMType VkPresentRegionsKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPresentRegionsKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPresentRegionsKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPresentRegionsKHR, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkPresentRegionsKHR
         where
        type FieldType "sType" VkPresentRegionsKHR = VkStructureType
        type FieldOptional "sType" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPresentRegionsKHR =
             #{offset VkPresentRegionsKHR, sType}
        type FieldIsArray "sType" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentRegionsKHR, sType}

instance CanReadField "sType" VkPresentRegionsKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPresentRegionsKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkPresentRegionsKHR where
        type VkPNextMType VkPresentRegionsKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPresentRegionsKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPresentRegionsKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPresentRegionsKHR, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkPresentRegionsKHR
         where
        type FieldType "pNext" VkPresentRegionsKHR = Ptr Void
        type FieldOptional "pNext" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPresentRegionsKHR =
             #{offset VkPresentRegionsKHR, pNext}
        type FieldIsArray "pNext" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentRegionsKHR, pNext}

instance CanReadField "pNext" VkPresentRegionsKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPresentRegionsKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchainCount VkPresentRegionsKHR where
        type VkSwapchainCountMType VkPresentRegionsKHR = Word32

        {-# NOINLINE vkSwapchainCount #-}
        vkSwapchainCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, swapchainCount})

        {-# INLINE vkSwapchainCountByteOffset #-}
        vkSwapchainCountByteOffset ~_
          = #{offset VkPresentRegionsKHR, swapchainCount}

        {-# INLINE readVkSwapchainCount #-}
        readVkSwapchainCount p
          = peekByteOff p #{offset VkPresentRegionsKHR, swapchainCount}

        {-# INLINE writeVkSwapchainCount #-}
        writeVkSwapchainCount p
          = pokeByteOff p #{offset VkPresentRegionsKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkPresentRegionsKHR where
        type FieldType "swapchainCount" VkPresentRegionsKHR = Word32
        type FieldOptional "swapchainCount" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkPresentRegionsKHR =
             #{offset VkPresentRegionsKHR, swapchainCount}
        type FieldIsArray "swapchainCount" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentRegionsKHR, swapchainCount}

instance CanReadField "swapchainCount" VkPresentRegionsKHR where
        {-# INLINE getField #-}
        getField = vkSwapchainCount

        {-# INLINE readField #-}
        readField = readVkSwapchainCount

instance CanWriteField "swapchainCount" VkPresentRegionsKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchainCount

instance {-# OVERLAPPING #-} HasVkPRegions VkPresentRegionsKHR
         where
        type VkPRegionsMType VkPresentRegionsKHR = Ptr VkPresentRegionKHR

        {-# NOINLINE vkPRegions #-}
        vkPRegions x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionsKHR, pRegions})

        {-# INLINE vkPRegionsByteOffset #-}
        vkPRegionsByteOffset ~_
          = #{offset VkPresentRegionsKHR, pRegions}

        {-# INLINE readVkPRegions #-}
        readVkPRegions p
          = peekByteOff p #{offset VkPresentRegionsKHR, pRegions}

        {-# INLINE writeVkPRegions #-}
        writeVkPRegions p
          = pokeByteOff p #{offset VkPresentRegionsKHR, pRegions}

instance {-# OVERLAPPING #-}
         HasField "pRegions" VkPresentRegionsKHR where
        type FieldType "pRegions" VkPresentRegionsKHR =
             Ptr VkPresentRegionKHR
        type FieldOptional "pRegions" VkPresentRegionsKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pRegions" VkPresentRegionsKHR =
             #{offset VkPresentRegionsKHR, pRegions}
        type FieldIsArray "pRegions" VkPresentRegionsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentRegionsKHR, pRegions}

instance CanReadField "pRegions" VkPresentRegionsKHR where
        {-# INLINE getField #-}
        getField = vkPRegions

        {-# INLINE readField #-}
        readField = readVkPRegions

instance CanWriteField "pRegions" VkPresentRegionsKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPRegions

instance Show VkPresentRegionsKHR where
        showsPrec d x
          = showString "VkPresentRegionsKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchainCount = " .
                            showsPrec d (vkSwapchainCount x) .
                              showString ", " .
                                showString "vkPRegions = " .
                                  showsPrec d (vkPRegions x) . showChar '}'
