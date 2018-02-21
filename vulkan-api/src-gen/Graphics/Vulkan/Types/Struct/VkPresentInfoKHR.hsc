#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPresentInfoKHR
       (VkPresentInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkResult        (VkResult)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkSemaphore,
                                                             VkSwapchainKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t         waitSemaphoreCount;
--   >     const VkSemaphore* pWaitSemaphores;
--   >     uint32_t                         swapchainCount;
--   >     const VkSwapchainKHR* pSwapchains;
--   >     const uint32_t* pImageIndices;
--   >     VkResult* pResults;
--   > } VkPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPresentInfoKHR.html VkPresentInfoKHR registry at www.khronos.org>
data VkPresentInfoKHR = VkPresentInfoKHR## Addr## ByteArray##

instance Eq VkPresentInfoKHR where
        (VkPresentInfoKHR## a _) == x@(VkPresentInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentInfoKHR where
        (VkPresentInfoKHR## a _) `compare` x@(VkPresentInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentInfoKHR where
        sizeOf ~_ = #{size VkPresentInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentInfoKHR where
        unsafeAddr (VkPresentInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentInfoKHR where
        type StructFields VkPresentInfoKHR =
             '["sType", "pNext", "waitSemaphoreCount", "pWaitSemaphores", -- ' closing tick for hsc2hs
               "swapchainCount", "pSwapchains", "pImageIndices", "pResults"]
        type CUnionType VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkPresentInfoKHR where
        type VkSTypeMType VkPresentInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPresentInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPresentInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPresentInfoKHR, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkPresentInfoKHR
         where
        type FieldType "sType" VkPresentInfoKHR = VkStructureType
        type FieldOptional "sType" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, sType}
        type FieldIsArray "sType" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, sType}

instance CanReadField "sType" VkPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkPresentInfoKHR where
        type VkPNextMType VkPresentInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPresentInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPresentInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkPresentInfoKHR
         where
        type FieldType "pNext" VkPresentInfoKHR = Ptr Void
        type FieldOptional "pNext" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pNext}
        type FieldIsArray "pNext" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, pNext}

instance CanReadField "pNext" VkPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkWaitSemaphoreCount VkPresentInfoKHR where
        type VkWaitSemaphoreCountMType VkPresentInfoKHR = Word32

        {-# NOINLINE vkWaitSemaphoreCount #-}
        vkWaitSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, waitSemaphoreCount})

        {-# INLINE vkWaitSemaphoreCountByteOffset #-}
        vkWaitSemaphoreCountByteOffset ~_
          = #{offset VkPresentInfoKHR, waitSemaphoreCount}

        {-# INLINE readVkWaitSemaphoreCount #-}
        readVkWaitSemaphoreCount p
          = peekByteOff p #{offset VkPresentInfoKHR, waitSemaphoreCount}

        {-# INLINE writeVkWaitSemaphoreCount #-}
        writeVkWaitSemaphoreCount p
          = pokeByteOff p #{offset VkPresentInfoKHR, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkPresentInfoKHR where
        type FieldType "waitSemaphoreCount" VkPresentInfoKHR = Word32
        type FieldOptional "waitSemaphoreCount" VkPresentInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentInfoKHR, waitSemaphoreCount}

instance CanReadField "waitSemaphoreCount" VkPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkWaitSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkWaitSemaphoreCount

instance CanWriteField "waitSemaphoreCount" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkWaitSemaphoreCount

instance {-# OVERLAPPING #-} HasVkPWaitSemaphores VkPresentInfoKHR
         where
        type VkPWaitSemaphoresMType VkPresentInfoKHR = Ptr VkSemaphore

        {-# NOINLINE vkPWaitSemaphores #-}
        vkPWaitSemaphores x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pWaitSemaphores})

        {-# INLINE vkPWaitSemaphoresByteOffset #-}
        vkPWaitSemaphoresByteOffset ~_
          = #{offset VkPresentInfoKHR, pWaitSemaphores}

        {-# INLINE readVkPWaitSemaphores #-}
        readVkPWaitSemaphores p
          = peekByteOff p #{offset VkPresentInfoKHR, pWaitSemaphores}

        {-# INLINE writeVkPWaitSemaphores #-}
        writeVkPWaitSemaphores p
          = pokeByteOff p #{offset VkPresentInfoKHR, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphores" VkPresentInfoKHR where
        type FieldType "pWaitSemaphores" VkPresentInfoKHR = Ptr VkSemaphore
        type FieldOptional "pWaitSemaphores" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphores" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pWaitSemaphores}
        type FieldIsArray "pWaitSemaphores" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentInfoKHR, pWaitSemaphores}

instance CanReadField "pWaitSemaphores" VkPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkPWaitSemaphores

        {-# INLINE readField #-}
        readField = readVkPWaitSemaphores

instance CanWriteField "pWaitSemaphores" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPWaitSemaphores

instance {-# OVERLAPPING #-} HasVkSwapchainCount VkPresentInfoKHR
         where
        type VkSwapchainCountMType VkPresentInfoKHR = Word32

        {-# NOINLINE vkSwapchainCount #-}
        vkSwapchainCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, swapchainCount})

        {-# INLINE vkSwapchainCountByteOffset #-}
        vkSwapchainCountByteOffset ~_
          = #{offset VkPresentInfoKHR, swapchainCount}

        {-# INLINE readVkSwapchainCount #-}
        readVkSwapchainCount p
          = peekByteOff p #{offset VkPresentInfoKHR, swapchainCount}

        {-# INLINE writeVkSwapchainCount #-}
        writeVkSwapchainCount p
          = pokeByteOff p #{offset VkPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkPresentInfoKHR where
        type FieldType "swapchainCount" VkPresentInfoKHR = Word32
        type FieldOptional "swapchainCount" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, swapchainCount}
        type FieldIsArray "swapchainCount" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentInfoKHR, swapchainCount}

instance CanReadField "swapchainCount" VkPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkSwapchainCount

        {-# INLINE readField #-}
        readField = readVkSwapchainCount

instance CanWriteField "swapchainCount" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchainCount

instance {-# OVERLAPPING #-} HasVkPSwapchains VkPresentInfoKHR
         where
        type VkPSwapchainsMType VkPresentInfoKHR = Ptr VkSwapchainKHR

        {-# NOINLINE vkPSwapchains #-}
        vkPSwapchains x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pSwapchains})

        {-# INLINE vkPSwapchainsByteOffset #-}
        vkPSwapchainsByteOffset ~_
          = #{offset VkPresentInfoKHR, pSwapchains}

        {-# INLINE readVkPSwapchains #-}
        readVkPSwapchains p
          = peekByteOff p #{offset VkPresentInfoKHR, pSwapchains}

        {-# INLINE writeVkPSwapchains #-}
        writeVkPSwapchains p
          = pokeByteOff p #{offset VkPresentInfoKHR, pSwapchains}

instance {-# OVERLAPPING #-}
         HasField "pSwapchains" VkPresentInfoKHR where
        type FieldType "pSwapchains" VkPresentInfoKHR = Ptr VkSwapchainKHR
        type FieldOptional "pSwapchains" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSwapchains" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pSwapchains}
        type FieldIsArray "pSwapchains" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, pSwapchains}

instance CanReadField "pSwapchains" VkPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkPSwapchains

        {-# INLINE readField #-}
        readField = readVkPSwapchains

instance CanWriteField "pSwapchains" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPSwapchains

instance {-# OVERLAPPING #-} HasVkPImageIndices VkPresentInfoKHR
         where
        type VkPImageIndicesMType VkPresentInfoKHR = Ptr Word32

        {-# NOINLINE vkPImageIndices #-}
        vkPImageIndices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pImageIndices})

        {-# INLINE vkPImageIndicesByteOffset #-}
        vkPImageIndicesByteOffset ~_
          = #{offset VkPresentInfoKHR, pImageIndices}

        {-# INLINE readVkPImageIndices #-}
        readVkPImageIndices p
          = peekByteOff p #{offset VkPresentInfoKHR, pImageIndices}

        {-# INLINE writeVkPImageIndices #-}
        writeVkPImageIndices p
          = pokeByteOff p #{offset VkPresentInfoKHR, pImageIndices}

instance {-# OVERLAPPING #-}
         HasField "pImageIndices" VkPresentInfoKHR where
        type FieldType "pImageIndices" VkPresentInfoKHR = Ptr Word32
        type FieldOptional "pImageIndices" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pImageIndices" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pImageIndices}
        type FieldIsArray "pImageIndices" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, pImageIndices}

instance CanReadField "pImageIndices" VkPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkPImageIndices

        {-# INLINE readField #-}
        readField = readVkPImageIndices

instance CanWriteField "pImageIndices" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPImageIndices

instance {-# OVERLAPPING #-} HasVkPResults VkPresentInfoKHR where
        type VkPResultsMType VkPresentInfoKHR = Ptr VkResult

        {-# NOINLINE vkPResults #-}
        vkPResults x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentInfoKHR, pResults})

        {-# INLINE vkPResultsByteOffset #-}
        vkPResultsByteOffset ~_
          = #{offset VkPresentInfoKHR, pResults}

        {-# INLINE readVkPResults #-}
        readVkPResults p
          = peekByteOff p #{offset VkPresentInfoKHR, pResults}

        {-# INLINE writeVkPResults #-}
        writeVkPResults p
          = pokeByteOff p #{offset VkPresentInfoKHR, pResults}

instance {-# OVERLAPPING #-} HasField "pResults" VkPresentInfoKHR
         where
        type FieldType "pResults" VkPresentInfoKHR = Ptr VkResult
        type FieldOptional "pResults" VkPresentInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pResults" VkPresentInfoKHR =
             #{offset VkPresentInfoKHR, pResults}
        type FieldIsArray "pResults" VkPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentInfoKHR, pResults}

instance CanReadField "pResults" VkPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkPResults

        {-# INLINE readField #-}
        readField = readVkPResults

instance CanWriteField "pResults" VkPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPResults

instance Show VkPresentInfoKHR where
        showsPrec d x
          = showString "VkPresentInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkWaitSemaphoreCount = " .
                            showsPrec d (vkWaitSemaphoreCount x) .
                              showString ", " .
                                showString "vkPWaitSemaphores = " .
                                  showsPrec d (vkPWaitSemaphores x) .
                                    showString ", " .
                                      showString "vkSwapchainCount = " .
                                        showsPrec d (vkSwapchainCount x) .
                                          showString ", " .
                                            showString "vkPSwapchains = " .
                                              showsPrec d (vkPSwapchains x) .
                                                showString ", " .
                                                  showString "vkPImageIndices = " .
                                                    showsPrec d (vkPImageIndices x) .
                                                      showString ", " .
                                                        showString "vkPResults = " .
                                                          showsPrec d (vkPResults x) . showChar '}'
