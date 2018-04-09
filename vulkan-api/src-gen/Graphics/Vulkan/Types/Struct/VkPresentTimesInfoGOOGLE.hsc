#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPresentTimesInfoGOOGLE
       (VkPresentTimesInfoGOOGLE(..)) where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Base                                         (Addr##,
                                                                   ByteArray##,
                                                                   byteArrayContents##,
                                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType       (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR    (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.VkPresentTimeGOOGLE (VkPresentTimeGOOGLE)
import           System.IO.Unsafe                                 (unsafeDupablePerformIO)

-- | > typedef struct VkPresentTimesInfoGOOGLE {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         swapchainCount;
--   >     const VkPresentTimeGOOGLE*   pTimes;
--   > } VkPresentTimesInfoGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPresentTimesInfoGOOGLEVkPresentTimesInfoGOOGLE registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPresentTimesInfoGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPresentTimesInfoGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "swapchainCount" VkPresentTimesInfoGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, swapchainCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchainCount" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, swapchainCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pTimes" VkPresentTimesInfoGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimesInfoGOOGLE, pTimes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimesInfoGOOGLE, pTimes}

instance {-# OVERLAPPING #-}
         CanWriteField "pTimes" VkPresentTimesInfoGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimesInfoGOOGLE, pTimes}

instance Show VkPresentTimesInfoGOOGLE where
        showsPrec d x
          = showString "VkPresentTimesInfoGOOGLE {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchainCount = " .
                            showsPrec d (getField @"swapchainCount" x) .
                              showString ", " .
                                showString "pTimes = " .
                                  showsPrec d (getField @"pTimes" x) . showChar '}'
