#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImageMemoryDeviceGroupInfo
       (VkBindImageMemoryDeviceGroupInfo(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Base                                           (Addr##,
                                                                     ByteArray##,
                                                                     byteArrayContents##,
                                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo (VkBindImageMemoryInfo)
import           Graphics.Vulkan.Types.Struct.VkRect2D              (VkRect2D)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkBindImageMemoryDeviceGroupInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   >     uint32_t         splitInstanceBindRegionCount;
--   >     const VkRect2D*  pSplitInstanceBindRegions;
--   > } VkBindImageMemoryDeviceGroupInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBindImageMemoryDeviceGroupInfo VkBindImageMemoryDeviceGroupInfo registry at www.khronos.org>
data VkBindImageMemoryDeviceGroupInfo = VkBindImageMemoryDeviceGroupInfo## Addr##
                                                                          ByteArray##

instance Eq VkBindImageMemoryDeviceGroupInfo where
        (VkBindImageMemoryDeviceGroupInfo## a _) ==
          x@(VkBindImageMemoryDeviceGroupInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryDeviceGroupInfo where
        (VkBindImageMemoryDeviceGroupInfo## a _) `compare`
          x@(VkBindImageMemoryDeviceGroupInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryDeviceGroupInfo where
        sizeOf ~_ = #{size VkBindImageMemoryDeviceGroupInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImageMemoryDeviceGroupInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImageMemoryDeviceGroupInfo where
        unsafeAddr (VkBindImageMemoryDeviceGroupInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImageMemoryDeviceGroupInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImageMemoryDeviceGroupInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImageMemoryDeviceGroupInfo where
        type StructFields VkBindImageMemoryDeviceGroupInfo =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices", -- ' closing tick for hsc2hs
               "splitInstanceBindRegionCount", "pSplitInstanceBindRegions"]
        type CUnionType VkBindImageMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImageMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImageMemoryDeviceGroupInfo =
             '[VkBindImageMemoryInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemoryDeviceGroupInfo where
        type FieldType "sType" VkBindImageMemoryDeviceGroupInfo =
             VkStructureType
        type FieldOptional "sType" VkBindImageMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImageMemoryDeviceGroupInfo =
             #{offset VkBindImageMemoryDeviceGroupInfo, sType}
        type FieldIsArray "sType" VkBindImageMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImageMemoryDeviceGroupInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImageMemoryDeviceGroupInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemoryDeviceGroupInfo where
        type FieldType "pNext" VkBindImageMemoryDeviceGroupInfo = Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImageMemoryDeviceGroupInfo =
             #{offset VkBindImageMemoryDeviceGroupInfo, pNext}
        type FieldIsArray "pNext" VkBindImageMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImageMemoryDeviceGroupInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImageMemoryDeviceGroupInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfo where
        type FieldType "deviceIndexCount" VkBindImageMemoryDeviceGroupInfo
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfo
             =
             #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo where
        type FieldType "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo =
             Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo
             =
             #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}
        type FieldIsArray "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "splitInstanceBindRegionCount"
           VkBindImageMemoryDeviceGroupInfo
         where
        type FieldType "splitInstanceBindRegionCount"
               VkBindImageMemoryDeviceGroupInfo
             = Word32
        type FieldOptional "splitInstanceBindRegionCount"
               VkBindImageMemoryDeviceGroupInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "splitInstanceBindRegionCount"
               VkBindImageMemoryDeviceGroupInfo
             =
             #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}
        type FieldIsArray "splitInstanceBindRegionCount"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}

instance {-# OVERLAPPING #-}
         CanReadField "splitInstanceBindRegionCount"
           VkBindImageMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}

instance {-# OVERLAPPING #-}
         CanWriteField "splitInstanceBindRegionCount"
           VkBindImageMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, splitInstanceBindRegionCount}

instance {-# OVERLAPPING #-}
         HasField "pSplitInstanceBindRegions"
           VkBindImageMemoryDeviceGroupInfo
         where
        type FieldType "pSplitInstanceBindRegions"
               VkBindImageMemoryDeviceGroupInfo
             = Ptr VkRect2D
        type FieldOptional "pSplitInstanceBindRegions"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSplitInstanceBindRegions"
               VkBindImageMemoryDeviceGroupInfo
             =
             #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}
        type FieldIsArray "pSplitInstanceBindRegions"
               VkBindImageMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}

instance {-# OVERLAPPING #-}
         CanReadField "pSplitInstanceBindRegions"
           VkBindImageMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}

instance {-# OVERLAPPING #-}
         CanWriteField "pSplitInstanceBindRegions"
           VkBindImageMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfo, pSplitInstanceBindRegions}

instance Show VkBindImageMemoryDeviceGroupInfo where
        showsPrec d x
          = showString "VkBindImageMemoryDeviceGroupInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceIndexCount = " .
                            showsPrec d (getField @"deviceIndexCount" x) .
                              showString ", " .
                                showString "pDeviceIndices = " .
                                  showsPrec d (getField @"pDeviceIndices" x) .
                                    showString ", " .
                                      showString "splitInstanceBindRegionCount = " .
                                        showsPrec d (getField @"splitInstanceBindRegionCount" x) .
                                          showString ", " .
                                            showString "pSplitInstanceBindRegions = " .
                                              showsPrec d (getField @"pSplitInstanceBindRegions" x)
                                                . showChar '}'
