#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupRenderPassBeginInfo
       (VkDeviceGroupRenderPassBeginInfo(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkRect2D              (VkRect2D)
import           Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo (VkRenderPassBeginInfo)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupRenderPassBeginInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   >     uint32_t         deviceRenderAreaCount;
--   >     const VkRect2D*  pDeviceRenderAreas;
--   > } VkDeviceGroupRenderPassBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDeviceGroupRenderPassBeginInfo.html VkDeviceGroupRenderPassBeginInfo registry at www.khronos.org>
data VkDeviceGroupRenderPassBeginInfo = VkDeviceGroupRenderPassBeginInfo## Addr##
                                                                          ByteArray##

instance Eq VkDeviceGroupRenderPassBeginInfo where
        (VkDeviceGroupRenderPassBeginInfo## a _) ==
          x@(VkDeviceGroupRenderPassBeginInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupRenderPassBeginInfo where
        (VkDeviceGroupRenderPassBeginInfo## a _) `compare`
          x@(VkDeviceGroupRenderPassBeginInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupRenderPassBeginInfo where
        sizeOf ~_ = #{size VkDeviceGroupRenderPassBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupRenderPassBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupRenderPassBeginInfo where
        unsafeAddr (VkDeviceGroupRenderPassBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupRenderPassBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupRenderPassBeginInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupRenderPassBeginInfo where
        type StructFields VkDeviceGroupRenderPassBeginInfo =
             '["sType", "pNext", "deviceMask", "deviceRenderAreaCount", -- ' closing tick for hsc2hs
               "pDeviceRenderAreas"]
        type CUnionType VkDeviceGroupRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupRenderPassBeginInfo =
             '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupRenderPassBeginInfo where
        type FieldType "sType" VkDeviceGroupRenderPassBeginInfo =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupRenderPassBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupRenderPassBeginInfo =
             #{offset VkDeviceGroupRenderPassBeginInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupRenderPassBeginInfo where
        type FieldType "pNext" VkDeviceGroupRenderPassBeginInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupRenderPassBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupRenderPassBeginInfo =
             #{offset VkDeviceGroupRenderPassBeginInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupRenderPassBeginInfo where
        type FieldType "deviceMask" VkDeviceGroupRenderPassBeginInfo =
             Word32
        type FieldOptional "deviceMask" VkDeviceGroupRenderPassBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkDeviceGroupRenderPassBeginInfo =
             #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}
        type FieldIsArray "deviceMask" VkDeviceGroupRenderPassBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkDeviceGroupRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkDeviceGroupRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceRenderAreaCount" VkDeviceGroupRenderPassBeginInfo
         where
        type FieldType "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfo
             = Word32
        type FieldOptional "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfo
             =
             #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}
        type FieldIsArray "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         CanReadField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceRenderAreas" VkDeviceGroupRenderPassBeginInfo
         where
        type FieldType "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfo
             = Ptr VkRect2D
        type FieldOptional "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfo
             =
             #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}
        type FieldIsArray "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceRenderAreas" VkDeviceGroupRenderPassBeginInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceRenderAreas" VkDeviceGroupRenderPassBeginInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}

instance Show VkDeviceGroupRenderPassBeginInfo where
        showsPrec d x
          = showString "VkDeviceGroupRenderPassBeginInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceMask = " .
                            showsPrec d (getField @"deviceMask" x) .
                              showString ", " .
                                showString "deviceRenderAreaCount = " .
                                  showsPrec d (getField @"deviceRenderAreaCount" x) .
                                    showString ", " .
                                      showString "pDeviceRenderAreas = " .
                                        showsPrec d (getField @"pDeviceRenderAreas" x) .
                                          showChar '}'
