#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupRenderPassBeginInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupRenderPassBeginInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupRenderPassBeginInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupRenderPassBeginInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceMask}

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

instance {-# OVERLAPPING #-}
         CanReadField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, deviceRenderAreaCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceRenderAreas"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceRenderAreas"
           VkDeviceGroupRenderPassBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfoKHX, pDeviceRenderAreas}

instance Show VkDeviceGroupRenderPassBeginInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupRenderPassBeginInfoKHX {" .
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
