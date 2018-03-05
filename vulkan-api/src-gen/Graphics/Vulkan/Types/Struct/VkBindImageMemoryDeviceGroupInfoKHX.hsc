#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImageMemoryDeviceGroupInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImageMemoryDeviceGroupInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImageMemoryDeviceGroupInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImageMemoryDeviceGroupInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "deviceIndexCount" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceIndexCount"
           VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, deviceIndexCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceIndices" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pDeviceIndices}

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

instance {-# OVERLAPPING #-}
         CanReadField "SFRRectCount" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

instance {-# OVERLAPPING #-}
         CanWriteField "SFRRectCount" VkBindImageMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, SFRRectCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

instance {-# OVERLAPPING #-}
         CanWriteField "pSFRRects" VkBindImageMemoryDeviceGroupInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImageMemoryDeviceGroupInfoKHX, pSFRRects}

instance Show VkBindImageMemoryDeviceGroupInfoKHX where
        showsPrec d x
          = showString "VkBindImageMemoryDeviceGroupInfoKHX {" .
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
                                      showString "SFRRectCount = " .
                                        showsPrec d (getField @"SFRRectCount" x) .
                                          showString ", " .
                                            showString "pSFRRects = " .
                                              showsPrec d (getField @"pSFRRects" x) . showChar '}'
