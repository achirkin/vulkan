#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImagePlaneMemoryInfo
       (VkBindImagePlaneMemoryInfo(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Base                                           (Addr##,
                                                                     ByteArray##,
                                                                     byteArrayContents##,
                                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags      (VkImageAspectFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo (VkBindImageMemoryInfo)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkBindImagePlaneMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImageAspectFlagBits            planeAspect;
--   > } VkBindImagePlaneMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBindImagePlaneMemoryInfo VkBindImagePlaneMemoryInfo registry at www.khronos.org>
data VkBindImagePlaneMemoryInfo = VkBindImagePlaneMemoryInfo## Addr##
                                                              ByteArray##

instance Eq VkBindImagePlaneMemoryInfo where
        (VkBindImagePlaneMemoryInfo## a _) ==
          x@(VkBindImagePlaneMemoryInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImagePlaneMemoryInfo where
        (VkBindImagePlaneMemoryInfo## a _) `compare`
          x@(VkBindImagePlaneMemoryInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImagePlaneMemoryInfo where
        sizeOf ~_ = #{size VkBindImagePlaneMemoryInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindImagePlaneMemoryInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImagePlaneMemoryInfo where
        unsafeAddr (VkBindImagePlaneMemoryInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImagePlaneMemoryInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImagePlaneMemoryInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImagePlaneMemoryInfo where
        type StructFields VkBindImagePlaneMemoryInfo =
             '["sType", "pNext", "planeAspect"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImagePlaneMemoryInfo =
             '[VkBindImageMemoryInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImagePlaneMemoryInfo where
        type FieldType "sType" VkBindImagePlaneMemoryInfo = VkStructureType
        type FieldOptional "sType" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImagePlaneMemoryInfo =
             #{offset VkBindImagePlaneMemoryInfo, sType}
        type FieldIsArray "sType" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImagePlaneMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImagePlaneMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImagePlaneMemoryInfo where
        type FieldType "pNext" VkBindImagePlaneMemoryInfo = Ptr Void
        type FieldOptional "pNext" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImagePlaneMemoryInfo =
             #{offset VkBindImagePlaneMemoryInfo, pNext}
        type FieldIsArray "pNext" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImagePlaneMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImagePlaneMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "planeAspect" VkBindImagePlaneMemoryInfo where
        type FieldType "planeAspect" VkBindImagePlaneMemoryInfo =
             VkImageAspectFlagBits
        type FieldOptional "planeAspect" VkBindImagePlaneMemoryInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "planeAspect" VkBindImagePlaneMemoryInfo =
             #{offset VkBindImagePlaneMemoryInfo, planeAspect}
        type FieldIsArray "planeAspect" VkBindImagePlaneMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfo, planeAspect}

instance {-# OVERLAPPING #-}
         CanReadField "planeAspect" VkBindImagePlaneMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfo, planeAspect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfo, planeAspect}

instance {-# OVERLAPPING #-}
         CanWriteField "planeAspect" VkBindImagePlaneMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfo, planeAspect}

instance Show VkBindImagePlaneMemoryInfo where
        showsPrec d x
          = showString "VkBindImagePlaneMemoryInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "planeAspect = " .
                            showsPrec d (getField @"planeAspect" x) . showChar '}'
