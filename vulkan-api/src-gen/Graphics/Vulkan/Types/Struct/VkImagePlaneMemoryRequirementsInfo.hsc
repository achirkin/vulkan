#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImagePlaneMemoryRequirementsInfo
       (VkImagePlaneMemoryRequirementsInfo(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags               (VkImageAspectFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2 (VkImageMemoryRequirementsInfo2)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkImagePlaneMemoryRequirementsInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImageAspectFlagBits            planeAspect;
--   > } VkImagePlaneMemoryRequirementsInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkImagePlaneMemoryRequirementsInfo.html VkImagePlaneMemoryRequirementsInfo registry at www.khronos.org>
data VkImagePlaneMemoryRequirementsInfo = VkImagePlaneMemoryRequirementsInfo## Addr##
                                                                              ByteArray##

instance Eq VkImagePlaneMemoryRequirementsInfo where
        (VkImagePlaneMemoryRequirementsInfo## a _) ==
          x@(VkImagePlaneMemoryRequirementsInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImagePlaneMemoryRequirementsInfo where
        (VkImagePlaneMemoryRequirementsInfo## a _) `compare`
          x@(VkImagePlaneMemoryRequirementsInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImagePlaneMemoryRequirementsInfo where
        sizeOf ~_ = #{size VkImagePlaneMemoryRequirementsInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImagePlaneMemoryRequirementsInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImagePlaneMemoryRequirementsInfo where
        unsafeAddr (VkImagePlaneMemoryRequirementsInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImagePlaneMemoryRequirementsInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImagePlaneMemoryRequirementsInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImagePlaneMemoryRequirementsInfo where
        type StructFields VkImagePlaneMemoryRequirementsInfo =
             '["sType", "pNext", "planeAspect"] -- ' closing tick for hsc2hs
        type CUnionType VkImagePlaneMemoryRequirementsInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImagePlaneMemoryRequirementsInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImagePlaneMemoryRequirementsInfo =
             '[VkImageMemoryRequirementsInfo2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImagePlaneMemoryRequirementsInfo where
        type FieldType "sType" VkImagePlaneMemoryRequirementsInfo =
             VkStructureType
        type FieldOptional "sType" VkImagePlaneMemoryRequirementsInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImagePlaneMemoryRequirementsInfo =
             #{offset VkImagePlaneMemoryRequirementsInfo, sType}
        type FieldIsArray "sType" VkImagePlaneMemoryRequirementsInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImagePlaneMemoryRequirementsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImagePlaneMemoryRequirementsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImagePlaneMemoryRequirementsInfo where
        type FieldType "pNext" VkImagePlaneMemoryRequirementsInfo =
             Ptr Void
        type FieldOptional "pNext" VkImagePlaneMemoryRequirementsInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImagePlaneMemoryRequirementsInfo =
             #{offset VkImagePlaneMemoryRequirementsInfo, pNext}
        type FieldIsArray "pNext" VkImagePlaneMemoryRequirementsInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImagePlaneMemoryRequirementsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImagePlaneMemoryRequirementsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "planeAspect" VkImagePlaneMemoryRequirementsInfo where
        type FieldType "planeAspect" VkImagePlaneMemoryRequirementsInfo =
             VkImageAspectFlagBits
        type FieldOptional "planeAspect" VkImagePlaneMemoryRequirementsInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "planeAspect" VkImagePlaneMemoryRequirementsInfo =
             #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}
        type FieldIsArray "planeAspect" VkImagePlaneMemoryRequirementsInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}

instance {-# OVERLAPPING #-}
         CanReadField "planeAspect" VkImagePlaneMemoryRequirementsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}

instance {-# OVERLAPPING #-}
         CanWriteField "planeAspect" VkImagePlaneMemoryRequirementsInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}

instance Show VkImagePlaneMemoryRequirementsInfo where
        showsPrec d x
          = showString "VkImagePlaneMemoryRequirementsInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "planeAspect = " .
                            showsPrec d (getField @"planeAspect" x) . showChar '}'
