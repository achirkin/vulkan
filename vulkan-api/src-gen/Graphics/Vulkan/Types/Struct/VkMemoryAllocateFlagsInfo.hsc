#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfo
       (VkMemoryAllocateFlagsInfo(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Base                                          (Addr##,
                                                                    ByteArray##,
                                                                    byteArrayContents##,
                                                                    plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlags  (VkMemoryAllocateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryAllocateFlagsInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMemoryAllocateFlags flags;
--   >     uint32_t                         deviceMask;
--   > } VkMemoryAllocateFlagsInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryAllocateFlagsInfo VkMemoryAllocateFlagsInfo registry at www.khronos.org>
data VkMemoryAllocateFlagsInfo = VkMemoryAllocateFlagsInfo## Addr##
                                                            ByteArray##

instance Eq VkMemoryAllocateFlagsInfo where
        (VkMemoryAllocateFlagsInfo## a _) ==
          x@(VkMemoryAllocateFlagsInfo## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateFlagsInfo where
        (VkMemoryAllocateFlagsInfo## a _) `compare`
          x@(VkMemoryAllocateFlagsInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateFlagsInfo where
        sizeOf ~_ = #{size VkMemoryAllocateFlagsInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryAllocateFlagsInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryAllocateFlagsInfo where
        unsafeAddr (VkMemoryAllocateFlagsInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryAllocateFlagsInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryAllocateFlagsInfo## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryAllocateFlagsInfo where
        type StructFields VkMemoryAllocateFlagsInfo =
             '["sType", "pNext", "flags", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryAllocateFlagsInfo =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryAllocateFlagsInfo where
        type FieldType "sType" VkMemoryAllocateFlagsInfo = VkStructureType
        type FieldOptional "sType" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryAllocateFlagsInfo =
             #{offset VkMemoryAllocateFlagsInfo, sType}
        type FieldIsArray "sType" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryAllocateFlagsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryAllocateFlagsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryAllocateFlagsInfo where
        type FieldType "pNext" VkMemoryAllocateFlagsInfo = Ptr Void
        type FieldOptional "pNext" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryAllocateFlagsInfo =
             #{offset VkMemoryAllocateFlagsInfo, pNext}
        type FieldIsArray "pNext" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryAllocateFlagsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryAllocateFlagsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMemoryAllocateFlagsInfo where
        type FieldType "flags" VkMemoryAllocateFlagsInfo =
             VkMemoryAllocateFlags
        type FieldOptional "flags" VkMemoryAllocateFlagsInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMemoryAllocateFlagsInfo =
             #{offset VkMemoryAllocateFlagsInfo, flags}
        type FieldIsArray "flags" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkMemoryAllocateFlagsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkMemoryAllocateFlagsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkMemoryAllocateFlagsInfo where
        type FieldType "deviceMask" VkMemoryAllocateFlagsInfo = Word32
        type FieldOptional "deviceMask" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkMemoryAllocateFlagsInfo =
             #{offset VkMemoryAllocateFlagsInfo, deviceMask}
        type FieldIsArray "deviceMask" VkMemoryAllocateFlagsInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkMemoryAllocateFlagsInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfo, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkMemoryAllocateFlagsInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfo, deviceMask}

instance Show VkMemoryAllocateFlagsInfo where
        showsPrec d x
          = showString "VkMemoryAllocateFlagsInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "deviceMask = " .
                                  showsPrec d (getField @"deviceMask" x) . showChar '}'
