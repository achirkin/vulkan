#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo
       (VkCommandBufferBeginInfo(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Base                                                    (Addr##,
                                                                              ByteArray##,
                                                                              byteArrayContents##,
                                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags        (VkCommandBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo (VkCommandBufferInheritanceInfo)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkCommandBufferBeginInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandBufferUsageFlags  flags;
--   >     const VkCommandBufferInheritanceInfo*       pInheritanceInfo;
--   > } VkCommandBufferBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferBeginInfo VkCommandBufferBeginInfo registry at www.khronos.org>
data VkCommandBufferBeginInfo = VkCommandBufferBeginInfo## Addr##
                                                          ByteArray##

instance Eq VkCommandBufferBeginInfo where
        (VkCommandBufferBeginInfo## a _) ==
          x@(VkCommandBufferBeginInfo## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandBufferBeginInfo where
        (VkCommandBufferBeginInfo## a _) `compare`
          x@(VkCommandBufferBeginInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandBufferBeginInfo where
        sizeOf ~_ = #{size VkCommandBufferBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandBufferBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandBufferBeginInfo where
        unsafeAddr (VkCommandBufferBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandBufferBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandBufferBeginInfo## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandBufferBeginInfo where
        type StructFields VkCommandBufferBeginInfo =
             '["sType", "pNext", "flags", "pInheritanceInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandBufferBeginInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandBufferBeginInfo where
        type FieldType "sType" VkCommandBufferBeginInfo = VkStructureType
        type FieldOptional "sType" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, sType}
        type FieldIsArray "sType" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandBufferBeginInfo where
        type FieldType "pNext" VkCommandBufferBeginInfo = Ptr Void
        type FieldOptional "pNext" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, pNext}
        type FieldIsArray "pNext" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkCommandBufferBeginInfo where
        type FieldType "flags" VkCommandBufferBeginInfo =
             VkCommandBufferUsageFlags
        type FieldOptional "flags" VkCommandBufferBeginInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, flags}
        type FieldIsArray "flags" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "pInheritanceInfo" VkCommandBufferBeginInfo where
        type FieldType "pInheritanceInfo" VkCommandBufferBeginInfo =
             Ptr VkCommandBufferInheritanceInfo
        type FieldOptional "pInheritanceInfo" VkCommandBufferBeginInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pInheritanceInfo" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, pInheritanceInfo}
        type FieldIsArray "pInheritanceInfo" VkCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

instance {-# OVERLAPPING #-}
         CanReadField "pInheritanceInfo" VkCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, pInheritanceInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "pInheritanceInfo" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

instance Show VkCommandBufferBeginInfo where
        showsPrec d x
          = showString "VkCommandBufferBeginInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "pInheritanceInfo = " .
                                  showsPrec d (getField @"pInheritanceInfo" x) . showChar '}'
