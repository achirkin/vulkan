#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorPoolCreateInfo
       (VkDescriptorPoolCreateInfo(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorPoolCreateFlags (VkDescriptorPoolCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDescriptorPoolSize      (VkDescriptorPoolSize)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorPoolCreateFlags  flags;
--   >     uint32_t               maxSets;
--   >     uint32_t               poolSizeCount;
--   >     const VkDescriptorPoolSize* pPoolSizes;
--   > } VkDescriptorPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDescriptorPoolCreateInfo.html VkDescriptorPoolCreateInfo registry at www.khronos.org>
data VkDescriptorPoolCreateInfo = VkDescriptorPoolCreateInfo## Addr##
                                                              ByteArray##

instance Eq VkDescriptorPoolCreateInfo where
        (VkDescriptorPoolCreateInfo## a _) ==
          x@(VkDescriptorPoolCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorPoolCreateInfo where
        (VkDescriptorPoolCreateInfo## a _) `compare`
          x@(VkDescriptorPoolCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorPoolCreateInfo where
        sizeOf ~_ = #{size VkDescriptorPoolCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorPoolCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorPoolCreateInfo where
        unsafeAddr (VkDescriptorPoolCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorPoolCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorPoolCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorPoolCreateInfo where
        type StructFields VkDescriptorPoolCreateInfo =
             '["sType", "pNext", "flags", "maxSets", "poolSizeCount", -- ' closing tick for hsc2hs
               "pPoolSizes"]
        type CUnionType VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorPoolCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorPoolCreateInfo where
        type FieldType "sType" VkDescriptorPoolCreateInfo = VkStructureType
        type FieldOptional "sType" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, sType}
        type FieldIsArray "sType" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorPoolCreateInfo where
        type FieldType "pNext" VkDescriptorPoolCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorPoolCreateInfo where
        type FieldType "flags" VkDescriptorPoolCreateInfo =
             VkDescriptorPoolCreateFlags
        type FieldOptional "flags" VkDescriptorPoolCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, flags}
        type FieldIsArray "flags" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "maxSets" VkDescriptorPoolCreateInfo where
        type FieldType "maxSets" VkDescriptorPoolCreateInfo = Word32
        type FieldOptional "maxSets" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSets" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, maxSets}
        type FieldIsArray "maxSets" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, maxSets}

instance {-# OVERLAPPING #-}
         CanReadField "maxSets" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, maxSets})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, maxSets}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSets" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, maxSets}

instance {-# OVERLAPPING #-}
         HasField "poolSizeCount" VkDescriptorPoolCreateInfo where
        type FieldType "poolSizeCount" VkDescriptorPoolCreateInfo = Word32
        type FieldOptional "poolSizeCount" VkDescriptorPoolCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "poolSizeCount" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, poolSizeCount}
        type FieldIsArray "poolSizeCount" VkDescriptorPoolCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

instance {-# OVERLAPPING #-}
         CanReadField "poolSizeCount" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, poolSizeCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

instance {-# OVERLAPPING #-}
         CanWriteField "poolSizeCount" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

instance {-# OVERLAPPING #-}
         HasField "pPoolSizes" VkDescriptorPoolCreateInfo where
        type FieldType "pPoolSizes" VkDescriptorPoolCreateInfo =
             Ptr VkDescriptorPoolSize
        type FieldOptional "pPoolSizes" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPoolSizes" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, pPoolSizes}
        type FieldIsArray "pPoolSizes" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

instance {-# OVERLAPPING #-}
         CanReadField "pPoolSizes" VkDescriptorPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, pPoolSizes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

instance {-# OVERLAPPING #-}
         CanWriteField "pPoolSizes" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

instance Show VkDescriptorPoolCreateInfo where
        showsPrec d x
          = showString "VkDescriptorPoolCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "maxSets = " .
                                  showsPrec d (getField @"maxSets" x) .
                                    showString ", " .
                                      showString "poolSizeCount = " .
                                        showsPrec d (getField @"poolSizeCount" x) .
                                          showString ", " .
                                            showString "pPoolSizes = " .
                                              showsPrec d (getField @"pPoolSizes" x) . showChar '}'
