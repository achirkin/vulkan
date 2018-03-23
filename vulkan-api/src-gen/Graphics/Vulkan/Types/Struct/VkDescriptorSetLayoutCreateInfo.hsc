#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo
       (VkDescriptorSetLayoutCreateInfo(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags (VkDescriptorSetLayoutCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding   (VkDescriptorSetLayoutBinding)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetLayoutCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorSetLayoutCreateFlags    flags;
--   >     uint32_t               bindingCount;
--   >     const VkDescriptorSetLayoutBinding* pBindings;
--   > } VkDescriptorSetLayoutCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDescriptorSetLayoutCreateInfo.html VkDescriptorSetLayoutCreateInfo registry at www.khronos.org>
data VkDescriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkDescriptorSetLayoutCreateInfo where
        (VkDescriptorSetLayoutCreateInfo## a _) ==
          x@(VkDescriptorSetLayoutCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutCreateInfo where
        (VkDescriptorSetLayoutCreateInfo## a _) `compare`
          x@(VkDescriptorSetLayoutCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutCreateInfo where
        sizeOf ~_ = #{size VkDescriptorSetLayoutCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetLayoutCreateInfo where
        unsafeAddr (VkDescriptorSetLayoutCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetLayoutCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetLayoutCreateInfo where
        type StructFields VkDescriptorSetLayoutCreateInfo =
             '["sType", "pNext", "flags", "bindingCount", "pBindings"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetLayoutCreateInfo where
        type FieldType "sType" VkDescriptorSetLayoutCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, sType}
        type FieldIsArray "sType" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetLayoutCreateInfo where
        type FieldType "pNext" VkDescriptorSetLayoutCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorSetLayoutCreateInfo where
        type FieldType "flags" VkDescriptorSetLayoutCreateInfo =
             VkDescriptorSetLayoutCreateFlags
        type FieldOptional "flags" VkDescriptorSetLayoutCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, flags}
        type FieldIsArray "flags" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "bindingCount" VkDescriptorSetLayoutCreateInfo where
        type FieldType "bindingCount" VkDescriptorSetLayoutCreateInfo =
             Word32
        type FieldOptional "bindingCount" VkDescriptorSetLayoutCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "bindingCount" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}
        type FieldIsArray "bindingCount" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindingCount" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, bindingCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindingCount" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

instance {-# OVERLAPPING #-}
         HasField "pBindings" VkDescriptorSetLayoutCreateInfo where
        type FieldType "pBindings" VkDescriptorSetLayoutCreateInfo =
             Ptr VkDescriptorSetLayoutBinding
        type FieldOptional "pBindings" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pBindings" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, pBindings}
        type FieldIsArray "pBindings" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

instance {-# OVERLAPPING #-}
         CanReadField "pBindings" VkDescriptorSetLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, pBindings})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

instance {-# OVERLAPPING #-}
         CanWriteField "pBindings" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

instance Show VkDescriptorSetLayoutCreateInfo where
        showsPrec d x
          = showString "VkDescriptorSetLayoutCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "bindingCount = " .
                                  showsPrec d (getField @"bindingCount" x) .
                                    showString ", " .
                                      showString "pBindings = " .
                                        showsPrec d (getField @"pBindings" x) . showChar '}'
