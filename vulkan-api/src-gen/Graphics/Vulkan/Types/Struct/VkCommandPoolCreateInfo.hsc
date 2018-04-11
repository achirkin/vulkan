#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCommandPoolCreateInfo
       (VkCommandPoolCreateInfo(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Base                                            (Addr##,
                                                                      ByteArray##,
                                                                      byteArrayContents##,
                                                                      plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCommandPoolCreateFlags (VkCommandPoolCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkCommandPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandPoolCreateFlags   flags;
--   >     uint32_t               queueFamilyIndex;
--   > } VkCommandPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandPoolCreateInfo VkCommandPoolCreateInfo registry at www.khronos.org>
data VkCommandPoolCreateInfo = VkCommandPoolCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkCommandPoolCreateInfo where
        (VkCommandPoolCreateInfo## a _) == x@(VkCommandPoolCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandPoolCreateInfo where
        (VkCommandPoolCreateInfo## a _) `compare`
          x@(VkCommandPoolCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandPoolCreateInfo where
        sizeOf ~_ = #{size VkCommandPoolCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandPoolCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandPoolCreateInfo where
        unsafeAddr (VkCommandPoolCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandPoolCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandPoolCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandPoolCreateInfo where
        type StructFields VkCommandPoolCreateInfo =
             '["sType", "pNext", "flags", "queueFamilyIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandPoolCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandPoolCreateInfo where
        type FieldType "sType" VkCommandPoolCreateInfo = VkStructureType
        type FieldOptional "sType" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, sType}
        type FieldIsArray "sType" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCommandPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandPoolCreateInfo where
        type FieldType "pNext" VkCommandPoolCreateInfo = Ptr Void
        type FieldOptional "pNext" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, pNext}
        type FieldIsArray "pNext" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCommandPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkCommandPoolCreateInfo where
        type FieldType "flags" VkCommandPoolCreateInfo =
             VkCommandPoolCreateFlags
        type FieldOptional "flags" VkCommandPoolCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, flags}
        type FieldIsArray "flags" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkCommandPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndex" VkCommandPoolCreateInfo where
        type FieldType "queueFamilyIndex" VkCommandPoolCreateInfo = Word32
        type FieldOptional "queueFamilyIndex" VkCommandPoolCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndex" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, queueFamilyIndex}
        type FieldIsArray "queueFamilyIndex" VkCommandPoolCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndex" VkCommandPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, queueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndex" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

instance Show VkCommandPoolCreateInfo where
        showsPrec d x
          = showString "VkCommandPoolCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "queueFamilyIndex = " .
                                  showsPrec d (getField @"queueFamilyIndex" x) . showChar '}'
