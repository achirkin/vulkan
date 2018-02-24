#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkShaderResourceUsageAMD
       (VkShaderResourceUsageAMD(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkShaderResourceUsageAMD {
--   >     uint32_t numUsedVgprs;
--   >     uint32_t numUsedSgprs;
--   >     uint32_t ldsSizePerLocalWorkGroup;
--   >     size_t ldsUsageSizeInBytes;
--   >     size_t scratchMemUsageInBytes;
--   > } VkShaderResourceUsageAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkShaderResourceUsageAMD.html VkShaderResourceUsageAMD registry at www.khronos.org>
data VkShaderResourceUsageAMD = VkShaderResourceUsageAMD## Addr##
                                                          ByteArray##

instance Eq VkShaderResourceUsageAMD where
        (VkShaderResourceUsageAMD## a _) ==
          x@(VkShaderResourceUsageAMD## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderResourceUsageAMD where
        (VkShaderResourceUsageAMD## a _) `compare`
          x@(VkShaderResourceUsageAMD## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderResourceUsageAMD where
        sizeOf ~_ = #{size VkShaderResourceUsageAMD}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderResourceUsageAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkShaderResourceUsageAMD where
        unsafeAddr (VkShaderResourceUsageAMD## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderResourceUsageAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderResourceUsageAMD## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderResourceUsageAMD where
        type StructFields VkShaderResourceUsageAMD =
             '["numUsedVgprs", "numUsedSgprs", "ldsSizePerLocalWorkGroup", -- ' closing tick for hsc2hs
               "ldsUsageSizeInBytes", "scratchMemUsageInBytes"]
        type CUnionType VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderResourceUsageAMD = 'True -- ' closing tick for hsc2hs
        type StructExtends VkShaderResourceUsageAMD = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkNumUsedVgprs VkShaderResourceUsageAMD where
        type VkNumUsedVgprsMType VkShaderResourceUsageAMD = Word32

        {-# NOINLINE vkNumUsedVgprs #-}
        vkNumUsedVgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedVgprs})

        {-# INLINE vkNumUsedVgprsByteOffset #-}
        vkNumUsedVgprsByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, numUsedVgprs}

        {-# INLINE readVkNumUsedVgprs #-}
        readVkNumUsedVgprs p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

        {-# INLINE writeVkNumUsedVgprs #-}
        writeVkNumUsedVgprs p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance {-# OVERLAPPING #-}
         HasField "numUsedVgprs" VkShaderResourceUsageAMD where
        type FieldType "numUsedVgprs" VkShaderResourceUsageAMD = Word32
        type FieldOptional "numUsedVgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type FieldOffset "numUsedVgprs" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, numUsedVgprs}
        type FieldIsArray "numUsedVgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance CanReadField "numUsedVgprs" VkShaderResourceUsageAMD where
        {-# INLINE getField #-}
        getField = vkNumUsedVgprs

        {-# INLINE readField #-}
        readField = readVkNumUsedVgprs

instance CanWriteField "numUsedVgprs" VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkNumUsedVgprs

instance {-# OVERLAPPING #-}
         HasVkNumUsedSgprs VkShaderResourceUsageAMD where
        type VkNumUsedSgprsMType VkShaderResourceUsageAMD = Word32

        {-# NOINLINE vkNumUsedSgprs #-}
        vkNumUsedSgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedSgprs})

        {-# INLINE vkNumUsedSgprsByteOffset #-}
        vkNumUsedSgprsByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, numUsedSgprs}

        {-# INLINE readVkNumUsedSgprs #-}
        readVkNumUsedSgprs p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

        {-# INLINE writeVkNumUsedSgprs #-}
        writeVkNumUsedSgprs p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance {-# OVERLAPPING #-}
         HasField "numUsedSgprs" VkShaderResourceUsageAMD where
        type FieldType "numUsedSgprs" VkShaderResourceUsageAMD = Word32
        type FieldOptional "numUsedSgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type FieldOffset "numUsedSgprs" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, numUsedSgprs}
        type FieldIsArray "numUsedSgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance CanReadField "numUsedSgprs" VkShaderResourceUsageAMD where
        {-# INLINE getField #-}
        getField = vkNumUsedSgprs

        {-# INLINE readField #-}
        readField = readVkNumUsedSgprs

instance CanWriteField "numUsedSgprs" VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkNumUsedSgprs

instance {-# OVERLAPPING #-}
         HasVkLdsSizePerLocalWorkGroup VkShaderResourceUsageAMD where
        type VkLdsSizePerLocalWorkGroupMType VkShaderResourceUsageAMD =
             Word32

        {-# NOINLINE vkLdsSizePerLocalWorkGroup #-}
        vkLdsSizePerLocalWorkGroup x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup})

        {-# INLINE vkLdsSizePerLocalWorkGroupByteOffset #-}
        vkLdsSizePerLocalWorkGroupByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

        {-# INLINE readVkLdsSizePerLocalWorkGroup #-}
        readVkLdsSizePerLocalWorkGroup p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

        {-# INLINE writeVkLdsSizePerLocalWorkGroup #-}
        writeVkLdsSizePerLocalWorkGroup p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance {-# OVERLAPPING #-}
         HasField "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD where
        type FieldType "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD
             = Word32
        type FieldOptional "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             =
             #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}
        type FieldIsArray "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance CanReadField "ldsSizePerLocalWorkGroup"
           VkShaderResourceUsageAMD
         where
        {-# INLINE getField #-}
        getField = vkLdsSizePerLocalWorkGroup

        {-# INLINE readField #-}
        readField = readVkLdsSizePerLocalWorkGroup

instance CanWriteField "ldsSizePerLocalWorkGroup"
           VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkLdsSizePerLocalWorkGroup

instance {-# OVERLAPPING #-}
         HasVkLdsUsageSizeInBytes VkShaderResourceUsageAMD where
        type VkLdsUsageSizeInBytesMType VkShaderResourceUsageAMD = CSize

        {-# NOINLINE vkLdsUsageSizeInBytes #-}
        vkLdsUsageSizeInBytes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes})

        {-# INLINE vkLdsUsageSizeInBytesByteOffset #-}
        vkLdsUsageSizeInBytesByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

        {-# INLINE readVkLdsUsageSizeInBytes #-}
        readVkLdsUsageSizeInBytes p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

        {-# INLINE writeVkLdsUsageSizeInBytes #-}
        writeVkLdsUsageSizeInBytes p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance {-# OVERLAPPING #-}
         HasField "ldsUsageSizeInBytes" VkShaderResourceUsageAMD where
        type FieldType "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             CSize
        type FieldOptional "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}
        type FieldIsArray "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance CanReadField "ldsUsageSizeInBytes"
           VkShaderResourceUsageAMD
         where
        {-# INLINE getField #-}
        getField = vkLdsUsageSizeInBytes

        {-# INLINE readField #-}
        readField = readVkLdsUsageSizeInBytes

instance CanWriteField "ldsUsageSizeInBytes"
           VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkLdsUsageSizeInBytes

instance {-# OVERLAPPING #-}
         HasVkScratchMemUsageInBytes VkShaderResourceUsageAMD where
        type VkScratchMemUsageInBytesMType VkShaderResourceUsageAMD = CSize

        {-# NOINLINE vkScratchMemUsageInBytes #-}
        vkScratchMemUsageInBytes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes})

        {-# INLINE vkScratchMemUsageInBytesByteOffset #-}
        vkScratchMemUsageInBytesByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

        {-# INLINE readVkScratchMemUsageInBytes #-}
        readVkScratchMemUsageInBytes p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

        {-# INLINE writeVkScratchMemUsageInBytes #-}
        writeVkScratchMemUsageInBytes p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance {-# OVERLAPPING #-}
         HasField "scratchMemUsageInBytes" VkShaderResourceUsageAMD where
        type FieldType "scratchMemUsageInBytes" VkShaderResourceUsageAMD =
             CSize
        type FieldOptional "scratchMemUsageInBytes"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "scratchMemUsageInBytes" VkShaderResourceUsageAMD
             =
             #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}
        type FieldIsArray "scratchMemUsageInBytes" VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance CanReadField "scratchMemUsageInBytes"
           VkShaderResourceUsageAMD
         where
        {-# INLINE getField #-}
        getField = vkScratchMemUsageInBytes

        {-# INLINE readField #-}
        readField = readVkScratchMemUsageInBytes

instance CanWriteField "scratchMemUsageInBytes"
           VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkScratchMemUsageInBytes

instance Show VkShaderResourceUsageAMD where
        showsPrec d x
          = showString "VkShaderResourceUsageAMD {" .
              showString "vkNumUsedVgprs = " .
                showsPrec d (vkNumUsedVgprs x) .
                  showString ", " .
                    showString "vkNumUsedSgprs = " .
                      showsPrec d (vkNumUsedSgprs x) .
                        showString ", " .
                          showString "vkLdsSizePerLocalWorkGroup = " .
                            showsPrec d (vkLdsSizePerLocalWorkGroup x) .
                              showString ", " .
                                showString "vkLdsUsageSizeInBytes = " .
                                  showsPrec d (vkLdsUsageSizeInBytes x) .
                                    showString ", " .
                                      showString "vkScratchMemUsageInBytes = " .
                                        showsPrec d (vkScratchMemUsageInBytes x) . showChar '}'
