#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkShaderResourceUsageAMD
       (VkShaderResourceUsageAMD(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

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

instance {-# OVERLAPPING #-}
         CanReadField "numUsedVgprs" VkShaderResourceUsageAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedVgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numUsedVgprs" VkShaderResourceUsageAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

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

instance {-# OVERLAPPING #-}
         CanReadField "numUsedSgprs" VkShaderResourceUsageAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedSgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numUsedSgprs" VkShaderResourceUsageAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

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

instance {-# OVERLAPPING #-}
         CanReadField "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance {-# OVERLAPPING #-}
         CanWriteField "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

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

instance {-# OVERLAPPING #-}
         CanReadField "ldsUsageSizeInBytes" VkShaderResourceUsageAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance {-# OVERLAPPING #-}
         CanWriteField "ldsUsageSizeInBytes" VkShaderResourceUsageAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

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

instance {-# OVERLAPPING #-}
         CanReadField "scratchMemUsageInBytes" VkShaderResourceUsageAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance {-# OVERLAPPING #-}
         CanWriteField "scratchMemUsageInBytes" VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance Show VkShaderResourceUsageAMD where
        showsPrec d x
          = showString "VkShaderResourceUsageAMD {" .
              showString "numUsedVgprs = " .
                showsPrec d (getField @"numUsedVgprs" x) .
                  showString ", " .
                    showString "numUsedSgprs = " .
                      showsPrec d (getField @"numUsedSgprs" x) .
                        showString ", " .
                          showString "ldsSizePerLocalWorkGroup = " .
                            showsPrec d (getField @"ldsSizePerLocalWorkGroup" x) .
                              showString ", " .
                                showString "ldsUsageSizeInBytes = " .
                                  showsPrec d (getField @"ldsUsageSizeInBytes" x) .
                                    showString ", " .
                                      showString "scratchMemUsageInBytes = " .
                                        showsPrec d (getField @"scratchMemUsageInBytes" x) .
                                          showChar '}'
