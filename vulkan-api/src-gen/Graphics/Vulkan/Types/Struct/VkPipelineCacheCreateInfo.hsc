#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineCacheCreateInfo
       (VkPipelineCacheCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineCacheCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineCacheCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCacheCreateFlags    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkPipelineCacheCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPipelineCacheCreateInfo.html VkPipelineCacheCreateInfo registry at www.khronos.org>
data VkPipelineCacheCreateInfo = VkPipelineCacheCreateInfo## Addr##
                                                            ByteArray##

instance Eq VkPipelineCacheCreateInfo where
        (VkPipelineCacheCreateInfo## a _) ==
          x@(VkPipelineCacheCreateInfo## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineCacheCreateInfo where
        (VkPipelineCacheCreateInfo## a _) `compare`
          x@(VkPipelineCacheCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineCacheCreateInfo where
        sizeOf ~_ = #{size VkPipelineCacheCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPipelineCacheCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineCacheCreateInfo where
        unsafeAddr (VkPipelineCacheCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineCacheCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineCacheCreateInfo## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineCacheCreateInfo where
        type StructFields VkPipelineCacheCreateInfo =
             '["sType", "pNext", "flags", "initialDataSize", "pInitialData"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineCacheCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineCacheCreateInfo where
        type FieldType "sType" VkPipelineCacheCreateInfo = VkStructureType
        type FieldOptional "sType" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineCacheCreateInfo where
        type FieldType "pNext" VkPipelineCacheCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineCacheCreateInfo where
        type FieldType "flags" VkPipelineCacheCreateInfo =
             VkPipelineCacheCreateFlags
        type FieldOptional "flags" VkPipelineCacheCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "initialDataSize" VkPipelineCacheCreateInfo where
        type FieldType "initialDataSize" VkPipelineCacheCreateInfo = CSize
        type FieldOptional "initialDataSize" VkPipelineCacheCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "initialDataSize" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, initialDataSize}
        type FieldIsArray "initialDataSize" VkPipelineCacheCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, initialDataSize}

instance {-# OVERLAPPING #-}
         CanReadField "initialDataSize" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, initialDataSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, initialDataSize}

instance {-# OVERLAPPING #-}
         CanWriteField "initialDataSize" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, initialDataSize}

instance {-# OVERLAPPING #-}
         HasField "pInitialData" VkPipelineCacheCreateInfo where
        type FieldType "pInitialData" VkPipelineCacheCreateInfo = Ptr Void
        type FieldOptional "pInitialData" VkPipelineCacheCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pInitialData" VkPipelineCacheCreateInfo =
             #{offset VkPipelineCacheCreateInfo, pInitialData}
        type FieldIsArray "pInitialData" VkPipelineCacheCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCacheCreateInfo, pInitialData}

instance {-# OVERLAPPING #-}
         CanReadField "pInitialData" VkPipelineCacheCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, pInitialData})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, pInitialData}

instance {-# OVERLAPPING #-}
         CanWriteField "pInitialData" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, pInitialData}

instance Show VkPipelineCacheCreateInfo where
        showsPrec d x
          = showString "VkPipelineCacheCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "initialDataSize = " .
                                  showsPrec d (getField @"initialDataSize" x) .
                                    showString ", " .
                                      showString "pInitialData = " .
                                        showsPrec d (getField @"pInitialData" x) . showChar '}'
