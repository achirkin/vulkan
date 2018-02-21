#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineCacheCreateInfo
       (VkPipelineCacheCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineCacheCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineCacheCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCacheCreateFlags    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkPipelineCacheCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineCacheCreateInfo.html VkPipelineCacheCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkPipelineCacheCreateInfo
         where
        type VkSTypeMType VkPipelineCacheCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineCacheCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, sType}

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

instance CanReadField "sType" VkPipelineCacheCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkPipelineCacheCreateInfo
         where
        type VkPNextMType VkPipelineCacheCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineCacheCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, pNext}

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

instance CanReadField "pNext" VkPipelineCacheCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkPipelineCacheCreateInfo
         where
        type VkFlagsMType VkPipelineCacheCreateInfo =
             VkPipelineCacheCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineCacheCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, flags}

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

instance CanReadField "flags" VkPipelineCacheCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPipelineCacheCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkInitialDataSize VkPipelineCacheCreateInfo where
        type VkInitialDataSizeMType VkPipelineCacheCreateInfo = CSize

        {-# NOINLINE vkInitialDataSize #-}
        vkInitialDataSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, initialDataSize})

        {-# INLINE vkInitialDataSizeByteOffset #-}
        vkInitialDataSizeByteOffset ~_
          = #{offset VkPipelineCacheCreateInfo, initialDataSize}

        {-# INLINE readVkInitialDataSize #-}
        readVkInitialDataSize p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, initialDataSize}

        {-# INLINE writeVkInitialDataSize #-}
        writeVkInitialDataSize p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, initialDataSize}

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

instance CanReadField "initialDataSize" VkPipelineCacheCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkInitialDataSize

        {-# INLINE readField #-}
        readField = readVkInitialDataSize

instance CanWriteField "initialDataSize" VkPipelineCacheCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkInitialDataSize

instance {-# OVERLAPPING #-}
         HasVkPInitialData VkPipelineCacheCreateInfo where
        type VkPInitialDataMType VkPipelineCacheCreateInfo = Ptr Void

        {-# NOINLINE vkPInitialData #-}
        vkPInitialData x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCacheCreateInfo, pInitialData})

        {-# INLINE vkPInitialDataByteOffset #-}
        vkPInitialDataByteOffset ~_
          = #{offset VkPipelineCacheCreateInfo, pInitialData}

        {-# INLINE readVkPInitialData #-}
        readVkPInitialData p
          = peekByteOff p #{offset VkPipelineCacheCreateInfo, pInitialData}

        {-# INLINE writeVkPInitialData #-}
        writeVkPInitialData p
          = pokeByteOff p #{offset VkPipelineCacheCreateInfo, pInitialData}

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

instance CanReadField "pInitialData" VkPipelineCacheCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPInitialData

        {-# INLINE readField #-}
        readField = readVkPInitialData

instance CanWriteField "pInitialData" VkPipelineCacheCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPInitialData

instance Show VkPipelineCacheCreateInfo where
        showsPrec d x
          = showString "VkPipelineCacheCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkInitialDataSize = " .
                                  showsPrec d (vkInitialDataSize x) .
                                    showString ", " .
                                      showString "vkPInitialData = " .
                                        showsPrec d (vkPInitialData x) . showChar '}'
