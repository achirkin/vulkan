#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkShaderModuleCreateInfo
       (VkShaderModuleCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkShaderModuleCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkShaderModuleCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkShaderModuleCreateFlags flags;
--   >     size_t                 codeSize;
--   >     const uint32_t*            pCode;
--   > } VkShaderModuleCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkShaderModuleCreateInfo.html VkShaderModuleCreateInfo registry at www.khronos.org>
data VkShaderModuleCreateInfo = VkShaderModuleCreateInfo## Addr##
                                                          ByteArray##

instance Eq VkShaderModuleCreateInfo where
        (VkShaderModuleCreateInfo## a _) ==
          x@(VkShaderModuleCreateInfo## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderModuleCreateInfo where
        (VkShaderModuleCreateInfo## a _) `compare`
          x@(VkShaderModuleCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderModuleCreateInfo where
        sizeOf ~_ = #{size VkShaderModuleCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderModuleCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkShaderModuleCreateInfo where
        unsafeAddr (VkShaderModuleCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderModuleCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderModuleCreateInfo## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderModuleCreateInfo where
        type StructFields VkShaderModuleCreateInfo =
             '["sType", "pNext", "flags", "codeSize", "pCode"] -- ' closing tick for hsc2hs
        type CUnionType VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkShaderModuleCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkShaderModuleCreateInfo
         where
        type VkSTypeMType VkShaderModuleCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkShaderModuleCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkShaderModuleCreateInfo where
        type FieldType "sType" VkShaderModuleCreateInfo = VkStructureType
        type FieldOptional "sType" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, sType}
        type FieldIsArray "sType" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkShaderModuleCreateInfo, sType}

instance CanReadField "sType" VkShaderModuleCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkShaderModuleCreateInfo
         where
        type VkPNextMType VkShaderModuleCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkShaderModuleCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkShaderModuleCreateInfo where
        type FieldType "pNext" VkShaderModuleCreateInfo = Ptr Void
        type FieldOptional "pNext" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, pNext}
        type FieldIsArray "pNext" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkShaderModuleCreateInfo, pNext}

instance CanReadField "pNext" VkShaderModuleCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkShaderModuleCreateInfo
         where
        type VkFlagsMType VkShaderModuleCreateInfo =
             VkShaderModuleCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkShaderModuleCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkShaderModuleCreateInfo where
        type FieldType "flags" VkShaderModuleCreateInfo =
             VkShaderModuleCreateFlags
        type FieldOptional "flags" VkShaderModuleCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, flags}
        type FieldIsArray "flags" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkShaderModuleCreateInfo, flags}

instance CanReadField "flags" VkShaderModuleCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkCodeSize VkShaderModuleCreateInfo
         where
        type VkCodeSizeMType VkShaderModuleCreateInfo = CSize

        {-# NOINLINE vkCodeSize #-}
        vkCodeSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, codeSize})

        {-# INLINE vkCodeSizeByteOffset #-}
        vkCodeSizeByteOffset ~_
          = #{offset VkShaderModuleCreateInfo, codeSize}

        {-# INLINE readVkCodeSize #-}
        readVkCodeSize p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, codeSize}

        {-# INLINE writeVkCodeSize #-}
        writeVkCodeSize p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, codeSize}

instance {-# OVERLAPPING #-}
         HasField "codeSize" VkShaderModuleCreateInfo where
        type FieldType "codeSize" VkShaderModuleCreateInfo = CSize
        type FieldOptional "codeSize" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "codeSize" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, codeSize}
        type FieldIsArray "codeSize" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleCreateInfo, codeSize}

instance CanReadField "codeSize" VkShaderModuleCreateInfo where
        {-# INLINE getField #-}
        getField = vkCodeSize

        {-# INLINE readField #-}
        readField = readVkCodeSize

instance CanWriteField "codeSize" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkCodeSize

instance {-# OVERLAPPING #-} HasVkPCode VkShaderModuleCreateInfo
         where
        type VkPCodeMType VkShaderModuleCreateInfo = Ptr Word32

        {-# NOINLINE vkPCode #-}
        vkPCode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, pCode})

        {-# INLINE vkPCodeByteOffset #-}
        vkPCodeByteOffset ~_
          = #{offset VkShaderModuleCreateInfo, pCode}

        {-# INLINE readVkPCode #-}
        readVkPCode p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, pCode}

        {-# INLINE writeVkPCode #-}
        writeVkPCode p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, pCode}

instance {-# OVERLAPPING #-}
         HasField "pCode" VkShaderModuleCreateInfo where
        type FieldType "pCode" VkShaderModuleCreateInfo = Ptr Word32
        type FieldOptional "pCode" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCode" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, pCode}
        type FieldIsArray "pCode" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkShaderModuleCreateInfo, pCode}

instance CanReadField "pCode" VkShaderModuleCreateInfo where
        {-# INLINE getField #-}
        getField = vkPCode

        {-# INLINE readField #-}
        readField = readVkPCode

instance CanWriteField "pCode" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPCode

instance Show VkShaderModuleCreateInfo where
        showsPrec d x
          = showString "VkShaderModuleCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkCodeSize = " .
                                  showsPrec d (vkCodeSize x) .
                                    showString ", " .
                                      showString "vkPCode = " .
                                        showsPrec d (vkPCode x) . showChar '}'
