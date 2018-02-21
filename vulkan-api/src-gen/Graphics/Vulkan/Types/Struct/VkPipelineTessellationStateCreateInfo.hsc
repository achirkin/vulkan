#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
       (VkPipelineTessellationStateCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineTessellationStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineTessellationStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineTessellationStateCreateFlags    flags;
--   >     uint32_t               patchControlPoints;
--   > } VkPipelineTessellationStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineTessellationStateCreateInfo.html VkPipelineTessellationStateCreateInfo registry at www.khronos.org>
data VkPipelineTessellationStateCreateInfo = VkPipelineTessellationStateCreateInfo## Addr##
                                                                                    ByteArray##

instance Eq VkPipelineTessellationStateCreateInfo where
        (VkPipelineTessellationStateCreateInfo## a _) ==
          x@(VkPipelineTessellationStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineTessellationStateCreateInfo where
        (VkPipelineTessellationStateCreateInfo## a _) `compare`
          x@(VkPipelineTessellationStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineTessellationStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineTessellationStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineTessellationStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineTessellationStateCreateInfo
         where
        unsafeAddr (VkPipelineTessellationStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineTessellationStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineTessellationStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineTessellationStateCreateInfo where
        type StructFields VkPipelineTessellationStateCreateInfo =
             '["sType", "pNext", "flags", "patchControlPoints"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineTessellationStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineTessellationStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineTessellationStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineTessellationStateCreateInfo where
        type VkSTypeMType VkPipelineTessellationStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineTessellationStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineTessellationStateCreateInfo where
        type FieldType "sType" VkPipelineTessellationStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineTessellationStateCreateInfo =
             #{offset VkPipelineTessellationStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationStateCreateInfo, sType}

instance CanReadField "sType" VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineTessellationStateCreateInfo where
        type VkPNextMType VkPipelineTessellationStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineTessellationStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineTessellationStateCreateInfo where
        type FieldType "pNext" VkPipelineTessellationStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineTessellationStateCreateInfo =
             #{offset VkPipelineTessellationStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationStateCreateInfo, pNext}

instance CanReadField "pNext" VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineTessellationStateCreateInfo where
        type VkFlagsMType VkPipelineTessellationStateCreateInfo =
             VkPipelineTessellationStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineTessellationStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineTessellationStateCreateInfo where
        type FieldType "flags" VkPipelineTessellationStateCreateInfo =
             VkPipelineTessellationStateCreateFlags
        type FieldOptional "flags" VkPipelineTessellationStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineTessellationStateCreateInfo =
             #{offset VkPipelineTessellationStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineTessellationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationStateCreateInfo, flags}

instance CanReadField "flags" VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkPatchControlPoints VkPipelineTessellationStateCreateInfo where
        type VkPatchControlPointsMType
               VkPipelineTessellationStateCreateInfo
             = Word32

        {-# NOINLINE vkPatchControlPoints #-}
        vkPatchControlPoints x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints})

        {-# INLINE vkPatchControlPointsByteOffset #-}
        vkPatchControlPointsByteOffset ~_
          = #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

        {-# INLINE readVkPatchControlPoints #-}
        readVkPatchControlPoints p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

        {-# INLINE writeVkPatchControlPoints #-}
        writeVkPatchControlPoints p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

instance {-# OVERLAPPING #-}
         HasField "patchControlPoints" VkPipelineTessellationStateCreateInfo
         where
        type FieldType "patchControlPoints"
               VkPipelineTessellationStateCreateInfo
             = Word32
        type FieldOptional "patchControlPoints"
               VkPipelineTessellationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "patchControlPoints"
               VkPipelineTessellationStateCreateInfo
             =
             #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}
        type FieldIsArray "patchControlPoints"
               VkPipelineTessellationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

instance CanReadField "patchControlPoints"
           VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPatchControlPoints

        {-# INLINE readField #-}
        readField = readVkPatchControlPoints

instance CanWriteField "patchControlPoints"
           VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPatchControlPoints

instance Show VkPipelineTessellationStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineTessellationStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkPatchControlPoints = " .
                                  showsPrec d (vkPatchControlPoints x) . showChar '}'
