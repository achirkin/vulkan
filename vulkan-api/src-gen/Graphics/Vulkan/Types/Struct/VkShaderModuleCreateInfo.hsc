#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkShaderModuleCreateInfo
       (VkShaderModuleCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkShaderModuleCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkShaderModuleCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkShaderModuleCreateFlags flags;
--   >     size_t                 codeSize;
--   >     const uint32_t*            pCode;
--   > } VkShaderModuleCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkShaderModuleCreateInfoVkShaderModuleCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "codeSize" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, codeSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, codeSize}

instance {-# OVERLAPPING #-}
         CanWriteField "codeSize" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, codeSize}

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

instance {-# OVERLAPPING #-}
         CanReadField "pCode" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, pCode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, pCode}

instance {-# OVERLAPPING #-}
         CanWriteField "pCode" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, pCode}

instance Show VkShaderModuleCreateInfo where
        showsPrec d x
          = showString "VkShaderModuleCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "codeSize = " .
                                  showsPrec d (getField @"codeSize" x) .
                                    showString ", " .
                                      showString "pCode = " .
                                        showsPrec d (getField @"pCode" x) . showChar '}'
