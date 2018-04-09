#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
       (VkPipelineTessellationStateCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineTessellationStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineTessellationStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineTessellationStateCreateFlags    flags;
--   >     uint32_t               patchControlPoints;
--   > } VkPipelineTessellationStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineTessellationStateCreateInfoVkPipelineTessellationStateCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineTessellationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineTessellationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineTessellationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineTessellationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineTessellationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineTessellationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "patchControlPoints"
           VkPipelineTessellationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

instance {-# OVERLAPPING #-}
         CanWriteField "patchControlPoints"
           VkPipelineTessellationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationStateCreateInfo, patchControlPoints}

instance Show VkPipelineTessellationStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineTessellationStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "patchControlPoints = " .
                                  showsPrec d (getField @"patchControlPoints" x) . showChar '}'
