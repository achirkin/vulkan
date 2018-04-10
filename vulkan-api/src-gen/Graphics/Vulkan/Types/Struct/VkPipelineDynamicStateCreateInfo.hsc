#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineDynamicStateCreateInfo
       (VkPipelineDynamicStateCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineDynamicStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkDynamicState  (VkDynamicState)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineDynamicStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineDynamicStateCreateFlags    flags;
--   >     uint32_t               dynamicStateCount;
--   >     const VkDynamicState*  pDynamicStates;
--   > } VkPipelineDynamicStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo registry at www.khronos.org>
data VkPipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo## Addr##
                                                                          ByteArray##

instance Eq VkPipelineDynamicStateCreateInfo where
        (VkPipelineDynamicStateCreateInfo## a _) ==
          x@(VkPipelineDynamicStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDynamicStateCreateInfo where
        (VkPipelineDynamicStateCreateInfo## a _) `compare`
          x@(VkPipelineDynamicStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineDynamicStateCreateInfo where
        sizeOf ~_ = #{size VkPipelineDynamicStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDynamicStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineDynamicStateCreateInfo where
        unsafeAddr (VkPipelineDynamicStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineDynamicStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineDynamicStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineDynamicStateCreateInfo where
        type StructFields VkPipelineDynamicStateCreateInfo =
             '["sType", "pNext", "flags", "dynamicStateCount", "pDynamicStates"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineDynamicStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDynamicStateCreateInfo where
        type FieldType "sType" VkPipelineDynamicStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineDynamicStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineDynamicStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineDynamicStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDynamicStateCreateInfo where
        type FieldType "pNext" VkPipelineDynamicStateCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineDynamicStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineDynamicStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineDynamicStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDynamicStateCreateInfo where
        type FieldType "flags" VkPipelineDynamicStateCreateInfo =
             VkPipelineDynamicStateCreateFlags
        type FieldOptional "flags" VkPipelineDynamicStateCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineDynamicStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineDynamicStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "dynamicStateCount" VkPipelineDynamicStateCreateInfo where
        type FieldType "dynamicStateCount" VkPipelineDynamicStateCreateInfo
             = Word32
        type FieldOptional "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             =
             #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}
        type FieldIsArray "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

instance {-# OVERLAPPING #-}
         CanReadField "dynamicStateCount" VkPipelineDynamicStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

instance {-# OVERLAPPING #-}
         CanWriteField "dynamicStateCount" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

instance {-# OVERLAPPING #-}
         HasField "pDynamicStates" VkPipelineDynamicStateCreateInfo where
        type FieldType "pDynamicStates" VkPipelineDynamicStateCreateInfo =
             Ptr VkDynamicState
        type FieldOptional "pDynamicStates"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDynamicStates" VkPipelineDynamicStateCreateInfo
             =
             #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}
        type FieldIsArray "pDynamicStates" VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

instance {-# OVERLAPPING #-}
         CanReadField "pDynamicStates" VkPipelineDynamicStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

instance {-# OVERLAPPING #-}
         CanWriteField "pDynamicStates" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

instance Show VkPipelineDynamicStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineDynamicStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "dynamicStateCount = " .
                                  showsPrec d (getField @"dynamicStateCount" x) .
                                    showString ", " .
                                      showString "pDynamicStates = " .
                                        showsPrec d (getField @"pDynamicStates" x) . showChar '}'
