#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGeneratedCommandsLimitsNVX
       (VkDeviceGeneratedCommandsLimitsNVX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGeneratedCommandsLimitsNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         maxIndirectCommandsLayoutTokenCount;
--   >     uint32_t                         maxObjectEntryCounts;
--   >     uint32_t                         minSequenceCountBufferOffsetAlignment;
--   >     uint32_t                         minSequenceIndexBufferOffsetAlignment;
--   >     uint32_t                         minCommandsTokenBufferOffsetAlignment;
--   > } VkDeviceGeneratedCommandsLimitsNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGeneratedCommandsLimitsNVX.html VkDeviceGeneratedCommandsLimitsNVX registry at www.khronos.org>
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX## Addr##
                                                                              ByteArray##

instance Eq VkDeviceGeneratedCommandsLimitsNVX where
        (VkDeviceGeneratedCommandsLimitsNVX## a _) ==
          x@(VkDeviceGeneratedCommandsLimitsNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGeneratedCommandsLimitsNVX where
        (VkDeviceGeneratedCommandsLimitsNVX## a _) `compare`
          x@(VkDeviceGeneratedCommandsLimitsNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGeneratedCommandsLimitsNVX where
        sizeOf ~_ = #{size VkDeviceGeneratedCommandsLimitsNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGeneratedCommandsLimitsNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGeneratedCommandsLimitsNVX where
        unsafeAddr (VkDeviceGeneratedCommandsLimitsNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGeneratedCommandsLimitsNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGeneratedCommandsLimitsNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGeneratedCommandsLimitsNVX where
        type StructFields VkDeviceGeneratedCommandsLimitsNVX =
             '["sType", "pNext", "maxIndirectCommandsLayoutTokenCount", -- ' closing tick for hsc2hs
               "maxObjectEntryCounts", "minSequenceCountBufferOffsetAlignment",
               "minSequenceIndexBufferOffsetAlignment",
               "minCommandsTokenBufferOffsetAlignment"]
        type CUnionType VkDeviceGeneratedCommandsLimitsNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGeneratedCommandsLimitsNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGeneratedCommandsLimitsNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGeneratedCommandsLimitsNVX where
        type FieldType "sType" VkDeviceGeneratedCommandsLimitsNVX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGeneratedCommandsLimitsNVX =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}
        type FieldIsArray "sType" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGeneratedCommandsLimitsNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGeneratedCommandsLimitsNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGeneratedCommandsLimitsNVX where
        type FieldType "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}
        type FieldIsArray "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGeneratedCommandsLimitsNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGeneratedCommandsLimitsNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}
        type FieldIsArray "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

instance {-# OVERLAPPING #-}
         HasField "maxObjectEntryCounts" VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}
        type FieldIsArray "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

instance {-# OVERLAPPING #-}
         CanReadField "maxObjectEntryCounts"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "maxObjectEntryCounts"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

instance {-# OVERLAPPING #-}
         HasField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}
        type FieldIsArray "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanReadField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}
        type FieldIsArray "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanReadField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}
        type FieldIsArray "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanReadField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

instance Show VkDeviceGeneratedCommandsLimitsNVX where
        showsPrec d x
          = showString "VkDeviceGeneratedCommandsLimitsNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxIndirectCommandsLayoutTokenCount = " .
                            showsPrec d (getField @"maxIndirectCommandsLayoutTokenCount" x) .
                              showString ", " .
                                showString "maxObjectEntryCounts = " .
                                  showsPrec d (getField @"maxObjectEntryCounts" x) .
                                    showString ", " .
                                      showString "minSequenceCountBufferOffsetAlignment = " .
                                        showsPrec d
                                          (getField @"minSequenceCountBufferOffsetAlignment" x)
                                          .
                                          showString ", " .
                                            showString "minSequenceIndexBufferOffsetAlignment = " .
                                              showsPrec d
                                                (getField @"minSequenceIndexBufferOffsetAlignment"
                                                   x)
                                                .
                                                showString ", " .
                                                  showString
                                                    "minCommandsTokenBufferOffsetAlignment = "
                                                    .
                                                    showsPrec d
                                                      (getField
                                                         @"minCommandsTokenBufferOffsetAlignment"
                                                         x)
                                                      . showChar '}'
