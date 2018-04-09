#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCmdReserveSpaceForCommandsInfoNVX
       (VkCmdReserveSpaceForCommandsInfoNVX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkIndirectCommandsLayoutNVX,
                                                             VkObjectTableNVX)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkCmdReserveSpaceForCommandsInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkObjectTableNVX                                         objectTable;
--   >     VkIndirectCommandsLayoutNVX                              indirectCommandsLayout;
--   >     uint32_t                                                 maxSequencesCount;
--   > } VkCmdReserveSpaceForCommandsInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkCmdReserveSpaceForCommandsInfoNVXVkCmdReserveSpaceForCommandsInfoNVX registry at www.khronos.org>
data VkCmdReserveSpaceForCommandsInfoNVX = VkCmdReserveSpaceForCommandsInfoNVX## Addr##
                                                                                ByteArray##

instance Eq VkCmdReserveSpaceForCommandsInfoNVX where
        (VkCmdReserveSpaceForCommandsInfoNVX## a _) ==
          x@(VkCmdReserveSpaceForCommandsInfoNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCmdReserveSpaceForCommandsInfoNVX where
        (VkCmdReserveSpaceForCommandsInfoNVX## a _) `compare`
          x@(VkCmdReserveSpaceForCommandsInfoNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCmdReserveSpaceForCommandsInfoNVX where
        sizeOf ~_ = #{size VkCmdReserveSpaceForCommandsInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkCmdReserveSpaceForCommandsInfoNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCmdReserveSpaceForCommandsInfoNVX
         where
        unsafeAddr (VkCmdReserveSpaceForCommandsInfoNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCmdReserveSpaceForCommandsInfoNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCmdReserveSpaceForCommandsInfoNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCmdReserveSpaceForCommandsInfoNVX where
        type StructFields VkCmdReserveSpaceForCommandsInfoNVX =
             '["sType", "pNext", "objectTable", "indirectCommandsLayout", -- ' closing tick for hsc2hs
               "maxSequencesCount"]
        type CUnionType VkCmdReserveSpaceForCommandsInfoNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCmdReserveSpaceForCommandsInfoNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCmdReserveSpaceForCommandsInfoNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCmdReserveSpaceForCommandsInfoNVX where
        type FieldType "sType" VkCmdReserveSpaceForCommandsInfoNVX =
             VkStructureType
        type FieldOptional "sType" VkCmdReserveSpaceForCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCmdReserveSpaceForCommandsInfoNVX =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}
        type FieldIsArray "sType" VkCmdReserveSpaceForCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCmdReserveSpaceForCommandsInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCmdReserveSpaceForCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCmdReserveSpaceForCommandsInfoNVX where
        type FieldType "pNext" VkCmdReserveSpaceForCommandsInfoNVX =
             Ptr Void
        type FieldOptional "pNext" VkCmdReserveSpaceForCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCmdReserveSpaceForCommandsInfoNVX =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}
        type FieldIsArray "pNext" VkCmdReserveSpaceForCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCmdReserveSpaceForCommandsInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCmdReserveSpaceForCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectTable" VkCmdReserveSpaceForCommandsInfoNVX where
        type FieldType "objectTable" VkCmdReserveSpaceForCommandsInfoNVX =
             VkObjectTableNVX
        type FieldOptional "objectTable"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "objectTable" VkCmdReserveSpaceForCommandsInfoNVX
             =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}
        type FieldIsArray "objectTable" VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

instance {-# OVERLAPPING #-}
         CanReadField "objectTable" VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

instance {-# OVERLAPPING #-}
         CanWriteField "objectTable" VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

instance {-# OVERLAPPING #-}
         HasField "indirectCommandsLayout"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        type FieldType "indirectCommandsLayout"
               VkCmdReserveSpaceForCommandsInfoNVX
             = VkIndirectCommandsLayoutNVX
        type FieldOptional "indirectCommandsLayout"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "indirectCommandsLayout"
               VkCmdReserveSpaceForCommandsInfoNVX
             =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}
        type FieldIsArray "indirectCommandsLayout"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

instance {-# OVERLAPPING #-}
         CanReadField "indirectCommandsLayout"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "indirectCommandsLayout"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

instance {-# OVERLAPPING #-}
         HasField "maxSequencesCount" VkCmdReserveSpaceForCommandsInfoNVX
         where
        type FieldType "maxSequencesCount"
               VkCmdReserveSpaceForCommandsInfoNVX
             = Word32
        type FieldOptional "maxSequencesCount"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSequencesCount"
               VkCmdReserveSpaceForCommandsInfoNVX
             =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}
        type FieldIsArray "maxSequencesCount"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxSequencesCount"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSequencesCount"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

instance Show VkCmdReserveSpaceForCommandsInfoNVX where
        showsPrec d x
          = showString "VkCmdReserveSpaceForCommandsInfoNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectTable = " .
                            showsPrec d (getField @"objectTable" x) .
                              showString ", " .
                                showString "indirectCommandsLayout = " .
                                  showsPrec d (getField @"indirectCommandsLayout" x) .
                                    showString ", " .
                                      showString "maxSequencesCount = " .
                                        showsPrec d (getField @"maxSequencesCount" x) . showChar '}'
