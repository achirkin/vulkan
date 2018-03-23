#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassSampleLocationsBeginInfoEXT
       (VkRenderPassSampleLocationsBeginInfoEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkAttachmentSampleLocationsEXT (VkAttachmentSampleLocationsEXT)
import           Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo          (VkRenderPassBeginInfo)
import           Graphics.Vulkan.Types.Struct.VkSubpassSampleLocationsEXT    (VkSubpassSampleLocationsEXT)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassSampleLocationsBeginInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         attachmentInitialSampleLocationsCount;
--   >     const VkAttachmentSampleLocationsEXT* pAttachmentInitialSampleLocations;
--   >     uint32_t         postSubpassSampleLocationsCount;
--   >     const VkSubpassSampleLocationsEXT* pPostSubpassSampleLocations;
--   > } VkRenderPassSampleLocationsBeginInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkRenderPassSampleLocationsBeginInfoEXT.html VkRenderPassSampleLocationsBeginInfoEXT registry at www.khronos.org>
data VkRenderPassSampleLocationsBeginInfoEXT = VkRenderPassSampleLocationsBeginInfoEXT## Addr##
                                                                                        ByteArray##

instance Eq VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a _) ==
          x@(VkRenderPassSampleLocationsBeginInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a _) `compare`
          x@(VkRenderPassSampleLocationsBeginInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassSampleLocationsBeginInfoEXT where
        sizeOf ~_
          = #{size VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassSampleLocationsBeginInfoEXT
         where
        unsafeAddr (VkRenderPassSampleLocationsBeginInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassSampleLocationsBeginInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassSampleLocationsBeginInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassSampleLocationsBeginInfoEXT
         where
        type StructFields VkRenderPassSampleLocationsBeginInfoEXT =
             '["sType", "pNext", "attachmentInitialSampleLocationsCount", -- ' closing tick for hsc2hs
               "pAttachmentInitialSampleLocations",
               "postSubpassSampleLocationsCount", "pPostSubpassSampleLocations"]
        type CUnionType VkRenderPassSampleLocationsBeginInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassSampleLocationsBeginInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassSampleLocationsBeginInfoEXT =
             '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassSampleLocationsBeginInfoEXT where
        type FieldType "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}
        type FieldIsArray "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassSampleLocationsBeginInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassSampleLocationsBeginInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassSampleLocationsBeginInfoEXT where
        type FieldType "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}
        type FieldIsArray "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassSampleLocationsBeginInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassSampleLocationsBeginInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32
        type FieldOptional "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}
        type FieldIsArray "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanReadField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkAttachmentSampleLocationsEXT
        type FieldOptional "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}
        type FieldIsArray "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         CanReadField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         HasField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32
        type FieldOptional "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}
        type FieldIsArray "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanReadField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanWriteField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkSubpassSampleLocationsEXT
        type FieldOptional "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}
        type FieldIsArray "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance {-# OVERLAPPING #-}
         CanReadField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance {-# OVERLAPPING #-}
         CanWriteField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance Show VkRenderPassSampleLocationsBeginInfoEXT where
        showsPrec d x
          = showString "VkRenderPassSampleLocationsBeginInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "attachmentInitialSampleLocationsCount = " .
                            showsPrec d (getField @"attachmentInitialSampleLocationsCount" x) .
                              showString ", " .
                                showString "pAttachmentInitialSampleLocations = " .
                                  showsPrec d (getField @"pAttachmentInitialSampleLocations" x) .
                                    showString ", " .
                                      showString "postSubpassSampleLocationsCount = " .
                                        showsPrec d (getField @"postSubpassSampleLocationsCount" x)
                                          .
                                          showString ", " .
                                            showString "pPostSubpassSampleLocations = " .
                                              showsPrec d
                                                (getField @"pPostSubpassSampleLocations" x)
                                                . showChar '}'
