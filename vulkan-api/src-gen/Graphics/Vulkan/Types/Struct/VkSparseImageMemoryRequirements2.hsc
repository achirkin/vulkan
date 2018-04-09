#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2
       (VkSparseImageMemoryRequirements2(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Base                                                     (Addr##,
                                                                               ByteArray##,
                                                                               byteArrayContents##,
                                                                               plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements (VkSparseImageMemoryRequirements)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageMemoryRequirements2 {
--   >     VkStructureType sType;
--   >     void*                                       pNext;
--   >     VkSparseImageMemoryRequirements                                      memoryRequirements;
--   > } VkSparseImageMemoryRequirements2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkSparseImageMemoryRequirements2VkSparseImageMemoryRequirements2 registry at www.khronos.org>
data VkSparseImageMemoryRequirements2 = VkSparseImageMemoryRequirements2## Addr##
                                                                          ByteArray##

instance Eq VkSparseImageMemoryRequirements2 where
        (VkSparseImageMemoryRequirements2## a _) ==
          x@(VkSparseImageMemoryRequirements2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryRequirements2 where
        (VkSparseImageMemoryRequirements2## a _) `compare`
          x@(VkSparseImageMemoryRequirements2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryRequirements2 where
        sizeOf ~_ = #{size VkSparseImageMemoryRequirements2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageMemoryRequirements2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryRequirements2 where
        unsafeAddr (VkSparseImageMemoryRequirements2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryRequirements2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryRequirements2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryRequirements2 where
        type StructFields VkSparseImageMemoryRequirements2 =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageMemoryRequirements2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryRequirements2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryRequirements2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageMemoryRequirements2 where
        type FieldType "sType" VkSparseImageMemoryRequirements2 =
             VkStructureType
        type FieldOptional "sType" VkSparseImageMemoryRequirements2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSparseImageMemoryRequirements2 =
             #{offset VkSparseImageMemoryRequirements2, sType}
        type FieldIsArray "sType" VkSparseImageMemoryRequirements2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSparseImageMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSparseImageMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageMemoryRequirements2 where
        type FieldType "pNext" VkSparseImageMemoryRequirements2 = Ptr Void
        type FieldOptional "pNext" VkSparseImageMemoryRequirements2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSparseImageMemoryRequirements2 =
             #{offset VkSparseImageMemoryRequirements2, pNext}
        type FieldIsArray "pNext" VkSparseImageMemoryRequirements2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSparseImageMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSparseImageMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkSparseImageMemoryRequirements2
         where
        type FieldType "memoryRequirements"
               VkSparseImageMemoryRequirements2
             = VkSparseImageMemoryRequirements
        type FieldOptional "memoryRequirements"
               VkSparseImageMemoryRequirements2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements"
               VkSparseImageMemoryRequirements2
             =
             #{offset VkSparseImageMemoryRequirements2, memoryRequirements}
        type FieldIsArray "memoryRequirements"
               VkSparseImageMemoryRequirements2
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanReadField "memoryRequirements" VkSparseImageMemoryRequirements2
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2, memoryRequirements})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryRequirements" VkSparseImageMemoryRequirements2
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2, memoryRequirements}

instance Show VkSparseImageMemoryRequirements2 where
        showsPrec d x
          = showString "VkSparseImageMemoryRequirements2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryRequirements = " .
                            showsPrec d (getField @"memoryRequirements" x) . showChar '}'
