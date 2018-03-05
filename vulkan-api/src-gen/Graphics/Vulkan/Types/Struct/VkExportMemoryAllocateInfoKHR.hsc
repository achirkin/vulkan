#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfoKHR
       (VkExportMemoryAllocateInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo             (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkExportMemoryAllocateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExportMemoryAllocateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExportMemoryAllocateInfoKHR.html VkExportMemoryAllocateInfoKHR registry at www.khronos.org>
data VkExportMemoryAllocateInfoKHR = VkExportMemoryAllocateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkExportMemoryAllocateInfoKHR where
        (VkExportMemoryAllocateInfoKHR## a _) ==
          x@(VkExportMemoryAllocateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfoKHR where
        (VkExportMemoryAllocateInfoKHR## a _) `compare`
          x@(VkExportMemoryAllocateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfoKHR where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryAllocateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryAllocateInfoKHR where
        unsafeAddr (VkExportMemoryAllocateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryAllocateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryAllocateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryAllocateInfoKHR where
        type StructFields VkExportMemoryAllocateInfoKHR =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryAllocateInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryAllocateInfoKHR where
        type FieldType "sType" VkExportMemoryAllocateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryAllocateInfoKHR =
             #{offset VkExportMemoryAllocateInfoKHR, sType}
        type FieldIsArray "sType" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryAllocateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryAllocateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryAllocateInfoKHR where
        type FieldType "pNext" VkExportMemoryAllocateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryAllocateInfoKHR =
             #{offset VkExportMemoryAllocateInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryAllocateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryAllocateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportMemoryAllocateInfoKHR where
        type FieldType "handleTypes" VkExportMemoryAllocateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExportMemoryAllocateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportMemoryAllocateInfoKHR =
             #{offset VkExportMemoryAllocateInfoKHR, handleTypes}
        type FieldIsArray "handleTypes" VkExportMemoryAllocateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportMemoryAllocateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportMemoryAllocateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

instance Show VkExportMemoryAllocateInfoKHR where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
