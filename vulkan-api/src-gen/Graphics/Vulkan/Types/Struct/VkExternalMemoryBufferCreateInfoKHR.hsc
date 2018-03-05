#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalMemoryBufferCreateInfoKHR
       (VkExternalMemoryBufferCreateInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBufferCreateInfo               (VkBufferCreateInfo)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryBufferCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExternalMemoryBufferCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalMemoryBufferCreateInfoKHR.html VkExternalMemoryBufferCreateInfoKHR registry at www.khronos.org>
data VkExternalMemoryBufferCreateInfoKHR = VkExternalMemoryBufferCreateInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkExternalMemoryBufferCreateInfoKHR where
        (VkExternalMemoryBufferCreateInfoKHR## a _) ==
          x@(VkExternalMemoryBufferCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryBufferCreateInfoKHR where
        (VkExternalMemoryBufferCreateInfoKHR## a _) `compare`
          x@(VkExternalMemoryBufferCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryBufferCreateInfoKHR where
        sizeOf ~_ = #{size VkExternalMemoryBufferCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryBufferCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryBufferCreateInfoKHR
         where
        unsafeAddr (VkExternalMemoryBufferCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryBufferCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryBufferCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryBufferCreateInfoKHR where
        type StructFields VkExternalMemoryBufferCreateInfoKHR =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryBufferCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryBufferCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryBufferCreateInfoKHR =
             '[VkBufferCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "sType" VkExternalMemoryBufferCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryBufferCreateInfoKHR =
             #{offset VkExternalMemoryBufferCreateInfoKHR, sType}
        type FieldIsArray "sType" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalMemoryBufferCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalMemoryBufferCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "pNext" VkExternalMemoryBufferCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryBufferCreateInfoKHR =
             #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalMemoryBufferCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalMemoryBufferCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "handleTypes" VkExternalMemoryBufferCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes"
               VkExternalMemoryBufferCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryBufferCreateInfoKHR
             =
             #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryBufferCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

instance Show VkExternalMemoryBufferCreateInfoKHR where
        showsPrec d x
          = showString "VkExternalMemoryBufferCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
