#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfoKHR
       (VkExternalMemoryImageCreateInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo                (VkImageCreateInfo)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryImageCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExternalMemoryImageCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalMemoryImageCreateInfoKHR.html VkExternalMemoryImageCreateInfoKHR registry at www.khronos.org>
data VkExternalMemoryImageCreateInfoKHR = VkExternalMemoryImageCreateInfoKHR## Addr##
                                                                              ByteArray##

instance Eq VkExternalMemoryImageCreateInfoKHR where
        (VkExternalMemoryImageCreateInfoKHR## a _) ==
          x@(VkExternalMemoryImageCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfoKHR where
        (VkExternalMemoryImageCreateInfoKHR## a _) `compare`
          x@(VkExternalMemoryImageCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfoKHR where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryImageCreateInfoKHR where
        unsafeAddr (VkExternalMemoryImageCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryImageCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryImageCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfoKHR where
        type StructFields VkExternalMemoryImageCreateInfoKHR =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryImageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryImageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryImageCreateInfoKHR =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "sType" VkExternalMemoryImageCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryImageCreateInfoKHR =
             #{offset VkExternalMemoryImageCreateInfoKHR, sType}
        type FieldIsArray "sType" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalMemoryImageCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalMemoryImageCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "pNext" VkExternalMemoryImageCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryImageCreateInfoKHR =
             #{offset VkExternalMemoryImageCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalMemoryImageCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalMemoryImageCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "handleTypes" VkExternalMemoryImageCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExternalMemoryImageCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryImageCreateInfoKHR =
             #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryImageCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExternalMemoryImageCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

instance Show VkExternalMemoryImageCreateInfoKHR where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
