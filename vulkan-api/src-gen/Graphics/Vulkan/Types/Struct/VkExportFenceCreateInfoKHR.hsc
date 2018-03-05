#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportFenceCreateInfoKHR
       (VkExportFenceCreateInfoKHR(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR (VkExternalFenceHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkFenceCreateInfo               (VkFenceCreateInfo)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkExportFenceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlagsKHR handleTypes;
--   > } VkExportFenceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExportFenceCreateInfoKHR.html VkExportFenceCreateInfoKHR registry at www.khronos.org>
data VkExportFenceCreateInfoKHR = VkExportFenceCreateInfoKHR## Addr##
                                                              ByteArray##

instance Eq VkExportFenceCreateInfoKHR where
        (VkExportFenceCreateInfoKHR## a _) ==
          x@(VkExportFenceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportFenceCreateInfoKHR where
        (VkExportFenceCreateInfoKHR## a _) `compare`
          x@(VkExportFenceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportFenceCreateInfoKHR where
        sizeOf ~_ = #{size VkExportFenceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExportFenceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportFenceCreateInfoKHR where
        unsafeAddr (VkExportFenceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportFenceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportFenceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportFenceCreateInfoKHR where
        type StructFields VkExportFenceCreateInfoKHR =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportFenceCreateInfoKHR =
             '[VkFenceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportFenceCreateInfoKHR where
        type FieldType "sType" VkExportFenceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportFenceCreateInfoKHR =
             #{offset VkExportFenceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportFenceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportFenceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportFenceCreateInfoKHR where
        type FieldType "pNext" VkExportFenceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportFenceCreateInfoKHR =
             #{offset VkExportFenceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportFenceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportFenceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportFenceCreateInfoKHR where
        type FieldType "handleTypes" VkExportFenceCreateInfoKHR =
             VkExternalFenceHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExportFenceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportFenceCreateInfoKHR =
             #{offset VkExportFenceCreateInfoKHR, handleTypes}
        type FieldIsArray "handleTypes" VkExportFenceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportFenceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfoKHR, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportFenceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfoKHR, handleTypes}

instance Show VkExportFenceCreateInfoKHR where
        showsPrec d x
          = showString "VkExportFenceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
