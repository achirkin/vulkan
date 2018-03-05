#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportMemoryHostPointerInfoEXT
       (VkImportMemoryHostPointerInfoEXT(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo             (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkImportMemoryHostPointerInfoEXT {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   >     void* pHostPointer;
--   > } VkImportMemoryHostPointerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImportMemoryHostPointerInfoEXT.html VkImportMemoryHostPointerInfoEXT registry at www.khronos.org>
data VkImportMemoryHostPointerInfoEXT = VkImportMemoryHostPointerInfoEXT## Addr##
                                                                          ByteArray##

instance Eq VkImportMemoryHostPointerInfoEXT where
        (VkImportMemoryHostPointerInfoEXT## a _) ==
          x@(VkImportMemoryHostPointerInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryHostPointerInfoEXT where
        (VkImportMemoryHostPointerInfoEXT## a _) `compare`
          x@(VkImportMemoryHostPointerInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryHostPointerInfoEXT where
        sizeOf ~_ = #{size VkImportMemoryHostPointerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryHostPointerInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryHostPointerInfoEXT where
        unsafeAddr (VkImportMemoryHostPointerInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryHostPointerInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryHostPointerInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryHostPointerInfoEXT where
        type StructFields VkImportMemoryHostPointerInfoEXT =
             '["sType", "pNext", "handleType", "pHostPointer"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryHostPointerInfoEXT =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryHostPointerInfoEXT where
        type FieldType "sType" VkImportMemoryHostPointerInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, sType}
        type FieldIsArray "sType" VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportMemoryHostPointerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportMemoryHostPointerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryHostPointerInfoEXT where
        type FieldType "pNext" VkImportMemoryHostPointerInfoEXT = Ptr Void
        type FieldOptional "pNext" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, pNext}
        type FieldIsArray "pNext" VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportMemoryHostPointerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportMemoryHostPointerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryHostPointerInfoEXT where
        type FieldType "handleType" VkImportMemoryHostPointerInfoEXT =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, handleType}
        type FieldIsArray "handleType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportMemoryHostPointerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportMemoryHostPointerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance {-# OVERLAPPING #-}
         HasField "pHostPointer" VkImportMemoryHostPointerInfoEXT where
        type FieldType "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             Ptr Void
        type FieldOptional "pHostPointer" VkImportMemoryHostPointerInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}
        type FieldIsArray "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance {-# OVERLAPPING #-}
         CanReadField "pHostPointer" VkImportMemoryHostPointerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance {-# OVERLAPPING #-}
         CanWriteField "pHostPointer" VkImportMemoryHostPointerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance Show VkImportMemoryHostPointerInfoEXT where
        showsPrec d x
          = showString "VkImportMemoryHostPointerInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) .
                              showString ", " .
                                showString "pHostPointer = " .
                                  showsPrec d (getField @"pHostPointer" x) . showChar '}'
