#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceDiscardRectanglePropertiesEXT
       (VkPhysicalDeviceDiscardRectanglePropertiesEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceDiscardRectanglePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxDiscardRectangles;
--   > } VkPhysicalDeviceDiscardRectanglePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceDiscardRectanglePropertiesEXT.html VkPhysicalDeviceDiscardRectanglePropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceDiscardRectanglePropertiesEXT = VkPhysicalDeviceDiscardRectanglePropertiesEXT## Addr##
                                                                                                    ByteArray##

instance Eq VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        (VkPhysicalDeviceDiscardRectanglePropertiesEXT## a _) ==
          x@(VkPhysicalDeviceDiscardRectanglePropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        (VkPhysicalDeviceDiscardRectanglePropertiesEXT## a _) `compare`
          x@(VkPhysicalDeviceDiscardRectanglePropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceDiscardRectanglePropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceDiscardRectanglePropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        unsafeAddr (VkPhysicalDeviceDiscardRectanglePropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceDiscardRectanglePropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceDiscardRectanglePropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type StructFields VkPhysicalDeviceDiscardRectanglePropertiesEXT =
             '["sType", "pNext", "maxDiscardRectangles"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceDiscardRectanglePropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceDiscardRectanglePropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceDiscardRectanglePropertiesEXT =
             '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             =
             #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             =
             #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxDiscardRectangles"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type FieldType "maxDiscardRectangles"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = Word32
        type FieldOptional "maxDiscardRectangles"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDiscardRectangles"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             =
             #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}
        type FieldIsArray "maxDiscardRectangles"
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

instance {-# OVERLAPPING #-}
         CanReadField "maxDiscardRectangles"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDiscardRectangles"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

instance Show VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceDiscardRectanglePropertiesEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxDiscardRectangles = " .
                            showsPrec d (getField @"maxDiscardRectangles" x) . showChar '}'
