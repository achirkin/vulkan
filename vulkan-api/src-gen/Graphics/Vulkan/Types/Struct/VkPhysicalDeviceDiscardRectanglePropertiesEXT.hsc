#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceDiscardRectanglePropertiesEXT
       (VkPhysicalDeviceDiscardRectanglePropertiesEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        type VkSTypeMType VkPhysicalDeviceDiscardRectanglePropertiesEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}

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

instance CanReadField "sType"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        type VkPNextMType VkPhysicalDeviceDiscardRectanglePropertiesEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}

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

instance CanReadField "pNext"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMaxDiscardRectangles
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        type VkMaxDiscardRectanglesMType
               VkPhysicalDeviceDiscardRectanglePropertiesEXT
             = Word32

        {-# NOINLINE vkMaxDiscardRectangles #-}
        vkMaxDiscardRectangles x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles})

        {-# INLINE vkMaxDiscardRectanglesByteOffset #-}
        vkMaxDiscardRectanglesByteOffset ~_
          = #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

        {-# INLINE readVkMaxDiscardRectangles #-}
        readVkMaxDiscardRectangles p
          = peekByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

        {-# INLINE writeVkMaxDiscardRectangles #-}
        writeVkMaxDiscardRectangles p
          = pokeByteOff p #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}

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

instance CanReadField "maxDiscardRectangles"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMaxDiscardRectangles

        {-# INLINE readField #-}
        readField = readVkMaxDiscardRectangles

instance CanWriteField "maxDiscardRectangles"
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDiscardRectangles

instance Show VkPhysicalDeviceDiscardRectanglePropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceDiscardRectanglePropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxDiscardRectangles = " .
                            showsPrec d (vkMaxDiscardRectangles x) . showChar '}'
