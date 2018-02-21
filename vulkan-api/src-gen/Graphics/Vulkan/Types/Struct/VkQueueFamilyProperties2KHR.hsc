#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2KHR
       (VkQueueFamilyProperties2KHR(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties (VkQueueFamilyProperties)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkQueueFamilyProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkQueueFamilyProperties          queueFamilyProperties;
--   > } VkQueueFamilyProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkQueueFamilyProperties2KHR.html VkQueueFamilyProperties2KHR registry at www.khronos.org>
data VkQueueFamilyProperties2KHR = VkQueueFamilyProperties2KHR## Addr##
                                                                ByteArray##

instance Eq VkQueueFamilyProperties2KHR where
        (VkQueueFamilyProperties2KHR## a _) ==
          x@(VkQueueFamilyProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkQueueFamilyProperties2KHR where
        (VkQueueFamilyProperties2KHR## a _) `compare`
          x@(VkQueueFamilyProperties2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkQueueFamilyProperties2KHR where
        sizeOf ~_ = #{size VkQueueFamilyProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkQueueFamilyProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkQueueFamilyProperties2KHR where
        unsafeAddr (VkQueueFamilyProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkQueueFamilyProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkQueueFamilyProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkQueueFamilyProperties2KHR where
        type StructFields VkQueueFamilyProperties2KHR =
             '["sType", "pNext", "queueFamilyProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkQueueFamilyProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkQueueFamilyProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkQueueFamilyProperties2KHR
         where
        type VkSTypeMType VkQueueFamilyProperties2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkQueueFamilyProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkQueueFamilyProperties2KHR where
        type FieldType "sType" VkQueueFamilyProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkQueueFamilyProperties2KHR =
             #{offset VkQueueFamilyProperties2KHR, sType}
        type FieldIsArray "sType" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties2KHR, sType}

instance CanReadField "sType" VkQueueFamilyProperties2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkQueueFamilyProperties2KHR
         where
        type VkPNextMType VkQueueFamilyProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkQueueFamilyProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkQueueFamilyProperties2KHR where
        type FieldType "pNext" VkQueueFamilyProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkQueueFamilyProperties2KHR =
             #{offset VkQueueFamilyProperties2KHR, pNext}
        type FieldIsArray "pNext" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties2KHR, pNext}

instance CanReadField "pNext" VkQueueFamilyProperties2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkQueueFamilyProperties VkQueueFamilyProperties2KHR where
        type VkQueueFamilyPropertiesMType VkQueueFamilyProperties2KHR =
             VkQueueFamilyProperties

        {-# NOINLINE vkQueueFamilyProperties #-}
        vkQueueFamilyProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties})

        {-# INLINE vkQueueFamilyPropertiesByteOffset #-}
        vkQueueFamilyPropertiesByteOffset ~_
          = #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

        {-# INLINE readVkQueueFamilyProperties #-}
        readVkQueueFamilyProperties p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

        {-# INLINE writeVkQueueFamilyProperties #-}
        writeVkQueueFamilyProperties p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyProperties" VkQueueFamilyProperties2KHR where
        type FieldType "queueFamilyProperties" VkQueueFamilyProperties2KHR
             = VkQueueFamilyProperties
        type FieldOptional "queueFamilyProperties"
               VkQueueFamilyProperties2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyProperties"
               VkQueueFamilyProperties2KHR
             =
             #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}
        type FieldIsArray "queueFamilyProperties"
               VkQueueFamilyProperties2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

instance CanReadField "queueFamilyProperties"
           VkQueueFamilyProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkQueueFamilyProperties

        {-# INLINE readField #-}
        readField = readVkQueueFamilyProperties

instance Show VkQueueFamilyProperties2KHR where
        showsPrec d x
          = showString "VkQueueFamilyProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkQueueFamilyProperties = " .
                            showsPrec d (vkQueueFamilyProperties x) . showChar '}'
