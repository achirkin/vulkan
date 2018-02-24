#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMultisamplePropertiesEXT
       (VkMultisamplePropertiesEXT(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExtent2D    (VkExtent2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMultisamplePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExtent2D                       maxSampleLocationGridSize;
--   > } VkMultisamplePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMultisamplePropertiesEXT.html VkMultisamplePropertiesEXT registry at www.khronos.org>
data VkMultisamplePropertiesEXT = VkMultisamplePropertiesEXT## Addr##
                                                              ByteArray##

instance Eq VkMultisamplePropertiesEXT where
        (VkMultisamplePropertiesEXT## a _) ==
          x@(VkMultisamplePropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMultisamplePropertiesEXT where
        (VkMultisamplePropertiesEXT## a _) `compare`
          x@(VkMultisamplePropertiesEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMultisamplePropertiesEXT where
        sizeOf ~_ = #{size VkMultisamplePropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMultisamplePropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMultisamplePropertiesEXT where
        unsafeAddr (VkMultisamplePropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMultisamplePropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMultisamplePropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMultisamplePropertiesEXT where
        type StructFields VkMultisamplePropertiesEXT =
             '["sType", "pNext", "maxSampleLocationGridSize"] -- ' closing tick for hsc2hs
        type CUnionType VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMultisamplePropertiesEXT = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMultisamplePropertiesEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMultisamplePropertiesEXT
         where
        type VkSTypeMType VkMultisamplePropertiesEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMultisamplePropertiesEXT where
        type FieldType "sType" VkMultisamplePropertiesEXT = VkStructureType
        type FieldOptional "sType" VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMultisamplePropertiesEXT =
             #{offset VkMultisamplePropertiesEXT, sType}
        type FieldIsArray "sType" VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMultisamplePropertiesEXT, sType}

instance CanReadField "sType" VkMultisamplePropertiesEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMultisamplePropertiesEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMultisamplePropertiesEXT
         where
        type VkPNextMType VkMultisamplePropertiesEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMultisamplePropertiesEXT where
        type FieldType "pNext" VkMultisamplePropertiesEXT = Ptr Void
        type FieldOptional "pNext" VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMultisamplePropertiesEXT =
             #{offset VkMultisamplePropertiesEXT, pNext}
        type FieldIsArray "pNext" VkMultisamplePropertiesEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMultisamplePropertiesEXT, pNext}

instance CanReadField "pNext" VkMultisamplePropertiesEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMultisamplePropertiesEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMaxSampleLocationGridSize VkMultisamplePropertiesEXT where
        type VkMaxSampleLocationGridSizeMType VkMultisamplePropertiesEXT =
             VkExtent2D

        {-# NOINLINE vkMaxSampleLocationGridSize #-}
        vkMaxSampleLocationGridSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize})

        {-# INLINE vkMaxSampleLocationGridSizeByteOffset #-}
        vkMaxSampleLocationGridSizeByteOffset ~_
          = #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE readVkMaxSampleLocationGridSize #-}
        readVkMaxSampleLocationGridSize p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

        {-# INLINE writeVkMaxSampleLocationGridSize #-}
        writeVkMaxSampleLocationGridSize p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

instance {-# OVERLAPPING #-}
         HasField "maxSampleLocationGridSize" VkMultisamplePropertiesEXT
         where
        type FieldType "maxSampleLocationGridSize"
               VkMultisamplePropertiesEXT
             = VkExtent2D
        type FieldOptional "maxSampleLocationGridSize"
               VkMultisamplePropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSampleLocationGridSize"
               VkMultisamplePropertiesEXT
             =
             #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}
        type FieldIsArray "maxSampleLocationGridSize"
               VkMultisamplePropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

instance CanReadField "maxSampleLocationGridSize"
           VkMultisamplePropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMaxSampleLocationGridSize

        {-# INLINE readField #-}
        readField = readVkMaxSampleLocationGridSize

instance CanWriteField "maxSampleLocationGridSize"
           VkMultisamplePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSampleLocationGridSize

instance Show VkMultisamplePropertiesEXT where
        showsPrec d x
          = showString "VkMultisamplePropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxSampleLocationGridSize = " .
                            showsPrec d (vkMaxSampleLocationGridSize x) . showChar '}'
