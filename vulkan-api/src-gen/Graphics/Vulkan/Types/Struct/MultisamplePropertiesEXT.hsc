#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.MultisamplePropertiesEXT
       (VkMultisamplePropertiesEXT(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent2D)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkMultisamplePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExtent2D                       maxSampleLocationGridSize;
--   > } VkMultisamplePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMultisamplePropertiesEXT VkMultisamplePropertiesEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMultisamplePropertiesEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMultisamplePropertiesEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMultisamplePropertiesEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMultisamplePropertiesEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxSampleLocationGridSize" VkMultisamplePropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSampleLocationGridSize"
           VkMultisamplePropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}

instance Show VkMultisamplePropertiesEXT where
        showsPrec d x
          = showString "VkMultisamplePropertiesEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxSampleLocationGridSize = " .
                            showsPrec d (getField @"maxSampleLocationGridSize" x) .
                              showChar '}'
