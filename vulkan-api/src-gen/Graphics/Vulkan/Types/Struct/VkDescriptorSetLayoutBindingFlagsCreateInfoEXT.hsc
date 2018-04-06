#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
       (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorBindingFlagsEXT       (VkDescriptorBindingFlagsEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo (VkDescriptorSetLayoutCreateInfo)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetLayoutBindingFlagsCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               bindingCount;
--   >     const VkDescriptorBindingFlagsEXT* pBindingFlags;
--   > } VkDescriptorSetLayoutBindingFlagsCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDescriptorSetLayoutBindingFlagsCreateInfoEXT.html VkDescriptorSetLayoutBindingFlagsCreateInfoEXT registry at www.khronos.org>
data VkDescriptorSetLayoutBindingFlagsCreateInfoEXT = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## Addr##
                                                                                                      ByteArray##

instance Eq VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
        (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## a _) ==
          x@(VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
        (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## a _) `compare`
          x@(VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkDescriptorSetLayoutBindingFlagsCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutBindingFlagsCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        unsafeAddr (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type StructFields VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
             '["sType", "pNext", "bindingCount", "pBindingFlags"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutBindingFlagsCreateInfoEXT =
             '[VkDescriptorSetLayoutCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type FieldType "sType"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             =
             #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type FieldType "pNext"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             =
             #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "bindingCount"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type FieldType "bindingCount"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = Word32
        type FieldOptional "bindingCount"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "bindingCount"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             =
             #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}
        type FieldIsArray "bindingCount"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindingCount"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindingCount"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, bindingCount}

instance {-# OVERLAPPING #-}
         HasField "pBindingFlags"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        type FieldType "pBindingFlags"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = Ptr VkDescriptorBindingFlagsEXT
        type FieldOptional "pBindingFlags"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBindingFlags"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             =
             #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}
        type FieldIsArray "pBindingFlags"
               VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}

instance {-# OVERLAPPING #-}
         CanReadField "pBindingFlags"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "pBindingFlags"
           VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBindingFlagsCreateInfoEXT, pBindingFlags}

instance Show VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
        showsPrec d x
          = showString "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "bindingCount = " .
                            showsPrec d (getField @"bindingCount" x) .
                              showString ", " .
                                showString "pBindingFlags = " .
                                  showsPrec d (getField @"pBindingFlags" x) . showChar '}'
