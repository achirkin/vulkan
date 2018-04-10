#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
       (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Base                                                  (Addr##,
                                                                            ByteArray##,
                                                                            byteArrayContents##,
                                                                            plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupport (VkDescriptorSetLayoutSupport)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetVariableDescriptorCountLayoutSupportEXT {
--   >     VkStructureType sType;
--   >     void*            pNext;
--   >     uint32_t         maxVariableDescriptorCount;
--   > } VkDescriptorSetVariableDescriptorCountLayoutSupportEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDescriptorSetVariableDescriptorCountLayoutSupportEXT VkDescriptorSetVariableDescriptorCountLayoutSupportEXT registry at www.khronos.org>
data VkDescriptorSetVariableDescriptorCountLayoutSupportEXT = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## Addr##
                                                                                                                      ByteArray##

instance Eq VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## a _) ==
          x@(VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## a _)
          `compare`
          x@(VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        sizeOf ~_
          = #{size VkDescriptorSetVariableDescriptorCountLayoutSupportEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetVariableDescriptorCountLayoutSupportEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        unsafeAddr
          (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        type StructFields
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = '["sType", "pNext", "maxVariableDescriptorCount"] -- ' closing tick for hsc2hs
        type CUnionType
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'True -- ' closing tick for hsc2hs
        type StructExtends
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = '[VkDescriptorSetLayoutSupport] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        type FieldType "sType"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = VkStructureType
        type FieldOptional "sType"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}
        type FieldIsArray "sType"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        type FieldType "pNext"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}
        type FieldIsArray "pNext"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxVariableDescriptorCount"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        type FieldType "maxVariableDescriptorCount"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = Word32
        type FieldOptional "maxVariableDescriptorCount"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVariableDescriptorCount"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}
        type FieldIsArray "maxVariableDescriptorCount"
               VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxVariableDescriptorCount"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVariableDescriptorCount"
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountLayoutSupportEXT, maxVariableDescriptorCount}

instance Show
           VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
         where
        showsPrec d x
          = showString
              "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxVariableDescriptorCount = " .
                            showsPrec d (getField @"maxVariableDescriptorCount" x) .
                              showChar '}'
