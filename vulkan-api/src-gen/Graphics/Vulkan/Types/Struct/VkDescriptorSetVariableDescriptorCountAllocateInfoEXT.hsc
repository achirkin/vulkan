#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
       (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetAllocateInfo (VkDescriptorSetAllocateInfo)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetVariableDescriptorCountAllocateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               descriptorSetCount;
--   >     const uint32_t* pDescriptorCounts;
--   > } VkDescriptorSetVariableDescriptorCountAllocateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDescriptorSetVariableDescriptorCountAllocateInfoEXT VkDescriptorSetVariableDescriptorCountAllocateInfoEXT registry at www.khronos.org>
data VkDescriptorSetVariableDescriptorCountAllocateInfoEXT = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## Addr##
                                                                                                                    ByteArray##

instance Eq VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## a _) ==
          x@(VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## a _)
          `compare`
          x@(VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        sizeOf ~_
          = #{size VkDescriptorSetVariableDescriptorCountAllocateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetVariableDescriptorCountAllocateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        unsafeAddr
          (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type StructFields
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = '["sType", "pNext", "descriptorSetCount", "pDescriptorCounts"] -- ' closing tick for hsc2hs
        type CUnionType
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = '[VkDescriptorSetAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type FieldType "sType"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}
        type FieldIsArray "sType"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type FieldType "pNext"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetCount"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type FieldType "descriptorSetCount"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = Word32
        type FieldOptional "descriptorSetCount"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetCount"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}
        type FieldIsArray "descriptorSetCount"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSetCount"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSetCount"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, descriptorSetCount}

instance {-# OVERLAPPING #-}
         HasField "pDescriptorCounts"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        type FieldType "pDescriptorCounts"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = Ptr Word32
        type FieldOptional "pDescriptorCounts"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDescriptorCounts"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             =
             #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}
        type FieldIsArray "pDescriptorCounts"
               VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}

instance {-# OVERLAPPING #-}
         CanReadField "pDescriptorCounts"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "pDescriptorCounts"
           VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetVariableDescriptorCountAllocateInfoEXT, pDescriptorCounts}

instance Show VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
         where
        showsPrec d x
          = showString
              "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "descriptorSetCount = " .
                            showsPrec d (getField @"descriptorSetCount" x) .
                              showString ", " .
                                showString "pDescriptorCounts = " .
                                  showsPrec d (getField @"pDescriptorCounts" x) . showChar '}'
