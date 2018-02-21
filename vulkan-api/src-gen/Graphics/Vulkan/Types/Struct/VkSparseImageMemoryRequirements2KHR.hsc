#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2KHR
       (VkSparseImageMemoryRequirements2KHR(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements (VkSparseImageMemoryRequirements)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageMemoryRequirements2KHR {
--   >     VkStructureType sType;
--   >     void*                                       pNext;
--   >     VkSparseImageMemoryRequirements                                      memoryRequirements;
--   > } VkSparseImageMemoryRequirements2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseImageMemoryRequirements2KHR.html VkSparseImageMemoryRequirements2KHR registry at www.khronos.org>
data VkSparseImageMemoryRequirements2KHR = VkSparseImageMemoryRequirements2KHR## Addr##
                                                                                ByteArray##

instance Eq VkSparseImageMemoryRequirements2KHR where
        (VkSparseImageMemoryRequirements2KHR## a _) ==
          x@(VkSparseImageMemoryRequirements2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryRequirements2KHR where
        (VkSparseImageMemoryRequirements2KHR## a _) `compare`
          x@(VkSparseImageMemoryRequirements2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryRequirements2KHR where
        sizeOf ~_ = #{size VkSparseImageMemoryRequirements2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageMemoryRequirements2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryRequirements2KHR
         where
        unsafeAddr (VkSparseImageMemoryRequirements2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryRequirements2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryRequirements2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryRequirements2KHR where
        type StructFields VkSparseImageMemoryRequirements2KHR =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryRequirements2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryRequirements2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkSparseImageMemoryRequirements2KHR where
        type VkSTypeMType VkSparseImageMemoryRequirements2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageMemoryRequirements2KHR where
        type FieldType "sType" VkSparseImageMemoryRequirements2KHR =
             VkStructureType
        type FieldOptional "sType" VkSparseImageMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSparseImageMemoryRequirements2KHR =
             #{offset VkSparseImageMemoryRequirements2KHR, sType}
        type FieldIsArray "sType" VkSparseImageMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2KHR, sType}

instance CanReadField "sType" VkSparseImageMemoryRequirements2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSparseImageMemoryRequirements2KHR where
        type VkPNextMType VkSparseImageMemoryRequirements2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageMemoryRequirements2KHR where
        type FieldType "pNext" VkSparseImageMemoryRequirements2KHR =
             Ptr Void
        type FieldOptional "pNext" VkSparseImageMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSparseImageMemoryRequirements2KHR =
             #{offset VkSparseImageMemoryRequirements2KHR, pNext}
        type FieldIsArray "pNext" VkSparseImageMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2KHR, pNext}

instance CanReadField "pNext" VkSparseImageMemoryRequirements2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemoryRequirements VkSparseImageMemoryRequirements2KHR where
        type VkMemoryRequirementsMType VkSparseImageMemoryRequirements2KHR
             = VkSparseImageMemoryRequirements

        {-# NOINLINE vkMemoryRequirements #-}
        vkMemoryRequirements x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements})

        {-# INLINE vkMemoryRequirementsByteOffset #-}
        vkMemoryRequirementsByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE readVkMemoryRequirements #-}
        readVkMemoryRequirements p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE writeVkMemoryRequirements #-}
        writeVkMemoryRequirements p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkSparseImageMemoryRequirements2KHR
         where
        type FieldType "memoryRequirements"
               VkSparseImageMemoryRequirements2KHR
             = VkSparseImageMemoryRequirements
        type FieldOptional "memoryRequirements"
               VkSparseImageMemoryRequirements2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements"
               VkSparseImageMemoryRequirements2KHR
             =
             #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}
        type FieldIsArray "memoryRequirements"
               VkSparseImageMemoryRequirements2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

instance CanReadField "memoryRequirements"
           VkSparseImageMemoryRequirements2KHR
         where
        {-# INLINE getField #-}
        getField = vkMemoryRequirements

        {-# INLINE readField #-}
        readField = readVkMemoryRequirements

instance Show VkSparseImageMemoryRequirements2KHR where
        showsPrec d x
          = showString "VkSparseImageMemoryRequirements2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryRequirements = " .
                            showsPrec d (vkMemoryRequirements x) . showChar '}'
