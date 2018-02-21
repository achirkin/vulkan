#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryRequirements2KHR
       (VkMemoryRequirements2KHR(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements (VkMemoryRequirements)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryRequirements2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkMemoryRequirements                                                 memoryRequirements;
--   > } VkMemoryRequirements2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryRequirements2KHR.html VkMemoryRequirements2KHR registry at www.khronos.org>
data VkMemoryRequirements2KHR = VkMemoryRequirements2KHR## Addr##
                                                          ByteArray##

instance Eq VkMemoryRequirements2KHR where
        (VkMemoryRequirements2KHR## a _) ==
          x@(VkMemoryRequirements2KHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryRequirements2KHR where
        (VkMemoryRequirements2KHR## a _) `compare`
          x@(VkMemoryRequirements2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryRequirements2KHR where
        sizeOf ~_ = #{size VkMemoryRequirements2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryRequirements2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryRequirements2KHR where
        unsafeAddr (VkMemoryRequirements2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryRequirements2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryRequirements2KHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryRequirements2KHR where
        type StructFields VkMemoryRequirements2KHR =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryRequirements2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryRequirements2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMemoryRequirements2KHR
         where
        type VkSTypeMType VkMemoryRequirements2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryRequirements2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryRequirements2KHR where
        type FieldType "sType" VkMemoryRequirements2KHR = VkStructureType
        type FieldOptional "sType" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, sType}
        type FieldIsArray "sType" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2KHR, sType}

instance CanReadField "sType" VkMemoryRequirements2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMemoryRequirements2KHR
         where
        type VkPNextMType VkMemoryRequirements2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryRequirements2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryRequirements2KHR where
        type FieldType "pNext" VkMemoryRequirements2KHR = Ptr Void
        type FieldOptional "pNext" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, pNext}
        type FieldIsArray "pNext" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2KHR, pNext}

instance CanReadField "pNext" VkMemoryRequirements2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemoryRequirements VkMemoryRequirements2KHR where
        type VkMemoryRequirementsMType VkMemoryRequirements2KHR =
             VkMemoryRequirements

        {-# NOINLINE vkMemoryRequirements #-}
        vkMemoryRequirements x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, memoryRequirements})

        {-# INLINE vkMemoryRequirementsByteOffset #-}
        vkMemoryRequirementsByteOffset ~_
          = #{offset VkMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE readVkMemoryRequirements #-}
        readVkMemoryRequirements p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE writeVkMemoryRequirements #-}
        writeVkMemoryRequirements p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, memoryRequirements}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkMemoryRequirements2KHR where
        type FieldType "memoryRequirements" VkMemoryRequirements2KHR =
             VkMemoryRequirements
        type FieldOptional "memoryRequirements" VkMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, memoryRequirements}
        type FieldIsArray "memoryRequirements" VkMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryRequirements2KHR, memoryRequirements}

instance CanReadField "memoryRequirements" VkMemoryRequirements2KHR
         where
        {-# INLINE getField #-}
        getField = vkMemoryRequirements

        {-# INLINE readField #-}
        readField = readVkMemoryRequirements

instance Show VkMemoryRequirements2KHR where
        showsPrec d x
          = showString "VkMemoryRequirements2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryRequirements = " .
                            showsPrec d (vkMemoryRequirements x) . showChar '}'
