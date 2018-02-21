#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
       (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                           (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR (VkPhysicalDeviceFeatures2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         advancedBlendCoherentOperations;
--   > } VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT.html VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT registry at www.khronos.org>
data VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## Addr##
                                                                                                            ByteArray##

instance Eq VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
        (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## a _) ==
          x@(VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## a _) `compare`
          x@(VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        unsafeAddr (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type StructFields VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = '["sType", "pNext", "advancedBlendCoherentOperations"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = '[VkPhysicalDeviceFeatures2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
        type VkSTypeMType VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

instance CanReadField "sType"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
        type VkPNextMType VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkAdvancedBlendCoherentOperations
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type VkAdvancedBlendCoherentOperationsMType
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = VkBool32

        {-# NOINLINE vkAdvancedBlendCoherentOperations #-}
        vkAdvancedBlendCoherentOperations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations})

        {-# INLINE vkAdvancedBlendCoherentOperationsByteOffset #-}
        vkAdvancedBlendCoherentOperationsByteOffset ~_
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

        {-# INLINE readVkAdvancedBlendCoherentOperations #-}
        readVkAdvancedBlendCoherentOperations p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

        {-# INLINE writeVkAdvancedBlendCoherentOperations #-}
        writeVkAdvancedBlendCoherentOperations p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

instance {-# OVERLAPPING #-}
         HasField "advancedBlendCoherentOperations"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        type FieldType "advancedBlendCoherentOperations"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = VkBool32
        type FieldOptional "advancedBlendCoherentOperations"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendCoherentOperations"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}
        type FieldIsArray "advancedBlendCoherentOperations"
               VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

instance CanReadField "advancedBlendCoherentOperations"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE getField #-}
        getField = vkAdvancedBlendCoherentOperations

        {-# INLINE readField #-}
        readField = readVkAdvancedBlendCoherentOperations

instance CanWriteField "advancedBlendCoherentOperations"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAdvancedBlendCoherentOperations

instance Show VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        showsPrec d x
          = showString "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAdvancedBlendCoherentOperations = " .
                            showsPrec d (vkAdvancedBlendCoherentOperations x) . showChar '}'
