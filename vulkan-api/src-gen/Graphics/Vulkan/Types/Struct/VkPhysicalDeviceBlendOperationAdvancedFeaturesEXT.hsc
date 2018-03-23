#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
       (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                        (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2 (VkPhysicalDeviceFeatures2)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         advancedBlendCoherentOperations;
--   > } VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT.html VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT registry at www.khronos.org>
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
             = '[VkPhysicalDeviceFeatures2] -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "advancedBlendCoherentOperations"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

instance {-# OVERLAPPING #-}
         CanWriteField "advancedBlendCoherentOperations"
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}

instance Show VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
        showsPrec d x
          = showString "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "advancedBlendCoherentOperations = " .
                            showsPrec d (getField @"advancedBlendCoherentOperations" x) .
                              showChar '}'
