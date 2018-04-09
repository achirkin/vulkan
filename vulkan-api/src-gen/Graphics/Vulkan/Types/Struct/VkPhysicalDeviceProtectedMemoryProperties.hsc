#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProtectedMemoryProperties
       (VkPhysicalDeviceProtectedMemoryProperties(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceProtectedMemoryProperties {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            protectedNoFault;
--   > } VkPhysicalDeviceProtectedMemoryProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceProtectedMemoryPropertiesVkPhysicalDeviceProtectedMemoryProperties registry at www.khronos.org>
data VkPhysicalDeviceProtectedMemoryProperties = VkPhysicalDeviceProtectedMemoryProperties## Addr##
                                                                                            ByteArray##

instance Eq VkPhysicalDeviceProtectedMemoryProperties where
        (VkPhysicalDeviceProtectedMemoryProperties## a _) ==
          x@(VkPhysicalDeviceProtectedMemoryProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceProtectedMemoryProperties where
        (VkPhysicalDeviceProtectedMemoryProperties## a _) `compare`
          x@(VkPhysicalDeviceProtectedMemoryProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceProtectedMemoryProperties where
        sizeOf ~_
          = #{size VkPhysicalDeviceProtectedMemoryProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceProtectedMemoryProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceProtectedMemoryProperties
         where
        unsafeAddr (VkPhysicalDeviceProtectedMemoryProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceProtectedMemoryProperties## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceProtectedMemoryProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceProtectedMemoryProperties
         where
        type StructFields VkPhysicalDeviceProtectedMemoryProperties =
             '["sType", "pNext", "protectedNoFault"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceProtectedMemoryProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceProtectedMemoryProperties =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceProtectedMemoryProperties =
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceProtectedMemoryProperties where
        type FieldType "sType" VkPhysicalDeviceProtectedMemoryProperties =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceProtectedMemoryProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceProtectedMemoryProperties
             =
             #{offset VkPhysicalDeviceProtectedMemoryProperties, sType}
        type FieldIsArray "sType" VkPhysicalDeviceProtectedMemoryProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProtectedMemoryProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceProtectedMemoryProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProtectedMemoryProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProtectedMemoryProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceProtectedMemoryProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProtectedMemoryProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceProtectedMemoryProperties where
        type FieldType "pNext" VkPhysicalDeviceProtectedMemoryProperties =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceProtectedMemoryProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceProtectedMemoryProperties
             =
             #{offset VkPhysicalDeviceProtectedMemoryProperties, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceProtectedMemoryProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProtectedMemoryProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceProtectedMemoryProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProtectedMemoryProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProtectedMemoryProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceProtectedMemoryProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProtectedMemoryProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "protectedNoFault"
           VkPhysicalDeviceProtectedMemoryProperties
         where
        type FieldType "protectedNoFault"
               VkPhysicalDeviceProtectedMemoryProperties
             = VkBool32
        type FieldOptional "protectedNoFault"
               VkPhysicalDeviceProtectedMemoryProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "protectedNoFault"
               VkPhysicalDeviceProtectedMemoryProperties
             =
             #{offset VkPhysicalDeviceProtectedMemoryProperties, protectedNoFault}
        type FieldIsArray "protectedNoFault"
               VkPhysicalDeviceProtectedMemoryProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProtectedMemoryProperties, protectedNoFault}

instance {-# OVERLAPPING #-}
         CanReadField "protectedNoFault"
           VkPhysicalDeviceProtectedMemoryProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProtectedMemoryProperties, protectedNoFault})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProtectedMemoryProperties, protectedNoFault}

instance {-# OVERLAPPING #-}
         CanWriteField "protectedNoFault"
           VkPhysicalDeviceProtectedMemoryProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProtectedMemoryProperties, protectedNoFault}

instance Show VkPhysicalDeviceProtectedMemoryProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceProtectedMemoryProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "protectedNoFault = " .
                            showsPrec d (getField @"protectedNoFault" x) . showChar '}'
