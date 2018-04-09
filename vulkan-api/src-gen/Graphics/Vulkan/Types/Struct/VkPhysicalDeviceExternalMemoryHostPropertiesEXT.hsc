#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalMemoryHostPropertiesEXT
       (VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalMemoryHostPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDeviceSize minImportedHostPointerAlignment;
--   > } VkPhysicalDeviceExternalMemoryHostPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceExternalMemoryHostPropertiesEXTVkPhysicalDeviceExternalMemoryHostPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceExternalMemoryHostPropertiesEXT = VkPhysicalDeviceExternalMemoryHostPropertiesEXT## Addr##
                                                                                                        ByteArray##

instance Eq VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceExternalMemoryHostPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## a _) `compare`
          x@(VkPhysicalDeviceExternalMemoryHostPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalMemoryHostPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalMemoryHostPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        unsafeAddr (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalMemoryHostPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type StructFields VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             '["sType", "pNext", "minImportedHostPointerAlignment"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             =
             #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             =
             #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = VkDeviceSize
        type FieldOptional "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             =
             #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}
        type FieldIsArray "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

instance {-# OVERLAPPING #-}
         CanReadField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

instance Show VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalMemoryHostPropertiesEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "minImportedHostPointerAlignment = " .
                            showsPrec d (getField @"minImportedHostPointerAlignment" x) .
                              showChar '}'
