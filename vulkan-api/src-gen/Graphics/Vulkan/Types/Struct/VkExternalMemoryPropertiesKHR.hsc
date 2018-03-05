#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR
       (VkExternalMemoryPropertiesKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsKHR    (VkExternalMemoryFeatureFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagsKHR)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryPropertiesKHR {
--   >     VkExternalMemoryFeatureFlagsKHR  externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlagsKHR compatibleHandleTypes;
--   > } VkExternalMemoryPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalMemoryPropertiesKHR.html VkExternalMemoryPropertiesKHR registry at www.khronos.org>
data VkExternalMemoryPropertiesKHR = VkExternalMemoryPropertiesKHR## Addr##
                                                                    ByteArray##

instance Eq VkExternalMemoryPropertiesKHR where
        (VkExternalMemoryPropertiesKHR## a _) ==
          x@(VkExternalMemoryPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryPropertiesKHR where
        (VkExternalMemoryPropertiesKHR## a _) `compare`
          x@(VkExternalMemoryPropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryPropertiesKHR where
        sizeOf ~_ = #{size VkExternalMemoryPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryPropertiesKHR where
        unsafeAddr (VkExternalMemoryPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryPropertiesKHR where
        type StructFields VkExternalMemoryPropertiesKHR =
             '["externalMemoryFeatures", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes"]
        type CUnionType VkExternalMemoryPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryPropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "externalMemoryFeatures" VkExternalMemoryPropertiesKHR
         where
        type FieldType "externalMemoryFeatures"
               VkExternalMemoryPropertiesKHR
             = VkExternalMemoryFeatureFlagsKHR
        type FieldOptional "externalMemoryFeatures"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryFeatures"
               VkExternalMemoryPropertiesKHR
             =
             #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}
        type FieldIsArray "externalMemoryFeatures"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryFeatures" VkExternalMemoryPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryFeatures"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryPropertiesKHR, externalMemoryFeatures}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalMemoryPropertiesKHR
             = VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalMemoryPropertiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalMemoryPropertiesKHR
             =
             #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "exportFromImportedHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "exportFromImportedHandleTypes"
           VkExternalMemoryPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryPropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalMemoryPropertiesKHR
         where
        type FieldType "compatibleHandleTypes"
               VkExternalMemoryPropertiesKHR
             = VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "compatibleHandleTypes"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes"
               VkExternalMemoryPropertiesKHR
             =
             #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}
        type FieldIsArray "compatibleHandleTypes"
               VkExternalMemoryPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "compatibleHandleTypes" VkExternalMemoryPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "compatibleHandleTypes" VkExternalMemoryPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryPropertiesKHR, compatibleHandleTypes}

instance Show VkExternalMemoryPropertiesKHR where
        showsPrec d x
          = showString "VkExternalMemoryPropertiesKHR {" .
              showString "externalMemoryFeatures = " .
                showsPrec d (getField @"externalMemoryFeatures" x) .
                  showString ", " .
                    showString "exportFromImportedHandleTypes = " .
                      showsPrec d (getField @"exportFromImportedHandleTypes" x) .
                        showString ", " .
                          showString "compatibleHandleTypes = " .
                            showsPrec d (getField @"compatibleHandleTypes" x) . showChar '}'
