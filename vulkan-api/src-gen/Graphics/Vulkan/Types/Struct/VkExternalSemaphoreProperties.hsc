#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalSemaphoreProperties
       (VkExternalSemaphoreProperties(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreFeatureFlags    (VkExternalSemaphoreFeatureFlags)
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlags (VkExternalSemaphoreHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkExternalSemaphoreProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalSemaphoreHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalSemaphoreHandleTypeFlags compatibleHandleTypes;
--   >     VkExternalSemaphoreFeatureFlags externalSemaphoreFeatures;
--   > } VkExternalSemaphoreProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkExternalSemaphoreProperties.html VkExternalSemaphoreProperties registry at www.khronos.org>
data VkExternalSemaphoreProperties = VkExternalSemaphoreProperties## Addr##
                                                                    ByteArray##

instance Eq VkExternalSemaphoreProperties where
        (VkExternalSemaphoreProperties## a _) ==
          x@(VkExternalSemaphoreProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalSemaphoreProperties where
        (VkExternalSemaphoreProperties## a _) `compare`
          x@(VkExternalSemaphoreProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalSemaphoreProperties where
        sizeOf ~_ = #{size VkExternalSemaphoreProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalSemaphoreProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalSemaphoreProperties where
        unsafeAddr (VkExternalSemaphoreProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalSemaphoreProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalSemaphoreProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalSemaphoreProperties where
        type StructFields VkExternalSemaphoreProperties =
             '["sType", "pNext", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes", "externalSemaphoreFeatures"]
        type CUnionType VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalSemaphoreProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalSemaphoreProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalSemaphoreProperties where
        type FieldType "sType" VkExternalSemaphoreProperties =
             VkStructureType
        type FieldOptional "sType" VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalSemaphoreProperties =
             #{offset VkExternalSemaphoreProperties, sType}
        type FieldIsArray "sType" VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalSemaphoreProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalSemaphoreProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalSemaphoreProperties where
        type FieldType "pNext" VkExternalSemaphoreProperties = Ptr Void
        type FieldOptional "pNext" VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalSemaphoreProperties =
             #{offset VkExternalSemaphoreProperties, pNext}
        type FieldIsArray "pNext" VkExternalSemaphoreProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalSemaphoreProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalSemaphoreProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes"
           VkExternalSemaphoreProperties
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalSemaphoreProperties
             = VkExternalSemaphoreHandleTypeFlags
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalSemaphoreProperties
             =
             #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "exportFromImportedHandleTypes"
           VkExternalSemaphoreProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "exportFromImportedHandleTypes"
           VkExternalSemaphoreProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalSemaphoreProperties
         where
        type FieldType "compatibleHandleTypes"
               VkExternalSemaphoreProperties
             = VkExternalSemaphoreHandleTypeFlags
        type FieldOptional "compatibleHandleTypes"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes"
               VkExternalSemaphoreProperties
             =
             #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}
        type FieldIsArray "compatibleHandleTypes"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "compatibleHandleTypes" VkExternalSemaphoreProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, compatibleHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "compatibleHandleTypes" VkExternalSemaphoreProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "externalSemaphoreFeatures" VkExternalSemaphoreProperties
         where
        type FieldType "externalSemaphoreFeatures"
               VkExternalSemaphoreProperties
             = VkExternalSemaphoreFeatureFlags
        type FieldOptional "externalSemaphoreFeatures"
               VkExternalSemaphoreProperties
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "externalSemaphoreFeatures"
               VkExternalSemaphoreProperties
             =
             #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}
        type FieldIsArray "externalSemaphoreFeatures"
               VkExternalSemaphoreProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "externalSemaphoreFeatures"
           VkExternalSemaphoreProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "externalSemaphoreFeatures"
           VkExternalSemaphoreProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}

instance Show VkExternalSemaphoreProperties where
        showsPrec d x
          = showString "VkExternalSemaphoreProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "exportFromImportedHandleTypes = " .
                            showsPrec d (getField @"exportFromImportedHandleTypes" x) .
                              showString ", " .
                                showString "compatibleHandleTypes = " .
                                  showsPrec d (getField @"compatibleHandleTypes" x) .
                                    showString ", " .
                                      showString "externalSemaphoreFeatures = " .
                                        showsPrec d (getField @"externalSemaphoreFeatures" x) .
                                          showChar '}'
