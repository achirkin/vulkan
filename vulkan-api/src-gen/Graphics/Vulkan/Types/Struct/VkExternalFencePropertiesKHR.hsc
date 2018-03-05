#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalFencePropertiesKHR
       (VkExternalFencePropertiesKHR(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlagsKHR    (VkExternalFenceFeatureFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR (VkExternalFenceHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkExternalFencePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalFenceHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalFenceHandleTypeFlagsKHR compatibleHandleTypes;
--   >     VkExternalFenceFeatureFlagsKHR externalFenceFeatures;
--   > } VkExternalFencePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalFencePropertiesKHR.html VkExternalFencePropertiesKHR registry at www.khronos.org>
data VkExternalFencePropertiesKHR = VkExternalFencePropertiesKHR## Addr##
                                                                  ByteArray##

instance Eq VkExternalFencePropertiesKHR where
        (VkExternalFencePropertiesKHR## a _) ==
          x@(VkExternalFencePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalFencePropertiesKHR where
        (VkExternalFencePropertiesKHR## a _) `compare`
          x@(VkExternalFencePropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalFencePropertiesKHR where
        sizeOf ~_ = #{size VkExternalFencePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalFencePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalFencePropertiesKHR where
        unsafeAddr (VkExternalFencePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalFencePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalFencePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalFencePropertiesKHR where
        type StructFields VkExternalFencePropertiesKHR =
             '["sType", "pNext", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes", "externalFenceFeatures"]
        type CUnionType VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalFencePropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalFencePropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalFencePropertiesKHR where
        type FieldType "sType" VkExternalFencePropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalFencePropertiesKHR =
             #{offset VkExternalFencePropertiesKHR, sType}
        type FieldIsArray "sType" VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalFencePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalFencePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalFencePropertiesKHR where
        type FieldType "pNext" VkExternalFencePropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalFencePropertiesKHR =
             #{offset VkExternalFencePropertiesKHR, pNext}
        type FieldIsArray "pNext" VkExternalFencePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalFencePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalFencePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes"
           VkExternalFencePropertiesKHR
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalFencePropertiesKHR
             = VkExternalFenceHandleTypeFlagsKHR
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalFencePropertiesKHR
             =
             #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "exportFromImportedHandleTypes"
           VkExternalFencePropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "exportFromImportedHandleTypes"
           VkExternalFencePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalFencePropertiesKHR where
        type FieldType "compatibleHandleTypes" VkExternalFencePropertiesKHR
             = VkExternalFenceHandleTypeFlagsKHR
        type FieldOptional "compatibleHandleTypes"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes"
               VkExternalFencePropertiesKHR
             =
             #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}
        type FieldIsArray "compatibleHandleTypes"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "compatibleHandleTypes" VkExternalFencePropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "compatibleHandleTypes" VkExternalFencePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "externalFenceFeatures" VkExternalFencePropertiesKHR where
        type FieldType "externalFenceFeatures" VkExternalFencePropertiesKHR
             = VkExternalFenceFeatureFlagsKHR
        type FieldOptional "externalFenceFeatures"
               VkExternalFencePropertiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "externalFenceFeatures"
               VkExternalFencePropertiesKHR
             =
             #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}
        type FieldIsArray "externalFenceFeatures"
               VkExternalFencePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "externalFenceFeatures" VkExternalFencePropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFencePropertiesKHR, externalFenceFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "externalFenceFeatures" VkExternalFencePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFencePropertiesKHR, externalFenceFeatures}

instance Show VkExternalFencePropertiesKHR where
        showsPrec d x
          = showString "VkExternalFencePropertiesKHR {" .
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
                                      showString "externalFenceFeatures = " .
                                        showsPrec d (getField @"externalFenceFeatures" x) .
                                          showChar '}'
