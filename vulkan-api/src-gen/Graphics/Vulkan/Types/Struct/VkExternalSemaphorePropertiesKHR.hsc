#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalSemaphorePropertiesKHR
       (VkExternalSemaphorePropertiesKHR(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreFeatureFlagsKHR
                                                                                   (VkExternalSemaphoreFeatureFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlagsKHR
                                                                                   (VkExternalSemaphoreHandleTypeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkExternalSemaphorePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalSemaphoreHandleTypeFlagsKHR exportFromImportedHandleTypes;
--   >     VkExternalSemaphoreHandleTypeFlagsKHR compatibleHandleTypes;
--   >     VkExternalSemaphoreFeatureFlagsKHR externalSemaphoreFeatures;
--   > } VkExternalSemaphorePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalSemaphorePropertiesKHR.html VkExternalSemaphorePropertiesKHR registry at www.khronos.org>
data VkExternalSemaphorePropertiesKHR = VkExternalSemaphorePropertiesKHR## Addr##
                                                                          ByteArray##

instance Eq VkExternalSemaphorePropertiesKHR where
        (VkExternalSemaphorePropertiesKHR## a _) ==
          x@(VkExternalSemaphorePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalSemaphorePropertiesKHR where
        (VkExternalSemaphorePropertiesKHR## a _) `compare`
          x@(VkExternalSemaphorePropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalSemaphorePropertiesKHR where
        sizeOf ~_ = #{size VkExternalSemaphorePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalSemaphorePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalSemaphorePropertiesKHR where
        unsafeAddr (VkExternalSemaphorePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalSemaphorePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalSemaphorePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalSemaphorePropertiesKHR where
        type StructFields VkExternalSemaphorePropertiesKHR =
             '["sType", "pNext", "exportFromImportedHandleTypes", -- ' closing tick for hsc2hs
               "compatibleHandleTypes", "externalSemaphoreFeatures"]
        type CUnionType VkExternalSemaphorePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalSemaphorePropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalSemaphorePropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalSemaphorePropertiesKHR where
        type FieldType "sType" VkExternalSemaphorePropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalSemaphorePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalSemaphorePropertiesKHR =
             #{offset VkExternalSemaphorePropertiesKHR, sType}
        type FieldIsArray "sType" VkExternalSemaphorePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalSemaphorePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalSemaphorePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalSemaphorePropertiesKHR where
        type FieldType "pNext" VkExternalSemaphorePropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkExternalSemaphorePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalSemaphorePropertiesKHR =
             #{offset VkExternalSemaphorePropertiesKHR, pNext}
        type FieldIsArray "pNext" VkExternalSemaphorePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalSemaphorePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalSemaphorePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "exportFromImportedHandleTypes"
           VkExternalSemaphorePropertiesKHR
         where
        type FieldType "exportFromImportedHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreHandleTypeFlagsKHR
        type FieldOptional "exportFromImportedHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "exportFromImportedHandleTypes"
               VkExternalSemaphorePropertiesKHR
             =
             #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}
        type FieldIsArray "exportFromImportedHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "exportFromImportedHandleTypes"
           VkExternalSemaphorePropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "exportFromImportedHandleTypes"
           VkExternalSemaphorePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, exportFromImportedHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "compatibleHandleTypes" VkExternalSemaphorePropertiesKHR
         where
        type FieldType "compatibleHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreHandleTypeFlagsKHR
        type FieldOptional "compatibleHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compatibleHandleTypes"
               VkExternalSemaphorePropertiesKHR
             =
             #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}
        type FieldIsArray "compatibleHandleTypes"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "compatibleHandleTypes"
           VkExternalSemaphorePropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "compatibleHandleTypes"
           VkExternalSemaphorePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, compatibleHandleTypes}

instance {-# OVERLAPPING #-}
         HasField "externalSemaphoreFeatures"
           VkExternalSemaphorePropertiesKHR
         where
        type FieldType "externalSemaphoreFeatures"
               VkExternalSemaphorePropertiesKHR
             = VkExternalSemaphoreFeatureFlagsKHR
        type FieldOptional "externalSemaphoreFeatures"
               VkExternalSemaphorePropertiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "externalSemaphoreFeatures"
               VkExternalSemaphorePropertiesKHR
             =
             #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}
        type FieldIsArray "externalSemaphoreFeatures"
               VkExternalSemaphorePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "externalSemaphoreFeatures"
           VkExternalSemaphorePropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "externalSemaphoreFeatures"
           VkExternalSemaphorePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalSemaphorePropertiesKHR, externalSemaphoreFeatures}

instance Show VkExternalSemaphorePropertiesKHR where
        showsPrec d x
          = showString "VkExternalSemaphorePropertiesKHR {" .
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
