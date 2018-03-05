#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayPropertiesKHR
       (VkDisplayPropertiesKHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                       (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR (VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Handles                         (VkDisplayKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D               (VkExtent2D)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayPropertiesKHR {
--   >     VkDisplayKHR                     display;
--   >     const char*                      displayName;
--   >     VkExtent2D                       physicalDimensions;
--   >     VkExtent2D                       physicalResolution;
--   >     VkSurfaceTransformFlagsKHR       supportedTransforms;
--   >     VkBool32                         planeReorderPossible;
--   >     VkBool32                         persistentContent;
--   > } VkDisplayPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayPropertiesKHR.html VkDisplayPropertiesKHR registry at www.khronos.org>
data VkDisplayPropertiesKHR = VkDisplayPropertiesKHR## Addr##
                                                      ByteArray##

instance Eq VkDisplayPropertiesKHR where
        (VkDisplayPropertiesKHR## a _) == x@(VkDisplayPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPropertiesKHR where
        (VkDisplayPropertiesKHR## a _) `compare`
          x@(VkDisplayPropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPropertiesKHR where
        sizeOf ~_ = #{size VkDisplayPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPropertiesKHR where
        unsafeAddr (VkDisplayPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPropertiesKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPropertiesKHR where
        type StructFields VkDisplayPropertiesKHR =
             '["display", "displayName", "physicalDimensions", -- ' closing tick for hsc2hs
               "physicalResolution", "supportedTransforms",
               "planeReorderPossible", "persistentContent"]
        type CUnionType VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "display" VkDisplayPropertiesKHR where
        type FieldType "display" VkDisplayPropertiesKHR = VkDisplayKHR
        type FieldOptional "display" VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "display" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, display}
        type FieldIsArray "display" VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPropertiesKHR, display}

instance {-# OVERLAPPING #-}
         CanReadField "display" VkDisplayPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, display})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, display}

instance {-# OVERLAPPING #-}
         CanWriteField "display" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, display}

instance {-# OVERLAPPING #-}
         HasField "displayName" VkDisplayPropertiesKHR where
        type FieldType "displayName" VkDisplayPropertiesKHR = CString
        type FieldOptional "displayName" VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayName" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, displayName}
        type FieldIsArray "displayName" VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, displayName}

instance {-# OVERLAPPING #-}
         CanReadField "displayName" VkDisplayPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, displayName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, displayName}

instance {-# OVERLAPPING #-}
         CanWriteField "displayName" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, displayName}

instance {-# OVERLAPPING #-}
         HasField "physicalDimensions" VkDisplayPropertiesKHR where
        type FieldType "physicalDimensions" VkDisplayPropertiesKHR =
             VkExtent2D
        type FieldOptional "physicalDimensions" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalDimensions" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, physicalDimensions}
        type FieldIsArray "physicalDimensions" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, physicalDimensions}

instance {-# OVERLAPPING #-}
         CanReadField "physicalDimensions" VkDisplayPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, physicalDimensions})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, physicalDimensions}

instance {-# OVERLAPPING #-}
         CanWriteField "physicalDimensions" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, physicalDimensions}

instance {-# OVERLAPPING #-}
         HasField "physicalResolution" VkDisplayPropertiesKHR where
        type FieldType "physicalResolution" VkDisplayPropertiesKHR =
             VkExtent2D
        type FieldOptional "physicalResolution" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalResolution" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, physicalResolution}
        type FieldIsArray "physicalResolution" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, physicalResolution}

instance {-# OVERLAPPING #-}
         CanReadField "physicalResolution" VkDisplayPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, physicalResolution})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, physicalResolution}

instance {-# OVERLAPPING #-}
         CanWriteField "physicalResolution" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, physicalResolution}

instance {-# OVERLAPPING #-}
         HasField "supportedTransforms" VkDisplayPropertiesKHR where
        type FieldType "supportedTransforms" VkDisplayPropertiesKHR =
             VkSurfaceTransformFlagsKHR
        type FieldOptional "supportedTransforms" VkDisplayPropertiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedTransforms" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, supportedTransforms}
        type FieldIsArray "supportedTransforms" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         CanReadField "supportedTransforms" VkDisplayPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, supportedTransforms})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedTransforms" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         HasField "planeReorderPossible" VkDisplayPropertiesKHR where
        type FieldType "planeReorderPossible" VkDisplayPropertiesKHR =
             VkBool32
        type FieldOptional "planeReorderPossible" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "planeReorderPossible" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, planeReorderPossible}
        type FieldIsArray "planeReorderPossible" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, planeReorderPossible}

instance {-# OVERLAPPING #-}
         CanReadField "planeReorderPossible" VkDisplayPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, planeReorderPossible})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, planeReorderPossible}

instance {-# OVERLAPPING #-}
         CanWriteField "planeReorderPossible" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, planeReorderPossible}

instance {-# OVERLAPPING #-}
         HasField "persistentContent" VkDisplayPropertiesKHR where
        type FieldType "persistentContent" VkDisplayPropertiesKHR =
             VkBool32
        type FieldOptional "persistentContent" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "persistentContent" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, persistentContent}
        type FieldIsArray "persistentContent" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, persistentContent}

instance {-# OVERLAPPING #-}
         CanReadField "persistentContent" VkDisplayPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, persistentContent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, persistentContent}

instance {-# OVERLAPPING #-}
         CanWriteField "persistentContent" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, persistentContent}

instance Show VkDisplayPropertiesKHR where
        showsPrec d x
          = showString "VkDisplayPropertiesKHR {" .
              showString "display = " .
                showsPrec d (getField @"display" x) .
                  showString ", " .
                    showString "displayName = " .
                      showsPrec d (getField @"displayName" x) .
                        showString ", " .
                          showString "physicalDimensions = " .
                            showsPrec d (getField @"physicalDimensions" x) .
                              showString ", " .
                                showString "physicalResolution = " .
                                  showsPrec d (getField @"physicalResolution" x) .
                                    showString ", " .
                                      showString "supportedTransforms = " .
                                        showsPrec d (getField @"supportedTransforms" x) .
                                          showString ", " .
                                            showString "planeReorderPossible = " .
                                              showsPrec d (getField @"planeReorderPossible" x) .
                                                showString ", " .
                                                  showString "persistentContent = " .
                                                    showsPrec d (getField @"persistentContent" x) .
                                                      showChar '}'
