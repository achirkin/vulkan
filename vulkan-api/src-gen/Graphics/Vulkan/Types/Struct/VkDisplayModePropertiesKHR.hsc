#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayModePropertiesKHR
       (VkDisplayModePropertiesKHR(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Handles                           (VkDisplayModeKHR)
import           Graphics.Vulkan.Types.Struct.VkDisplayModeParametersKHR (VkDisplayModeParametersKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayModePropertiesKHR {
--   >     VkDisplayModeKHR                 displayMode;
--   >     VkDisplayModeParametersKHR       parameters;
--   > } VkDisplayModePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayModePropertiesKHR.html VkDisplayModePropertiesKHR registry at www.khronos.org>
data VkDisplayModePropertiesKHR = VkDisplayModePropertiesKHR## Addr##
                                                              ByteArray##

instance Eq VkDisplayModePropertiesKHR where
        (VkDisplayModePropertiesKHR## a _) ==
          x@(VkDisplayModePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModePropertiesKHR where
        (VkDisplayModePropertiesKHR## a _) `compare`
          x@(VkDisplayModePropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayModePropertiesKHR where
        sizeOf ~_ = #{size VkDisplayModePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayModePropertiesKHR where
        unsafeAddr (VkDisplayModePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayModePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayModePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayModePropertiesKHR where
        type StructFields VkDisplayModePropertiesKHR =
             '["displayMode", "parameters"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayModePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayModePropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayModePropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDisplayMode VkDisplayModePropertiesKHR where
        type VkDisplayModeMType VkDisplayModePropertiesKHR =
             VkDisplayModeKHR

        {-# NOINLINE vkDisplayMode #-}
        vkDisplayMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModePropertiesKHR, displayMode})

        {-# INLINE vkDisplayModeByteOffset #-}
        vkDisplayModeByteOffset ~_
          = #{offset VkDisplayModePropertiesKHR, displayMode}

        {-# INLINE readVkDisplayMode #-}
        readVkDisplayMode p
          = peekByteOff p #{offset VkDisplayModePropertiesKHR, displayMode}

        {-# INLINE writeVkDisplayMode #-}
        writeVkDisplayMode p
          = pokeByteOff p #{offset VkDisplayModePropertiesKHR, displayMode}

instance {-# OVERLAPPING #-}
         HasField "displayMode" VkDisplayModePropertiesKHR where
        type FieldType "displayMode" VkDisplayModePropertiesKHR =
             VkDisplayModeKHR
        type FieldOptional "displayMode" VkDisplayModePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "displayMode" VkDisplayModePropertiesKHR =
             #{offset VkDisplayModePropertiesKHR, displayMode}
        type FieldIsArray "displayMode" VkDisplayModePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModePropertiesKHR, displayMode}

instance CanReadField "displayMode" VkDisplayModePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkDisplayMode

        {-# INLINE readField #-}
        readField = readVkDisplayMode

instance CanWriteField "displayMode" VkDisplayModePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDisplayMode

instance {-# OVERLAPPING #-}
         HasVkParameters VkDisplayModePropertiesKHR where
        type VkParametersMType VkDisplayModePropertiesKHR =
             VkDisplayModeParametersKHR

        {-# NOINLINE vkParameters #-}
        vkParameters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModePropertiesKHR, parameters})

        {-# INLINE vkParametersByteOffset #-}
        vkParametersByteOffset ~_
          = #{offset VkDisplayModePropertiesKHR, parameters}

        {-# INLINE readVkParameters #-}
        readVkParameters p
          = peekByteOff p #{offset VkDisplayModePropertiesKHR, parameters}

        {-# INLINE writeVkParameters #-}
        writeVkParameters p
          = pokeByteOff p #{offset VkDisplayModePropertiesKHR, parameters}

instance {-# OVERLAPPING #-}
         HasField "parameters" VkDisplayModePropertiesKHR where
        type FieldType "parameters" VkDisplayModePropertiesKHR =
             VkDisplayModeParametersKHR
        type FieldOptional "parameters" VkDisplayModePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "parameters" VkDisplayModePropertiesKHR =
             #{offset VkDisplayModePropertiesKHR, parameters}
        type FieldIsArray "parameters" VkDisplayModePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModePropertiesKHR, parameters}

instance CanReadField "parameters" VkDisplayModePropertiesKHR where
        {-# INLINE getField #-}
        getField = vkParameters

        {-# INLINE readField #-}
        readField = readVkParameters

instance CanWriteField "parameters" VkDisplayModePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkParameters

instance Show VkDisplayModePropertiesKHR where
        showsPrec d x
          = showString "VkDisplayModePropertiesKHR {" .
              showString "vkDisplayMode = " .
                showsPrec d (vkDisplayMode x) .
                  showString ", " .
                    showString "vkParameters = " .
                      showsPrec d (vkParameters x) . showChar '}'
