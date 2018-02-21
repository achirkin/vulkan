#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayPlanePropertiesKHR
       (VkDisplayPlanePropertiesKHR(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Handles       (VkDisplayKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayPlanePropertiesKHR {
--   >     VkDisplayKHR                     currentDisplay;
--   >     uint32_t                         currentStackIndex;
--   > } VkDisplayPlanePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayPlanePropertiesKHR.html VkDisplayPlanePropertiesKHR registry at www.khronos.org>
data VkDisplayPlanePropertiesKHR = VkDisplayPlanePropertiesKHR## Addr##
                                                                ByteArray##

instance Eq VkDisplayPlanePropertiesKHR where
        (VkDisplayPlanePropertiesKHR## a _) ==
          x@(VkDisplayPlanePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlanePropertiesKHR where
        (VkDisplayPlanePropertiesKHR## a _) `compare`
          x@(VkDisplayPlanePropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlanePropertiesKHR where
        sizeOf ~_ = #{size VkDisplayPlanePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPlanePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPlanePropertiesKHR where
        unsafeAddr (VkDisplayPlanePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPlanePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPlanePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPlanePropertiesKHR where
        type StructFields VkDisplayPlanePropertiesKHR =
             '["currentDisplay", "currentStackIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPlanePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPlanePropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPlanePropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkCurrentDisplay VkDisplayPlanePropertiesKHR where
        type VkCurrentDisplayMType VkDisplayPlanePropertiesKHR =
             VkDisplayKHR

        {-# NOINLINE vkCurrentDisplay #-}
        vkCurrentDisplay x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlanePropertiesKHR, currentDisplay})

        {-# INLINE vkCurrentDisplayByteOffset #-}
        vkCurrentDisplayByteOffset ~_
          = #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

        {-# INLINE readVkCurrentDisplay #-}
        readVkCurrentDisplay p
          = peekByteOff p #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

        {-# INLINE writeVkCurrentDisplay #-}
        writeVkCurrentDisplay p
          = pokeByteOff p #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

instance {-# OVERLAPPING #-}
         HasField "currentDisplay" VkDisplayPlanePropertiesKHR where
        type FieldType "currentDisplay" VkDisplayPlanePropertiesKHR =
             VkDisplayKHR
        type FieldOptional "currentDisplay" VkDisplayPlanePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "currentDisplay" VkDisplayPlanePropertiesKHR =
             #{offset VkDisplayPlanePropertiesKHR, currentDisplay}
        type FieldIsArray "currentDisplay" VkDisplayPlanePropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

instance CanReadField "currentDisplay" VkDisplayPlanePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCurrentDisplay

        {-# INLINE readField #-}
        readField = readVkCurrentDisplay

instance {-# OVERLAPPING #-}
         HasVkCurrentStackIndex VkDisplayPlanePropertiesKHR where
        type VkCurrentStackIndexMType VkDisplayPlanePropertiesKHR = Word32

        {-# NOINLINE vkCurrentStackIndex #-}
        vkCurrentStackIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlanePropertiesKHR, currentStackIndex})

        {-# INLINE vkCurrentStackIndexByteOffset #-}
        vkCurrentStackIndexByteOffset ~_
          = #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

        {-# INLINE readVkCurrentStackIndex #-}
        readVkCurrentStackIndex p
          = peekByteOff p #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

        {-# INLINE writeVkCurrentStackIndex #-}
        writeVkCurrentStackIndex p
          = pokeByteOff p #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

instance {-# OVERLAPPING #-}
         HasField "currentStackIndex" VkDisplayPlanePropertiesKHR where
        type FieldType "currentStackIndex" VkDisplayPlanePropertiesKHR =
             Word32
        type FieldOptional "currentStackIndex" VkDisplayPlanePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "currentStackIndex" VkDisplayPlanePropertiesKHR =
             #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}
        type FieldIsArray "currentStackIndex" VkDisplayPlanePropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

instance CanReadField "currentStackIndex"
           VkDisplayPlanePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCurrentStackIndex

        {-# INLINE readField #-}
        readField = readVkCurrentStackIndex

instance Show VkDisplayPlanePropertiesKHR where
        showsPrec d x
          = showString "VkDisplayPlanePropertiesKHR {" .
              showString "vkCurrentDisplay = " .
                showsPrec d (vkCurrentDisplay x) .
                  showString ", " .
                    showString "vkCurrentStackIndex = " .
                      showsPrec d (vkCurrentStackIndex x) . showChar '}'
