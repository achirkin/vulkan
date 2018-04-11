#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayModeParametersKHR
       (VkDisplayModeParametersKHR(..)) where
import           Foreign.Storable                        (Storable (..))
import           GHC.Base                                (Addr##, ByteArray##,
                                                          byteArrayContents##,
                                                          plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkExtent2D (VkExtent2D)
import           System.IO.Unsafe                        (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayModeParametersKHR {
--   >     VkExtent2D                       visibleRegion;
--   >     uint32_t                         refreshRate;
--   > } VkDisplayModeParametersKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayModeParametersKHR VkDisplayModeParametersKHR registry at www.khronos.org>
data VkDisplayModeParametersKHR = VkDisplayModeParametersKHR## Addr##
                                                              ByteArray##

instance Eq VkDisplayModeParametersKHR where
        (VkDisplayModeParametersKHR## a _) ==
          x@(VkDisplayModeParametersKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModeParametersKHR where
        (VkDisplayModeParametersKHR## a _) `compare`
          x@(VkDisplayModeParametersKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayModeParametersKHR where
        sizeOf ~_ = #{size VkDisplayModeParametersKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModeParametersKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayModeParametersKHR where
        unsafeAddr (VkDisplayModeParametersKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayModeParametersKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayModeParametersKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayModeParametersKHR where
        type StructFields VkDisplayModeParametersKHR =
             '["visibleRegion", "refreshRate"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayModeParametersKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayModeParametersKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayModeParametersKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "visibleRegion" VkDisplayModeParametersKHR where
        type FieldType "visibleRegion" VkDisplayModeParametersKHR =
             VkExtent2D
        type FieldOptional "visibleRegion" VkDisplayModeParametersKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "visibleRegion" VkDisplayModeParametersKHR =
             #{offset VkDisplayModeParametersKHR, visibleRegion}
        type FieldIsArray "visibleRegion" VkDisplayModeParametersKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeParametersKHR, visibleRegion}

instance {-# OVERLAPPING #-}
         CanReadField "visibleRegion" VkDisplayModeParametersKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeParametersKHR, visibleRegion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeParametersKHR, visibleRegion}

instance {-# OVERLAPPING #-}
         CanWriteField "visibleRegion" VkDisplayModeParametersKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeParametersKHR, visibleRegion}

instance {-# OVERLAPPING #-}
         HasField "refreshRate" VkDisplayModeParametersKHR where
        type FieldType "refreshRate" VkDisplayModeParametersKHR = Word32
        type FieldOptional "refreshRate" VkDisplayModeParametersKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "refreshRate" VkDisplayModeParametersKHR =
             #{offset VkDisplayModeParametersKHR, refreshRate}
        type FieldIsArray "refreshRate" VkDisplayModeParametersKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeParametersKHR, refreshRate}

instance {-# OVERLAPPING #-}
         CanReadField "refreshRate" VkDisplayModeParametersKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeParametersKHR, refreshRate})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeParametersKHR, refreshRate}

instance {-# OVERLAPPING #-}
         CanWriteField "refreshRate" VkDisplayModeParametersKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeParametersKHR, refreshRate}

instance Show VkDisplayModeParametersKHR where
        showsPrec d x
          = showString "VkDisplayModeParametersKHR {" .
              showString "visibleRegion = " .
                showsPrec d (getField @"visibleRegion" x) .
                  showString ", " .
                    showString "refreshRate = " .
                      showsPrec d (getField @"refreshRate" x) . showChar '}'
