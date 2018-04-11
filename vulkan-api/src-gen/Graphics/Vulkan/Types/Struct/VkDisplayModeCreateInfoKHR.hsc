#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayModeCreateInfoKHR
       (VkDisplayModeCreateInfoKHR(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Base                                                (Addr##, ByteArray##,
                                                                          byteArrayContents##,
                                                                          plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                          (VkDisplayModeCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType              (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDisplayModeParametersKHR (VkDisplayModeParametersKHR)
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayModeCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayModeCreateFlagsKHR      flags;
--   >     VkDisplayModeParametersKHR       parameters;
--   > } VkDisplayModeCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayModeCreateInfoKHR VkDisplayModeCreateInfoKHR registry at www.khronos.org>
data VkDisplayModeCreateInfoKHR = VkDisplayModeCreateInfoKHR## Addr##
                                                              ByteArray##

instance Eq VkDisplayModeCreateInfoKHR where
        (VkDisplayModeCreateInfoKHR## a _) ==
          x@(VkDisplayModeCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModeCreateInfoKHR where
        (VkDisplayModeCreateInfoKHR## a _) `compare`
          x@(VkDisplayModeCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayModeCreateInfoKHR where
        sizeOf ~_ = #{size VkDisplayModeCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModeCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayModeCreateInfoKHR where
        unsafeAddr (VkDisplayModeCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayModeCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayModeCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayModeCreateInfoKHR where
        type StructFields VkDisplayModeCreateInfoKHR =
             '["sType", "pNext", "flags", "parameters"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayModeCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayModeCreateInfoKHR where
        type FieldType "sType" VkDisplayModeCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayModeCreateInfoKHR =
             #{offset VkDisplayModeCreateInfoKHR, sType}
        type FieldIsArray "sType" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayModeCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayModeCreateInfoKHR where
        type FieldType "pNext" VkDisplayModeCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayModeCreateInfoKHR =
             #{offset VkDisplayModeCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayModeCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDisplayModeCreateInfoKHR where
        type FieldType "flags" VkDisplayModeCreateInfoKHR =
             VkDisplayModeCreateFlagsKHR
        type FieldOptional "flags" VkDisplayModeCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDisplayModeCreateInfoKHR =
             #{offset VkDisplayModeCreateInfoKHR, flags}
        type FieldIsArray "flags" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDisplayModeCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "parameters" VkDisplayModeCreateInfoKHR where
        type FieldType "parameters" VkDisplayModeCreateInfoKHR =
             VkDisplayModeParametersKHR
        type FieldOptional "parameters" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "parameters" VkDisplayModeCreateInfoKHR =
             #{offset VkDisplayModeCreateInfoKHR, parameters}
        type FieldIsArray "parameters" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeCreateInfoKHR, parameters}

instance {-# OVERLAPPING #-}
         CanReadField "parameters" VkDisplayModeCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, parameters})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, parameters}

instance {-# OVERLAPPING #-}
         CanWriteField "parameters" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, parameters}

instance Show VkDisplayModeCreateInfoKHR where
        showsPrec d x
          = showString "VkDisplayModeCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "parameters = " .
                                  showsPrec d (getField @"parameters" x) . showChar '}'
