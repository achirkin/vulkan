#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Display
       (VkDisplayEventInfoEXT(..), VkDisplayModeCreateInfoKHR(..),
        VkDisplayModeParametersKHR(..), VkDisplayModeProperties2KHR(..),
        VkDisplayModePropertiesKHR(..), VkDisplayPlaneCapabilities2KHR(..),
        VkDisplayPlaneCapabilitiesKHR(..), VkDisplayPlaneInfo2KHR(..),
        VkDisplayPlaneProperties2KHR(..), VkDisplayPlanePropertiesKHR(..),
        VkDisplayPowerInfoEXT(..), VkDisplayPresentInfoKHR(..),
        VkDisplayProperties2KHR(..), VkDisplayPropertiesKHR(..),
        VkDisplaySurfaceCreateInfoKHR(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks           (VkDisplayModeCreateFlagsKHR,
                                                           VkDisplaySurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.Display       (VkDisplayEventTypeEXT,
                                                           VkDisplayPlaneAlphaFlagBitsKHR,
                                                           VkDisplayPlaneAlphaFlagsKHR,
                                                           VkDisplayPowerStateEXT)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Enum.Surface       (VkSurfaceTransformFlagBitsKHR,
                                                           VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Handles            (VkDisplayKHR,
                                                           VkDisplayModeKHR)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Offset      (VkOffset2D)
import           Graphics.Vulkan.Types.Struct.Present     (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.Rect        (VkRect2D)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayEventInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayEventTypeEXT            displayEvent;
--   > } VkDisplayEventInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayEventInfoEXT VkDisplayEventInfoEXT registry at www.khronos.org>
data VkDisplayEventInfoEXT = VkDisplayEventInfoEXT## Addr##
                                                    ByteArray##

instance Eq VkDisplayEventInfoEXT where
        (VkDisplayEventInfoEXT## a _) == x@(VkDisplayEventInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayEventInfoEXT where
        (VkDisplayEventInfoEXT## a _) `compare`
          x@(VkDisplayEventInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayEventInfoEXT where
        sizeOf ~_ = #{size VkDisplayEventInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayEventInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayEventInfoEXT where
        unsafeAddr (VkDisplayEventInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayEventInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayEventInfoEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayEventInfoEXT where
        type StructFields VkDisplayEventInfoEXT =
             '["sType", "pNext", "displayEvent"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayEventInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDisplayEventInfoEXT
         where
        type FieldType "sType" VkDisplayEventInfoEXT = VkStructureType
        type FieldOptional "sType" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayEventInfoEXT =
             #{offset VkDisplayEventInfoEXT, sType}
        type FieldIsArray "sType" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayEventInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDisplayEventInfoEXT
         where
        type FieldType "pNext" VkDisplayEventInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayEventInfoEXT =
             #{offset VkDisplayEventInfoEXT, pNext}
        type FieldIsArray "pNext" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "displayEvent" VkDisplayEventInfoEXT where
        type FieldType "displayEvent" VkDisplayEventInfoEXT =
             VkDisplayEventTypeEXT
        type FieldOptional "displayEvent" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayEvent" VkDisplayEventInfoEXT =
             #{offset VkDisplayEventInfoEXT, displayEvent}
        type FieldIsArray "displayEvent" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayEventInfoEXT, displayEvent}

instance {-# OVERLAPPING #-}
         CanReadField "displayEvent" VkDisplayEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, displayEvent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, displayEvent}

instance {-# OVERLAPPING #-}
         CanWriteField "displayEvent" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, displayEvent}

instance Show VkDisplayEventInfoEXT where
        showsPrec d x
          = showString "VkDisplayEventInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "displayEvent = " .
                            showsPrec d (getField @"displayEvent" x) . showChar '}'

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

-- | > typedef struct VkDisplayModeProperties2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDisplayModePropertiesKHR displayModeProperties;
--   > } VkDisplayModeProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayModeProperties2KHR VkDisplayModeProperties2KHR registry at www.khronos.org>
data VkDisplayModeProperties2KHR = VkDisplayModeProperties2KHR## Addr##
                                                                ByteArray##

instance Eq VkDisplayModeProperties2KHR where
        (VkDisplayModeProperties2KHR## a _) ==
          x@(VkDisplayModeProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModeProperties2KHR where
        (VkDisplayModeProperties2KHR## a _) `compare`
          x@(VkDisplayModeProperties2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayModeProperties2KHR where
        sizeOf ~_ = #{size VkDisplayModeProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModeProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayModeProperties2KHR where
        unsafeAddr (VkDisplayModeProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayModeProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayModeProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayModeProperties2KHR where
        type StructFields VkDisplayModeProperties2KHR =
             '["sType", "pNext", "displayModeProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayModeProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayModeProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayModeProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayModeProperties2KHR where
        type FieldType "sType" VkDisplayModeProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkDisplayModeProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayModeProperties2KHR =
             #{offset VkDisplayModeProperties2KHR, sType}
        type FieldIsArray "sType" VkDisplayModeProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayModeProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeProperties2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayModeProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayModeProperties2KHR where
        type FieldType "pNext" VkDisplayModeProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkDisplayModeProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayModeProperties2KHR =
             #{offset VkDisplayModeProperties2KHR, pNext}
        type FieldIsArray "pNext" VkDisplayModeProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayModeProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeProperties2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayModeProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "displayModeProperties" VkDisplayModeProperties2KHR where
        type FieldType "displayModeProperties" VkDisplayModeProperties2KHR
             = VkDisplayModePropertiesKHR
        type FieldOptional "displayModeProperties"
               VkDisplayModeProperties2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayModeProperties"
               VkDisplayModeProperties2KHR
             =
             #{offset VkDisplayModeProperties2KHR, displayModeProperties}
        type FieldIsArray "displayModeProperties"
               VkDisplayModeProperties2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeProperties2KHR, displayModeProperties}

instance {-# OVERLAPPING #-}
         CanReadField "displayModeProperties" VkDisplayModeProperties2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeProperties2KHR, displayModeProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModeProperties2KHR, displayModeProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "displayModeProperties" VkDisplayModeProperties2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModeProperties2KHR, displayModeProperties}

instance Show VkDisplayModeProperties2KHR where
        showsPrec d x
          = showString "VkDisplayModeProperties2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "displayModeProperties = " .
                            showsPrec d (getField @"displayModeProperties" x) . showChar '}'

-- | > typedef struct VkDisplayModePropertiesKHR {
--   >     VkDisplayModeKHR                 displayMode;
--   >     VkDisplayModeParametersKHR       parameters;
--   > } VkDisplayModePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayModePropertiesKHR VkDisplayModePropertiesKHR registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "displayMode" VkDisplayModePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModePropertiesKHR, displayMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModePropertiesKHR, displayMode}

instance {-# OVERLAPPING #-}
         CanWriteField "displayMode" VkDisplayModePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModePropertiesKHR, displayMode}

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

instance {-# OVERLAPPING #-}
         CanReadField "parameters" VkDisplayModePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModePropertiesKHR, parameters})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayModePropertiesKHR, parameters}

instance {-# OVERLAPPING #-}
         CanWriteField "parameters" VkDisplayModePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayModePropertiesKHR, parameters}

instance Show VkDisplayModePropertiesKHR where
        showsPrec d x
          = showString "VkDisplayModePropertiesKHR {" .
              showString "displayMode = " .
                showsPrec d (getField @"displayMode" x) .
                  showString ", " .
                    showString "parameters = " .
                      showsPrec d (getField @"parameters" x) . showChar '}'

-- | > typedef struct VkDisplayPlaneCapabilities2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDisplayPlaneCapabilitiesKHR capabilities;
--   > } VkDisplayPlaneCapabilities2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlaneCapabilities2KHR VkDisplayPlaneCapabilities2KHR registry at www.khronos.org>
data VkDisplayPlaneCapabilities2KHR = VkDisplayPlaneCapabilities2KHR## Addr##
                                                                      ByteArray##

instance Eq VkDisplayPlaneCapabilities2KHR where
        (VkDisplayPlaneCapabilities2KHR## a _) ==
          x@(VkDisplayPlaneCapabilities2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlaneCapabilities2KHR where
        (VkDisplayPlaneCapabilities2KHR## a _) `compare`
          x@(VkDisplayPlaneCapabilities2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlaneCapabilities2KHR where
        sizeOf ~_ = #{size VkDisplayPlaneCapabilities2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplayPlaneCapabilities2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPlaneCapabilities2KHR where
        unsafeAddr (VkDisplayPlaneCapabilities2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPlaneCapabilities2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPlaneCapabilities2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPlaneCapabilities2KHR where
        type StructFields VkDisplayPlaneCapabilities2KHR =
             '["sType", "pNext", "capabilities"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPlaneCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPlaneCapabilities2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPlaneCapabilities2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayPlaneCapabilities2KHR where
        type FieldType "sType" VkDisplayPlaneCapabilities2KHR =
             VkStructureType
        type FieldOptional "sType" VkDisplayPlaneCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayPlaneCapabilities2KHR =
             #{offset VkDisplayPlaneCapabilities2KHR, sType}
        type FieldIsArray "sType" VkDisplayPlaneCapabilities2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayPlaneCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilities2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayPlaneCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayPlaneCapabilities2KHR where
        type FieldType "pNext" VkDisplayPlaneCapabilities2KHR = Ptr Void
        type FieldOptional "pNext" VkDisplayPlaneCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayPlaneCapabilities2KHR =
             #{offset VkDisplayPlaneCapabilities2KHR, pNext}
        type FieldIsArray "pNext" VkDisplayPlaneCapabilities2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayPlaneCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilities2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayPlaneCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "capabilities" VkDisplayPlaneCapabilities2KHR where
        type FieldType "capabilities" VkDisplayPlaneCapabilities2KHR =
             VkDisplayPlaneCapabilitiesKHR
        type FieldOptional "capabilities" VkDisplayPlaneCapabilities2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "capabilities" VkDisplayPlaneCapabilities2KHR =
             #{offset VkDisplayPlaneCapabilities2KHR, capabilities}
        type FieldIsArray "capabilities" VkDisplayPlaneCapabilities2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilities2KHR, capabilities}

instance {-# OVERLAPPING #-}
         CanReadField "capabilities" VkDisplayPlaneCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilities2KHR, capabilities})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilities2KHR, capabilities}

instance {-# OVERLAPPING #-}
         CanWriteField "capabilities" VkDisplayPlaneCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilities2KHR, capabilities}

instance Show VkDisplayPlaneCapabilities2KHR where
        showsPrec d x
          = showString "VkDisplayPlaneCapabilities2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "capabilities = " .
                            showsPrec d (getField @"capabilities" x) . showChar '}'

-- | > typedef struct VkDisplayPlaneCapabilitiesKHR {
--   >     VkDisplayPlaneAlphaFlagsKHR      supportedAlpha;
--   >     VkOffset2D                       minSrcPosition;
--   >     VkOffset2D                       maxSrcPosition;
--   >     VkExtent2D                       minSrcExtent;
--   >     VkExtent2D                       maxSrcExtent;
--   >     VkOffset2D                       minDstPosition;
--   >     VkOffset2D                       maxDstPosition;
--   >     VkExtent2D                       minDstExtent;
--   >     VkExtent2D                       maxDstExtent;
--   > } VkDisplayPlaneCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlaneCapabilitiesKHR VkDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
data VkDisplayPlaneCapabilitiesKHR = VkDisplayPlaneCapabilitiesKHR## Addr##
                                                                    ByteArray##

instance Eq VkDisplayPlaneCapabilitiesKHR where
        (VkDisplayPlaneCapabilitiesKHR## a _) ==
          x@(VkDisplayPlaneCapabilitiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlaneCapabilitiesKHR where
        (VkDisplayPlaneCapabilitiesKHR## a _) `compare`
          x@(VkDisplayPlaneCapabilitiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlaneCapabilitiesKHR where
        sizeOf ~_ = #{size VkDisplayPlaneCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplayPlaneCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPlaneCapabilitiesKHR where
        unsafeAddr (VkDisplayPlaneCapabilitiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPlaneCapabilitiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPlaneCapabilitiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPlaneCapabilitiesKHR where
        type StructFields VkDisplayPlaneCapabilitiesKHR =
             '["supportedAlpha", "minSrcPosition", "maxSrcPosition", -- ' closing tick for hsc2hs
               "minSrcExtent", "maxSrcExtent", "minDstPosition", "maxDstPosition",
               "minDstExtent", "maxDstExtent"]
        type CUnionType VkDisplayPlaneCapabilitiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPlaneCapabilitiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPlaneCapabilitiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "supportedAlpha" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             VkDisplayPlaneAlphaFlagsKHR
        type FieldOptional "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}
        type FieldIsArray "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance {-# OVERLAPPING #-}
         CanReadField "supportedAlpha" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedAlpha" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance {-# OVERLAPPING #-}
         HasField "minSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}
        type FieldIsArray "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance {-# OVERLAPPING #-}
         CanReadField "minSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance {-# OVERLAPPING #-}
         CanWriteField "minSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance {-# OVERLAPPING #-}
         HasField "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}
        type FieldIsArray "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance {-# OVERLAPPING #-}
         CanReadField "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance {-# OVERLAPPING #-}
         HasField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}
        type FieldIsArray "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance {-# OVERLAPPING #-}
         CanReadField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance {-# OVERLAPPING #-}
         HasField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}
        type FieldIsArray "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance {-# OVERLAPPING #-}
         HasField "minDstPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}
        type FieldIsArray "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance {-# OVERLAPPING #-}
         CanReadField "minDstPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance {-# OVERLAPPING #-}
         CanWriteField "minDstPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance {-# OVERLAPPING #-}
         HasField "maxDstPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}
        type FieldIsArray "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance {-# OVERLAPPING #-}
         CanReadField "maxDstPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDstPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance {-# OVERLAPPING #-}
         HasField "minDstExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}
        type FieldIsArray "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance {-# OVERLAPPING #-}
         CanReadField "minDstExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "minDstExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance {-# OVERLAPPING #-}
         HasField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}
        type FieldIsArray "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance Show VkDisplayPlaneCapabilitiesKHR where
        showsPrec d x
          = showString "VkDisplayPlaneCapabilitiesKHR {" .
              showString "supportedAlpha = " .
                showsPrec d (getField @"supportedAlpha" x) .
                  showString ", " .
                    showString "minSrcPosition = " .
                      showsPrec d (getField @"minSrcPosition" x) .
                        showString ", " .
                          showString "maxSrcPosition = " .
                            showsPrec d (getField @"maxSrcPosition" x) .
                              showString ", " .
                                showString "minSrcExtent = " .
                                  showsPrec d (getField @"minSrcExtent" x) .
                                    showString ", " .
                                      showString "maxSrcExtent = " .
                                        showsPrec d (getField @"maxSrcExtent" x) .
                                          showString ", " .
                                            showString "minDstPosition = " .
                                              showsPrec d (getField @"minDstPosition" x) .
                                                showString ", " .
                                                  showString "maxDstPosition = " .
                                                    showsPrec d (getField @"maxDstPosition" x) .
                                                      showString ", " .
                                                        showString "minDstExtent = " .
                                                          showsPrec d (getField @"minDstExtent" x) .
                                                            showString ", " .
                                                              showString "maxDstExtent = " .
                                                                showsPrec d
                                                                  (getField @"maxDstExtent" x)
                                                                  . showChar '}'

-- | > typedef struct VkDisplayPlaneInfo2KHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkDisplayModeKHR mode;
--   >     uint32_t planeIndex;
--   > } VkDisplayPlaneInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlaneInfo2KHR VkDisplayPlaneInfo2KHR registry at www.khronos.org>
data VkDisplayPlaneInfo2KHR = VkDisplayPlaneInfo2KHR## Addr##
                                                      ByteArray##

instance Eq VkDisplayPlaneInfo2KHR where
        (VkDisplayPlaneInfo2KHR## a _) == x@(VkDisplayPlaneInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlaneInfo2KHR where
        (VkDisplayPlaneInfo2KHR## a _) `compare`
          x@(VkDisplayPlaneInfo2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlaneInfo2KHR where
        sizeOf ~_ = #{size VkDisplayPlaneInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPlaneInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPlaneInfo2KHR where
        unsafeAddr (VkDisplayPlaneInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPlaneInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPlaneInfo2KHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPlaneInfo2KHR where
        type StructFields VkDisplayPlaneInfo2KHR =
             '["sType", "pNext", "mode", "planeIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPlaneInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayPlaneInfo2KHR where
        type FieldType "sType" VkDisplayPlaneInfo2KHR = VkStructureType
        type FieldOptional "sType" VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayPlaneInfo2KHR =
             #{offset VkDisplayPlaneInfo2KHR, sType}
        type FieldIsArray "sType" VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPlaneInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayPlaneInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneInfo2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayPlaneInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayPlaneInfo2KHR where
        type FieldType "pNext" VkDisplayPlaneInfo2KHR = Ptr Void
        type FieldOptional "pNext" VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayPlaneInfo2KHR =
             #{offset VkDisplayPlaneInfo2KHR, pNext}
        type FieldIsArray "pNext" VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPlaneInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayPlaneInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneInfo2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayPlaneInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneInfo2KHR, pNext}

instance {-# OVERLAPPING #-} HasField "mode" VkDisplayPlaneInfo2KHR
         where
        type FieldType "mode" VkDisplayPlaneInfo2KHR = VkDisplayModeKHR
        type FieldOptional "mode" VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mode" VkDisplayPlaneInfo2KHR =
             #{offset VkDisplayPlaneInfo2KHR, mode}
        type FieldIsArray "mode" VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPlaneInfo2KHR, mode}

instance {-# OVERLAPPING #-}
         CanReadField "mode" VkDisplayPlaneInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneInfo2KHR, mode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneInfo2KHR, mode}

instance {-# OVERLAPPING #-}
         CanWriteField "mode" VkDisplayPlaneInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneInfo2KHR, mode}

instance {-# OVERLAPPING #-}
         HasField "planeIndex" VkDisplayPlaneInfo2KHR where
        type FieldType "planeIndex" VkDisplayPlaneInfo2KHR = Word32
        type FieldOptional "planeIndex" VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "planeIndex" VkDisplayPlaneInfo2KHR =
             #{offset VkDisplayPlaneInfo2KHR, planeIndex}
        type FieldIsArray "planeIndex" VkDisplayPlaneInfo2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneInfo2KHR, planeIndex}

instance {-# OVERLAPPING #-}
         CanReadField "planeIndex" VkDisplayPlaneInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneInfo2KHR, planeIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneInfo2KHR, planeIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "planeIndex" VkDisplayPlaneInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneInfo2KHR, planeIndex}

instance Show VkDisplayPlaneInfo2KHR where
        showsPrec d x
          = showString "VkDisplayPlaneInfo2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "mode = " .
                            showsPrec d (getField @"mode" x) .
                              showString ", " .
                                showString "planeIndex = " .
                                  showsPrec d (getField @"planeIndex" x) . showChar '}'

-- | > typedef struct VkDisplayPlaneProperties2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDisplayPlanePropertiesKHR displayPlaneProperties;
--   > } VkDisplayPlaneProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlaneProperties2KHR VkDisplayPlaneProperties2KHR registry at www.khronos.org>
data VkDisplayPlaneProperties2KHR = VkDisplayPlaneProperties2KHR## Addr##
                                                                  ByteArray##

instance Eq VkDisplayPlaneProperties2KHR where
        (VkDisplayPlaneProperties2KHR## a _) ==
          x@(VkDisplayPlaneProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlaneProperties2KHR where
        (VkDisplayPlaneProperties2KHR## a _) `compare`
          x@(VkDisplayPlaneProperties2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlaneProperties2KHR where
        sizeOf ~_ = #{size VkDisplayPlaneProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplayPlaneProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPlaneProperties2KHR where
        unsafeAddr (VkDisplayPlaneProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPlaneProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPlaneProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPlaneProperties2KHR where
        type StructFields VkDisplayPlaneProperties2KHR =
             '["sType", "pNext", "displayPlaneProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPlaneProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPlaneProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPlaneProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayPlaneProperties2KHR where
        type FieldType "sType" VkDisplayPlaneProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkDisplayPlaneProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayPlaneProperties2KHR =
             #{offset VkDisplayPlaneProperties2KHR, sType}
        type FieldIsArray "sType" VkDisplayPlaneProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayPlaneProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneProperties2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayPlaneProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayPlaneProperties2KHR where
        type FieldType "pNext" VkDisplayPlaneProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkDisplayPlaneProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayPlaneProperties2KHR =
             #{offset VkDisplayPlaneProperties2KHR, pNext}
        type FieldIsArray "pNext" VkDisplayPlaneProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayPlaneProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneProperties2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayPlaneProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "displayPlaneProperties" VkDisplayPlaneProperties2KHR
         where
        type FieldType "displayPlaneProperties"
               VkDisplayPlaneProperties2KHR
             = VkDisplayPlanePropertiesKHR
        type FieldOptional "displayPlaneProperties"
               VkDisplayPlaneProperties2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayPlaneProperties"
               VkDisplayPlaneProperties2KHR
             =
             #{offset VkDisplayPlaneProperties2KHR, displayPlaneProperties}
        type FieldIsArray "displayPlaneProperties"
               VkDisplayPlaneProperties2KHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneProperties2KHR, displayPlaneProperties}

instance {-# OVERLAPPING #-}
         CanReadField "displayPlaneProperties" VkDisplayPlaneProperties2KHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneProperties2KHR, displayPlaneProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneProperties2KHR, displayPlaneProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "displayPlaneProperties" VkDisplayPlaneProperties2KHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneProperties2KHR, displayPlaneProperties}

instance Show VkDisplayPlaneProperties2KHR where
        showsPrec d x
          = showString "VkDisplayPlaneProperties2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "displayPlaneProperties = " .
                            showsPrec d (getField @"displayPlaneProperties" x) . showChar '}'

-- | > typedef struct VkDisplayPlanePropertiesKHR {
--   >     VkDisplayKHR                     currentDisplay;
--   >     uint32_t                         currentStackIndex;
--   > } VkDisplayPlanePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPlanePropertiesKHR VkDisplayPlanePropertiesKHR registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "currentDisplay" VkDisplayPlanePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlanePropertiesKHR, currentDisplay})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

instance {-# OVERLAPPING #-}
         CanWriteField "currentDisplay" VkDisplayPlanePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

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

instance {-# OVERLAPPING #-}
         CanReadField "currentStackIndex" VkDisplayPlanePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlanePropertiesKHR, currentStackIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "currentStackIndex" VkDisplayPlanePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

instance Show VkDisplayPlanePropertiesKHR where
        showsPrec d x
          = showString "VkDisplayPlanePropertiesKHR {" .
              showString "currentDisplay = " .
                showsPrec d (getField @"currentDisplay" x) .
                  showString ", " .
                    showString "currentStackIndex = " .
                      showsPrec d (getField @"currentStackIndex" x) . showChar '}'

-- | > typedef struct VkDisplayPowerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayPowerStateEXT           powerState;
--   > } VkDisplayPowerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPowerInfoEXT VkDisplayPowerInfoEXT registry at www.khronos.org>
data VkDisplayPowerInfoEXT = VkDisplayPowerInfoEXT## Addr##
                                                    ByteArray##

instance Eq VkDisplayPowerInfoEXT where
        (VkDisplayPowerInfoEXT## a _) == x@(VkDisplayPowerInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPowerInfoEXT where
        (VkDisplayPowerInfoEXT## a _) `compare`
          x@(VkDisplayPowerInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPowerInfoEXT where
        sizeOf ~_ = #{size VkDisplayPowerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPowerInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPowerInfoEXT where
        unsafeAddr (VkDisplayPowerInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPowerInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPowerInfoEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPowerInfoEXT where
        type StructFields VkDisplayPowerInfoEXT =
             '["sType", "pNext", "powerState"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPowerInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDisplayPowerInfoEXT
         where
        type FieldType "sType" VkDisplayPowerInfoEXT = VkStructureType
        type FieldOptional "sType" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayPowerInfoEXT =
             #{offset VkDisplayPowerInfoEXT, sType}
        type FieldIsArray "sType" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPowerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayPowerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDisplayPowerInfoEXT
         where
        type FieldType "pNext" VkDisplayPowerInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayPowerInfoEXT =
             #{offset VkDisplayPowerInfoEXT, pNext}
        type FieldIsArray "pNext" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPowerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayPowerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "powerState" VkDisplayPowerInfoEXT where
        type FieldType "powerState" VkDisplayPowerInfoEXT =
             VkDisplayPowerStateEXT
        type FieldOptional "powerState" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "powerState" VkDisplayPowerInfoEXT =
             #{offset VkDisplayPowerInfoEXT, powerState}
        type FieldIsArray "powerState" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPowerInfoEXT, powerState}

instance {-# OVERLAPPING #-}
         CanReadField "powerState" VkDisplayPowerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, powerState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, powerState}

instance {-# OVERLAPPING #-}
         CanWriteField "powerState" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, powerState}

instance Show VkDisplayPowerInfoEXT where
        showsPrec d x
          = showString "VkDisplayPowerInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "powerState = " .
                            showsPrec d (getField @"powerState" x) . showChar '}'

-- | > typedef struct VkDisplayPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkRect2D                         srcRect;
--   >     VkRect2D                         dstRect;
--   >     VkBool32                         persistent;
--   > } VkDisplayPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPresentInfoKHR VkDisplayPresentInfoKHR registry at www.khronos.org>
data VkDisplayPresentInfoKHR = VkDisplayPresentInfoKHR## Addr##
                                                        ByteArray##

instance Eq VkDisplayPresentInfoKHR where
        (VkDisplayPresentInfoKHR## a _) == x@(VkDisplayPresentInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPresentInfoKHR where
        (VkDisplayPresentInfoKHR## a _) `compare`
          x@(VkDisplayPresentInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPresentInfoKHR where
        sizeOf ~_ = #{size VkDisplayPresentInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPresentInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPresentInfoKHR where
        unsafeAddr (VkDisplayPresentInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPresentInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPresentInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPresentInfoKHR where
        type StructFields VkDisplayPresentInfoKHR =
             '["sType", "pNext", "srcRect", "dstRect", "persistent"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPresentInfoKHR = '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayPresentInfoKHR where
        type FieldType "sType" VkDisplayPresentInfoKHR = VkStructureType
        type FieldOptional "sType" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, sType}
        type FieldIsArray "sType" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayPresentInfoKHR where
        type FieldType "pNext" VkDisplayPresentInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, pNext}
        type FieldIsArray "pNext" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcRect" VkDisplayPresentInfoKHR where
        type FieldType "srcRect" VkDisplayPresentInfoKHR = VkRect2D
        type FieldOptional "srcRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcRect" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, srcRect}
        type FieldIsArray "srcRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, srcRect}

instance {-# OVERLAPPING #-}
         CanReadField "srcRect" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, srcRect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, srcRect}

instance {-# OVERLAPPING #-}
         CanWriteField "srcRect" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, srcRect}

instance {-# OVERLAPPING #-}
         HasField "dstRect" VkDisplayPresentInfoKHR where
        type FieldType "dstRect" VkDisplayPresentInfoKHR = VkRect2D
        type FieldOptional "dstRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstRect" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, dstRect}
        type FieldIsArray "dstRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, dstRect}

instance {-# OVERLAPPING #-}
         CanReadField "dstRect" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, dstRect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, dstRect}

instance {-# OVERLAPPING #-}
         CanWriteField "dstRect" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, dstRect}

instance {-# OVERLAPPING #-}
         HasField "persistent" VkDisplayPresentInfoKHR where
        type FieldType "persistent" VkDisplayPresentInfoKHR = VkBool32
        type FieldOptional "persistent" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "persistent" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, persistent}
        type FieldIsArray "persistent" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, persistent}

instance {-# OVERLAPPING #-}
         CanReadField "persistent" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, persistent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, persistent}

instance {-# OVERLAPPING #-}
         CanWriteField "persistent" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, persistent}

instance Show VkDisplayPresentInfoKHR where
        showsPrec d x
          = showString "VkDisplayPresentInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcRect = " .
                            showsPrec d (getField @"srcRect" x) .
                              showString ", " .
                                showString "dstRect = " .
                                  showsPrec d (getField @"dstRect" x) .
                                    showString ", " .
                                      showString "persistent = " .
                                        showsPrec d (getField @"persistent" x) . showChar '}'

-- | > typedef struct VkDisplayProperties2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDisplayPropertiesKHR displayProperties;
--   > } VkDisplayProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayProperties2KHR VkDisplayProperties2KHR registry at www.khronos.org>
data VkDisplayProperties2KHR = VkDisplayProperties2KHR## Addr##
                                                        ByteArray##

instance Eq VkDisplayProperties2KHR where
        (VkDisplayProperties2KHR## a _) == x@(VkDisplayProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayProperties2KHR where
        (VkDisplayProperties2KHR## a _) `compare`
          x@(VkDisplayProperties2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayProperties2KHR where
        sizeOf ~_ = #{size VkDisplayProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayProperties2KHR where
        unsafeAddr (VkDisplayProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayProperties2KHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayProperties2KHR where
        type StructFields VkDisplayProperties2KHR =
             '["sType", "pNext", "displayProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayProperties2KHR where
        type FieldType "sType" VkDisplayProperties2KHR = VkStructureType
        type FieldOptional "sType" VkDisplayProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayProperties2KHR =
             #{offset VkDisplayProperties2KHR, sType}
        type FieldIsArray "sType" VkDisplayProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayProperties2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayProperties2KHR where
        type FieldType "pNext" VkDisplayProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkDisplayProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayProperties2KHR =
             #{offset VkDisplayProperties2KHR, pNext}
        type FieldIsArray "pNext" VkDisplayProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayProperties2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "displayProperties" VkDisplayProperties2KHR where
        type FieldType "displayProperties" VkDisplayProperties2KHR =
             VkDisplayPropertiesKHR
        type FieldOptional "displayProperties" VkDisplayProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "displayProperties" VkDisplayProperties2KHR =
             #{offset VkDisplayProperties2KHR, displayProperties}
        type FieldIsArray "displayProperties" VkDisplayProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayProperties2KHR, displayProperties}

instance {-# OVERLAPPING #-}
         CanReadField "displayProperties" VkDisplayProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayProperties2KHR, displayProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayProperties2KHR, displayProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "displayProperties" VkDisplayProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayProperties2KHR, displayProperties}

instance Show VkDisplayProperties2KHR where
        showsPrec d x
          = showString "VkDisplayProperties2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "displayProperties = " .
                            showsPrec d (getField @"displayProperties" x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayPropertiesKHR VkDisplayPropertiesKHR registry at www.khronos.org>
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

-- | > typedef struct VkDisplaySurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplaySurfaceCreateFlagsKHR   flags;
--   >     VkDisplayModeKHR                 displayMode;
--   >     uint32_t                         planeIndex;
--   >     uint32_t                         planeStackIndex;
--   >     VkSurfaceTransformFlagBitsKHR    transform;
--   >     float                            globalAlpha;
--   >     VkDisplayPlaneAlphaFlagBitsKHR   alphaMode;
--   >     VkExtent2D                       imageExtent;
--   > } VkDisplaySurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplaySurfaceCreateInfoKHR VkDisplaySurfaceCreateInfoKHR registry at www.khronos.org>
data VkDisplaySurfaceCreateInfoKHR = VkDisplaySurfaceCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkDisplaySurfaceCreateInfoKHR where
        (VkDisplaySurfaceCreateInfoKHR## a _) ==
          x@(VkDisplaySurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplaySurfaceCreateInfoKHR where
        (VkDisplaySurfaceCreateInfoKHR## a _) `compare`
          x@(VkDisplaySurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplaySurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkDisplaySurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplaySurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplaySurfaceCreateInfoKHR where
        unsafeAddr (VkDisplaySurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplaySurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplaySurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplaySurfaceCreateInfoKHR where
        type StructFields VkDisplaySurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "displayMode", "planeIndex", -- ' closing tick for hsc2hs
               "planeStackIndex", "transform", "globalAlpha", "alphaMode",
               "imageExtent"]
        type CUnionType VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplaySurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "sType" VkDisplaySurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "pNext" VkDisplaySurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "flags" VkDisplaySurfaceCreateInfoKHR =
             VkDisplaySurfaceCreateFlagsKHR
        type FieldOptional "flags" VkDisplaySurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "displayMode" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "displayMode" VkDisplaySurfaceCreateInfoKHR =
             VkDisplayModeKHR
        type FieldOptional "displayMode" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "displayMode" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}
        type FieldIsArray "displayMode" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

instance {-# OVERLAPPING #-}
         CanReadField "displayMode" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, displayMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

instance {-# OVERLAPPING #-}
         CanWriteField "displayMode" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

instance {-# OVERLAPPING #-}
         HasField "planeIndex" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "planeIndex" VkDisplaySurfaceCreateInfoKHR = Word32
        type FieldOptional "planeIndex" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "planeIndex" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}
        type FieldIsArray "planeIndex" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

instance {-# OVERLAPPING #-}
         CanReadField "planeIndex" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "planeIndex" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

instance {-# OVERLAPPING #-}
         HasField "planeStackIndex" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "planeStackIndex" VkDisplaySurfaceCreateInfoKHR =
             Word32
        type FieldOptional "planeStackIndex" VkDisplaySurfaceCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "planeStackIndex" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}
        type FieldIsArray "planeStackIndex" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

instance {-# OVERLAPPING #-}
         CanReadField "planeStackIndex" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "planeStackIndex" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

instance {-# OVERLAPPING #-}
         HasField "transform" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "transform" VkDisplaySurfaceCreateInfoKHR =
             VkSurfaceTransformFlagBitsKHR
        type FieldOptional "transform" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "transform" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, transform}
        type FieldIsArray "transform" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, transform}

instance {-# OVERLAPPING #-}
         CanReadField "transform" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, transform})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, transform}

instance {-# OVERLAPPING #-}
         CanWriteField "transform" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, transform}

instance {-# OVERLAPPING #-}
         HasField "globalAlpha" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "globalAlpha" VkDisplaySurfaceCreateInfoKHR =
             #{type float}
        type FieldOptional "globalAlpha" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "globalAlpha" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}
        type FieldIsArray "globalAlpha" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

instance {-# OVERLAPPING #-}
         CanReadField "globalAlpha" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

instance {-# OVERLAPPING #-}
         CanWriteField "globalAlpha" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

instance {-# OVERLAPPING #-}
         HasField "alphaMode" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "alphaMode" VkDisplaySurfaceCreateInfoKHR =
             VkDisplayPlaneAlphaFlagBitsKHR
        type FieldOptional "alphaMode" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaMode" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}
        type FieldIsArray "alphaMode" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

instance {-# OVERLAPPING #-}
         CanReadField "alphaMode" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaMode" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

instance {-# OVERLAPPING #-}
         HasField "imageExtent" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "imageExtent" VkDisplaySurfaceCreateInfoKHR =
             VkExtent2D
        type FieldOptional "imageExtent" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "imageExtent" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}
        type FieldIsArray "imageExtent" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "imageExtent" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "imageExtent" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

instance Show VkDisplaySurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkDisplaySurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "displayMode = " .
                                  showsPrec d (getField @"displayMode" x) .
                                    showString ", " .
                                      showString "planeIndex = " .
                                        showsPrec d (getField @"planeIndex" x) .
                                          showString ", " .
                                            showString "planeStackIndex = " .
                                              showsPrec d (getField @"planeStackIndex" x) .
                                                showString ", " .
                                                  showString "transform = " .
                                                    showsPrec d (getField @"transform" x) .
                                                      showString ", " .
                                                        showString "globalAlpha = " .
                                                          showsPrec d (getField @"globalAlpha" x) .
                                                            showString ", " .
                                                              showString "alphaMode = " .
                                                                showsPrec d
                                                                  (getField @"alphaMode" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "imageExtent = " .
                                                                      showsPrec d
                                                                        (getField @"imageExtent" x)
                                                                        . showChar '}'
