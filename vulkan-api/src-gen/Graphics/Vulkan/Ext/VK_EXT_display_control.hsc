#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_display_control
       (-- * Vulkan extension: @VK_EXT_display_control@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @92@
        --
        -- Required extensions: 'VK_EXT_display_surface_counter', 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_EXT_display_surface_counter', 'VK_KHR_swapchain'.
        VkDisplayPowerInfoEXT(..), VkDeviceEventInfoEXT(..),
        VkDisplayEventInfoEXT(..), VkSwapchainCounterCreateInfoEXT(..),
        vkDisplayPowerControlEXT, vkRegisterDeviceEventEXT,
        vkRegisterDisplayEventEXT, vkGetSwapchainCounterEXT,
        VK_EXT_DISPLAY_CONTROL_SPEC_VERSION,
        pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION,
        VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME,
        pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT)
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..),
                                                   VkSwapchainCreateInfoKHR)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayPowerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayPowerStateEXT           powerState;
--   > } VkDisplayPowerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayPowerInfoEXT.html VkDisplayPowerInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkDisplayPowerInfoEXT where
        type VkSTypeMType VkDisplayPowerInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDisplayPowerInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, sType}

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

instance CanReadField "sType" VkDisplayPowerInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDisplayPowerInfoEXT where
        type VkPNextMType VkDisplayPowerInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDisplayPowerInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, pNext}

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

instance CanReadField "pNext" VkDisplayPowerInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkPowerState VkDisplayPowerInfoEXT
         where
        type VkPowerStateMType VkDisplayPowerInfoEXT =
             VkDisplayPowerStateEXT

        {-# NOINLINE vkPowerState #-}
        vkPowerState x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, powerState})

        {-# INLINE vkPowerStateByteOffset #-}
        vkPowerStateByteOffset ~_
          = #{offset VkDisplayPowerInfoEXT, powerState}

        {-# INLINE readVkPowerState #-}
        readVkPowerState p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, powerState}

        {-# INLINE writeVkPowerState #-}
        writeVkPowerState p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, powerState}

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

instance CanReadField "powerState" VkDisplayPowerInfoEXT where
        {-# INLINE getField #-}
        getField = vkPowerState

        {-# INLINE readField #-}
        readField = readVkPowerState

instance CanWriteField "powerState" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPowerState

instance Show VkDisplayPowerInfoEXT where
        showsPrec d x
          = showString "VkDisplayPowerInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPowerState = " .
                            showsPrec d (vkPowerState x) . showChar '}'

-- | > typedef struct VkDeviceEventInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceEventTypeEXT             deviceEvent;
--   > } VkDeviceEventInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceEventInfoEXT.html VkDeviceEventInfoEXT registry at www.khronos.org>
data VkDeviceEventInfoEXT = VkDeviceEventInfoEXT## Addr## ByteArray##

instance Eq VkDeviceEventInfoEXT where
        (VkDeviceEventInfoEXT## a _) == x@(VkDeviceEventInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceEventInfoEXT where
        (VkDeviceEventInfoEXT## a _) `compare` x@(VkDeviceEventInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceEventInfoEXT where
        sizeOf ~_ = #{size VkDeviceEventInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceEventInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceEventInfoEXT where
        unsafeAddr (VkDeviceEventInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceEventInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceEventInfoEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceEventInfoEXT where
        type StructFields VkDeviceEventInfoEXT =
             '["sType", "pNext", "deviceEvent"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceEventInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkDeviceEventInfoEXT where
        type VkSTypeMType VkDeviceEventInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceEventInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkDeviceEventInfoEXT
         where
        type FieldType "sType" VkDeviceEventInfoEXT = VkStructureType
        type FieldOptional "sType" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, sType}
        type FieldIsArray "sType" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceEventInfoEXT, sType}

instance CanReadField "sType" VkDeviceEventInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDeviceEventInfoEXT where
        type VkPNextMType VkDeviceEventInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceEventInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkDeviceEventInfoEXT
         where
        type FieldType "pNext" VkDeviceEventInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, pNext}
        type FieldIsArray "pNext" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceEventInfoEXT, pNext}

instance CanReadField "pNext" VkDeviceEventInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkDeviceEvent VkDeviceEventInfoEXT
         where
        type VkDeviceEventMType VkDeviceEventInfoEXT = VkDeviceEventTypeEXT

        {-# NOINLINE vkDeviceEvent #-}
        vkDeviceEvent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, deviceEvent})

        {-# INLINE vkDeviceEventByteOffset #-}
        vkDeviceEventByteOffset ~_
          = #{offset VkDeviceEventInfoEXT, deviceEvent}

        {-# INLINE readVkDeviceEvent #-}
        readVkDeviceEvent p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, deviceEvent}

        {-# INLINE writeVkDeviceEvent #-}
        writeVkDeviceEvent p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, deviceEvent}

instance {-# OVERLAPPING #-}
         HasField "deviceEvent" VkDeviceEventInfoEXT where
        type FieldType "deviceEvent" VkDeviceEventInfoEXT =
             VkDeviceEventTypeEXT
        type FieldOptional "deviceEvent" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceEvent" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, deviceEvent}
        type FieldIsArray "deviceEvent" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceEventInfoEXT, deviceEvent}

instance CanReadField "deviceEvent" VkDeviceEventInfoEXT where
        {-# INLINE getField #-}
        getField = vkDeviceEvent

        {-# INLINE readField #-}
        readField = readVkDeviceEvent

instance CanWriteField "deviceEvent" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceEvent

instance Show VkDeviceEventInfoEXT where
        showsPrec d x
          = showString "VkDeviceEventInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceEvent = " .
                            showsPrec d (vkDeviceEvent x) . showChar '}'

-- | > typedef struct VkDisplayEventInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayEventTypeEXT            displayEvent;
--   > } VkDisplayEventInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayEventInfoEXT.html VkDisplayEventInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkDisplayEventInfoEXT where
        type VkSTypeMType VkDisplayEventInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDisplayEventInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, sType}

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

instance CanReadField "sType" VkDisplayEventInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDisplayEventInfoEXT where
        type VkPNextMType VkDisplayEventInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDisplayEventInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, pNext}

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

instance CanReadField "pNext" VkDisplayEventInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDisplayEvent VkDisplayEventInfoEXT where
        type VkDisplayEventMType VkDisplayEventInfoEXT =
             VkDisplayEventTypeEXT

        {-# NOINLINE vkDisplayEvent #-}
        vkDisplayEvent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, displayEvent})

        {-# INLINE vkDisplayEventByteOffset #-}
        vkDisplayEventByteOffset ~_
          = #{offset VkDisplayEventInfoEXT, displayEvent}

        {-# INLINE readVkDisplayEvent #-}
        readVkDisplayEvent p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, displayEvent}

        {-# INLINE writeVkDisplayEvent #-}
        writeVkDisplayEvent p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, displayEvent}

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

instance CanReadField "displayEvent" VkDisplayEventInfoEXT where
        {-# INLINE getField #-}
        getField = vkDisplayEvent

        {-# INLINE readField #-}
        readField = readVkDisplayEvent

instance CanWriteField "displayEvent" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkDisplayEvent

instance Show VkDisplayEventInfoEXT where
        showsPrec d x
          = showString "VkDisplayEventInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDisplayEvent = " .
                            showsPrec d (vkDisplayEvent x) . showChar '}'

-- | > typedef struct VkSwapchainCounterCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSurfaceCounterFlagsEXT         surfaceCounters;
--   > } VkSwapchainCounterCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSwapchainCounterCreateInfoEXT.html VkSwapchainCounterCreateInfoEXT registry at www.khronos.org>
data VkSwapchainCounterCreateInfoEXT = VkSwapchainCounterCreateInfoEXT## Addr##
                                                                        ByteArray##

instance Eq VkSwapchainCounterCreateInfoEXT where
        (VkSwapchainCounterCreateInfoEXT## a _) ==
          x@(VkSwapchainCounterCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSwapchainCounterCreateInfoEXT where
        (VkSwapchainCounterCreateInfoEXT## a _) `compare`
          x@(VkSwapchainCounterCreateInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSwapchainCounterCreateInfoEXT where
        sizeOf ~_ = #{size VkSwapchainCounterCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSwapchainCounterCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSwapchainCounterCreateInfoEXT where
        unsafeAddr (VkSwapchainCounterCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSwapchainCounterCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSwapchainCounterCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSwapchainCounterCreateInfoEXT where
        type StructFields VkSwapchainCounterCreateInfoEXT =
             '["sType", "pNext", "surfaceCounters"] -- ' closing tick for hsc2hs
        type CUnionType VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSwapchainCounterCreateInfoEXT =
             '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkSwapchainCounterCreateInfoEXT where
        type VkSTypeMType VkSwapchainCounterCreateInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSwapchainCounterCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSwapchainCounterCreateInfoEXT where
        type FieldType "sType" VkSwapchainCounterCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSwapchainCounterCreateInfoEXT =
             #{offset VkSwapchainCounterCreateInfoEXT, sType}
        type FieldIsArray "sType" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, sType}

instance CanReadField "sType" VkSwapchainCounterCreateInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSwapchainCounterCreateInfoEXT where
        type VkPNextMType VkSwapchainCounterCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSwapchainCounterCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSwapchainCounterCreateInfoEXT where
        type FieldType "pNext" VkSwapchainCounterCreateInfoEXT = Ptr Void
        type FieldOptional "pNext" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSwapchainCounterCreateInfoEXT =
             #{offset VkSwapchainCounterCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, pNext}

instance CanReadField "pNext" VkSwapchainCounterCreateInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSurfaceCounters VkSwapchainCounterCreateInfoEXT where
        type VkSurfaceCountersMType VkSwapchainCounterCreateInfoEXT =
             VkSurfaceCounterFlagsEXT

        {-# NOINLINE vkSurfaceCounters #-}
        vkSurfaceCounters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters})

        {-# INLINE vkSurfaceCountersByteOffset #-}
        vkSurfaceCountersByteOffset ~_
          = #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

        {-# INLINE readVkSurfaceCounters #-}
        readVkSurfaceCounters p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

        {-# INLINE writeVkSurfaceCounters #-}
        writeVkSurfaceCounters p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance {-# OVERLAPPING #-}
         HasField "surfaceCounters" VkSwapchainCounterCreateInfoEXT where
        type FieldType "surfaceCounters" VkSwapchainCounterCreateInfoEXT =
             VkSurfaceCounterFlagsEXT
        type FieldOptional "surfaceCounters"
               VkSwapchainCounterCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "surfaceCounters" VkSwapchainCounterCreateInfoEXT
             =
             #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}
        type FieldIsArray "surfaceCounters" VkSwapchainCounterCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance CanReadField "surfaceCounters"
           VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSurfaceCounters

        {-# INLINE readField #-}
        readField = readVkSurfaceCounters

instance CanWriteField "surfaceCounters"
           VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSurfaceCounters

instance Show VkSwapchainCounterCreateInfoEXT where
        showsPrec d x
          = showString "VkSwapchainCounterCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurfaceCounters = " .
                            showsPrec d (vkSurfaceCounters x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkDisplayPowerControlEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayPowerInfoEXT* pDisplayPowerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDisplayPowerControlEXT.html vkDisplayPowerControlEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDisplayPowerControlEXT"
               vkDisplayPowerControlEXT ::
               VkDevice -- ^ device
                        ->
                 VkDisplayKHR -- ^ display
                              -> Ptr VkDisplayPowerInfoEXT -- ^ pDisplayPowerInfo
                                                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDeviceEventEXT
--   >     ( VkDevice device
--   >     , const VkDeviceEventInfoEXT* pDeviceEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkRegisterDeviceEventEXT.html vkRegisterDeviceEventEXT registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterDeviceEventEXT"
               vkRegisterDeviceEventEXT ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDeviceEventInfoEXT -- ^ pDeviceEventInfo
                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFence -- ^ pFence
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDisplayEventEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayEventInfoEXT* pDisplayEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkRegisterDisplayEventEXT.html vkRegisterDisplayEventEXT registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterDisplayEventEXT"
               vkRegisterDisplayEventEXT ::
               VkDevice -- ^ device
                        ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr VkDisplayEventInfoEXT -- ^ pDisplayEventInfo
                                             ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkFence -- ^ pFence
                                                              -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR'.
--
--   > VkResult vkGetSwapchainCounterEXT
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkSurfaceCounterFlagBitsEXT counter
--   >     , uint64_t* pCounterValue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetSwapchainCounterEXT.html vkGetSwapchainCounterEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainCounterEXT"
               vkGetSwapchainCounterEXT ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   VkSurfaceCounterFlagBitsEXT -- ^ counter
                                               -> Ptr Word64 -- ^ pCounterValue
                                                             -> IO VkResult

pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

type VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString

pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME <-
        (is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME -> True)
  where VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
          = _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME

{-# INLINE _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME #-}

_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString
_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  = Ptr "VK_EXT_display_control\NUL"##

{-# INLINE is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME #-}

is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  = eqCStrings _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME

type VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME =
     "VK_EXT_display_control"

pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT =
        VkStructureType 1000091000

pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT =
        VkStructureType 1000091001

pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT =
        VkStructureType 1000091002

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT =
        VkStructureType 1000091003
