#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceEventInfoEXT
       (VkDeviceEventInfoEXT(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceEventTypeEXT (VkDeviceEventTypeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

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
