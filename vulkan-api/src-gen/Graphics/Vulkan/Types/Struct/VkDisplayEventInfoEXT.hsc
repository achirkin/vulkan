#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayEventInfoEXT
       (VkDisplayEventInfoEXT(..)) where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDisplayEventTypeEXT (VkDisplayEventTypeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType       (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                 (unsafeDupablePerformIO)

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
