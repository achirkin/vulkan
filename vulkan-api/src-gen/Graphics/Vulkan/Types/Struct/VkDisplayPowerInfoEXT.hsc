#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayPowerInfoEXT
       (VkDisplayPowerInfoEXT(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDisplayPowerStateEXT (VkDisplayPowerStateEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

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
