#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayModeCreateInfoKHR
       (VkDisplayModeCreateInfoKHR(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                          (VkDisplayModeCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType              (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDisplayModeParametersKHR (VkDisplayModeParametersKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayModeCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayModeCreateFlagsKHR      flags;
--   >     VkDisplayModeParametersKHR       parameters;
--   > } VkDisplayModeCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayModeCreateInfoKHR.html VkDisplayModeCreateInfoKHR registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkDisplayModeCreateInfoKHR
         where
        type VkSTypeMType VkDisplayModeCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, sType}

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

instance CanReadField "sType" VkDisplayModeCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDisplayModeCreateInfoKHR
         where
        type VkPNextMType VkDisplayModeCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, pNext}

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

instance CanReadField "pNext" VkDisplayModeCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkDisplayModeCreateInfoKHR
         where
        type VkFlagsMType VkDisplayModeCreateInfoKHR =
             VkDisplayModeCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, flags}

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

instance CanReadField "flags" VkDisplayModeCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkParameters VkDisplayModeCreateInfoKHR where
        type VkParametersMType VkDisplayModeCreateInfoKHR =
             VkDisplayModeParametersKHR

        {-# NOINLINE vkParameters #-}
        vkParameters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, parameters})

        {-# INLINE vkParametersByteOffset #-}
        vkParametersByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, parameters}

        {-# INLINE readVkParameters #-}
        readVkParameters p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, parameters}

        {-# INLINE writeVkParameters #-}
        writeVkParameters p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, parameters}

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

instance CanReadField "parameters" VkDisplayModeCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkParameters

        {-# INLINE readField #-}
        readField = readVkParameters

instance CanWriteField "parameters" VkDisplayModeCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkParameters

instance Show VkDisplayModeCreateInfoKHR where
        showsPrec d x
          = showString "VkDisplayModeCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkParameters = " .
                                  showsPrec d (vkParameters x) . showChar '}'
