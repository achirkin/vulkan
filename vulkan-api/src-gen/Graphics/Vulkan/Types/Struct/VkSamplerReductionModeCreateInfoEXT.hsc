#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerReductionModeCreateInfoEXT
       (VkSamplerReductionModeCreateInfoEXT(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkSamplerReductionModeEXT (VkSamplerReductionModeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo     (VkSamplerCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerReductionModeCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSamplerReductionModeEXT reductionMode;
--   > } VkSamplerReductionModeCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSamplerReductionModeCreateInfoEXT.html VkSamplerReductionModeCreateInfoEXT registry at www.khronos.org>
data VkSamplerReductionModeCreateInfoEXT = VkSamplerReductionModeCreateInfoEXT## Addr##
                                                                                ByteArray##

instance Eq VkSamplerReductionModeCreateInfoEXT where
        (VkSamplerReductionModeCreateInfoEXT## a _) ==
          x@(VkSamplerReductionModeCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerReductionModeCreateInfoEXT where
        (VkSamplerReductionModeCreateInfoEXT## a _) `compare`
          x@(VkSamplerReductionModeCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerReductionModeCreateInfoEXT where
        sizeOf ~_ = #{size VkSamplerReductionModeCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerReductionModeCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerReductionModeCreateInfoEXT
         where
        unsafeAddr (VkSamplerReductionModeCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerReductionModeCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerReductionModeCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerReductionModeCreateInfoEXT where
        type StructFields VkSamplerReductionModeCreateInfoEXT =
             '["sType", "pNext", "reductionMode"] -- ' closing tick for hsc2hs
        type CUnionType VkSamplerReductionModeCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerReductionModeCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerReductionModeCreateInfoEXT =
             '[VkSamplerCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkSamplerReductionModeCreateInfoEXT where
        type VkSTypeMType VkSamplerReductionModeCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSamplerReductionModeCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "sType" VkSamplerReductionModeCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerReductionModeCreateInfoEXT =
             #{offset VkSamplerReductionModeCreateInfoEXT, sType}
        type FieldIsArray "sType" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, sType}

instance CanReadField "sType" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSamplerReductionModeCreateInfoEXT where
        type VkPNextMType VkSamplerReductionModeCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "pNext" VkSamplerReductionModeCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerReductionModeCreateInfoEXT =
             #{offset VkSamplerReductionModeCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

instance CanReadField "pNext" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkReductionMode VkSamplerReductionModeCreateInfoEXT where
        type VkReductionModeMType VkSamplerReductionModeCreateInfoEXT =
             VkSamplerReductionModeEXT

        {-# NOINLINE vkReductionMode #-}
        vkReductionMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode})

        {-# INLINE vkReductionModeByteOffset #-}
        vkReductionModeByteOffset ~_
          = #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

        {-# INLINE readVkReductionMode #-}
        readVkReductionMode p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

        {-# INLINE writeVkReductionMode #-}
        writeVkReductionMode p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

instance {-# OVERLAPPING #-}
         HasField "reductionMode" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "reductionMode" VkSamplerReductionModeCreateInfoEXT
             = VkSamplerReductionModeEXT
        type FieldOptional "reductionMode"
               VkSamplerReductionModeCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "reductionMode"
               VkSamplerReductionModeCreateInfoEXT
             =
             #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}
        type FieldIsArray "reductionMode"
               VkSamplerReductionModeCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

instance CanReadField "reductionMode"
           VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkReductionMode

        {-# INLINE readField #-}
        readField = readVkReductionMode

instance CanWriteField "reductionMode"
           VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkReductionMode

instance Show VkSamplerReductionModeCreateInfoEXT where
        showsPrec d x
          = showString "VkSamplerReductionModeCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkReductionMode = " .
                            showsPrec d (vkReductionMode x) . showChar '}'
