#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkShaderModuleValidationCacheCreateInfoEXT
       (VkShaderModuleValidationCacheCreateInfoEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Handles                         (VkValidationCacheEXT)
import           Graphics.Vulkan.Types.Struct.VkShaderModuleCreateInfo (VkShaderModuleCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkShaderModuleValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheEXT    validationCache;
--   > } VkShaderModuleValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkShaderModuleValidationCacheCreateInfoEXT.html VkShaderModuleValidationCacheCreateInfoEXT registry at www.khronos.org>
data VkShaderModuleValidationCacheCreateInfoEXT = VkShaderModuleValidationCacheCreateInfoEXT## Addr##
                                                                                              ByteArray##

instance Eq VkShaderModuleValidationCacheCreateInfoEXT where
        (VkShaderModuleValidationCacheCreateInfoEXT## a _) ==
          x@(VkShaderModuleValidationCacheCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderModuleValidationCacheCreateInfoEXT where
        (VkShaderModuleValidationCacheCreateInfoEXT## a _) `compare`
          x@(VkShaderModuleValidationCacheCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderModuleValidationCacheCreateInfoEXT where
        sizeOf ~_
          = #{size VkShaderModuleValidationCacheCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkShaderModuleValidationCacheCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        unsafeAddr (VkShaderModuleValidationCacheCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderModuleValidationCacheCreateInfoEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderModuleValidationCacheCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderModuleValidationCacheCreateInfoEXT
         where
        type StructFields VkShaderModuleValidationCacheCreateInfoEXT =
             '["sType", "pNext", "validationCache"] -- ' closing tick for hsc2hs
        type CUnionType VkShaderModuleValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderModuleValidationCacheCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkShaderModuleValidationCacheCreateInfoEXT =
             '[VkShaderModuleCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkShaderModuleValidationCacheCreateInfoEXT where
        type VkSTypeMType VkShaderModuleValidationCacheCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkShaderModuleValidationCacheCreateInfoEXT where
        type FieldType "sType" VkShaderModuleValidationCacheCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkShaderModuleValidationCacheCreateInfoEXT
             =
             #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

instance CanReadField "sType"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkShaderModuleValidationCacheCreateInfoEXT where
        type VkPNextMType VkShaderModuleValidationCacheCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkShaderModuleValidationCacheCreateInfoEXT where
        type FieldType "pNext" VkShaderModuleValidationCacheCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkShaderModuleValidationCacheCreateInfoEXT
             =
             #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkValidationCache VkShaderModuleValidationCacheCreateInfoEXT
         where
        type VkValidationCacheMType
               VkShaderModuleValidationCacheCreateInfoEXT
             = VkValidationCacheEXT

        {-# NOINLINE vkValidationCache #-}
        vkValidationCache x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache})

        {-# INLINE vkValidationCacheByteOffset #-}
        vkValidationCacheByteOffset ~_
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

        {-# INLINE readVkValidationCache #-}
        readVkValidationCache p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

        {-# INLINE writeVkValidationCache #-}
        writeVkValidationCache p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

instance {-# OVERLAPPING #-}
         HasField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        type FieldType "validationCache"
               VkShaderModuleValidationCacheCreateInfoEXT
             = VkValidationCacheEXT
        type FieldOptional "validationCache"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "validationCache"
               VkShaderModuleValidationCacheCreateInfoEXT
             =
             #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}
        type FieldIsArray "validationCache"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

instance CanReadField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkValidationCache

        {-# INLINE readField #-}
        readField = readVkValidationCache

instance CanWriteField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkValidationCache

instance Show VkShaderModuleValidationCacheCreateInfoEXT where
        showsPrec d x
          = showString "VkShaderModuleValidationCacheCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkValidationCache = " .
                            showsPrec d (vkValidationCache x) . showChar '}'
