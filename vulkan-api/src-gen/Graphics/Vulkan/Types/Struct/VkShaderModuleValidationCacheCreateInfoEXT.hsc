#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

instance {-# OVERLAPPING #-}
         CanWriteField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

instance Show VkShaderModuleValidationCacheCreateInfoEXT where
        showsPrec d x
          = showString "VkShaderModuleValidationCacheCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "validationCache = " .
                            showsPrec d (getField @"validationCache" x) . showChar '}'
