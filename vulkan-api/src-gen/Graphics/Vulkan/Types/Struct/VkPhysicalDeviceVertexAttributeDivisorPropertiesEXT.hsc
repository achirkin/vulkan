#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
       (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxVertexAttribDivisor;
--   > } VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT.html VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT## Addr##
                                                                                                                ByteArray##

instance Eq VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT## a _)
          `compare`
          x@(VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        unsafeAddr
          (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        type StructFields
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = '["sType", "pNext", "maxVertexAttribDivisor"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             =
             #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             =
             #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxVertexAttribDivisor"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        type FieldType "maxVertexAttribDivisor"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = Word32
        type FieldOptional "maxVertexAttribDivisor"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVertexAttribDivisor"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             =
             #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, maxVertexAttribDivisor}
        type FieldIsArray "maxVertexAttribDivisor"
               VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, maxVertexAttribDivisor}

instance {-# OVERLAPPING #-}
         CanReadField "maxVertexAttribDivisor"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, maxVertexAttribDivisor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, maxVertexAttribDivisor}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVertexAttribDivisor"
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, maxVertexAttribDivisor}

instance Show VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
        showsPrec d x
          = showString
              "VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxVertexAttribDivisor = " .
                            showsPrec d (getField @"maxVertexAttribDivisor" x) . showChar '}'
