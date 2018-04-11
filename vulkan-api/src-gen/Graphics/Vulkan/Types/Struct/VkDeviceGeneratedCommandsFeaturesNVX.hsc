#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGeneratedCommandsFeaturesNVX
       (VkDeviceGeneratedCommandsFeaturesNVX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGeneratedCommandsFeaturesNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         computeBindingPointSupport;
--   > } VkDeviceGeneratedCommandsFeaturesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGeneratedCommandsFeaturesNVX VkDeviceGeneratedCommandsFeaturesNVX registry at www.khronos.org>
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX## Addr##
                                                                                  ByteArray##

instance Eq VkDeviceGeneratedCommandsFeaturesNVX where
        (VkDeviceGeneratedCommandsFeaturesNVX## a _) ==
          x@(VkDeviceGeneratedCommandsFeaturesNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGeneratedCommandsFeaturesNVX where
        (VkDeviceGeneratedCommandsFeaturesNVX## a _) `compare`
          x@(VkDeviceGeneratedCommandsFeaturesNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGeneratedCommandsFeaturesNVX where
        sizeOf ~_
          = #{size VkDeviceGeneratedCommandsFeaturesNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGeneratedCommandsFeaturesNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGeneratedCommandsFeaturesNVX
         where
        unsafeAddr (VkDeviceGeneratedCommandsFeaturesNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGeneratedCommandsFeaturesNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGeneratedCommandsFeaturesNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGeneratedCommandsFeaturesNVX where
        type StructFields VkDeviceGeneratedCommandsFeaturesNVX =
             '["sType", "pNext", "computeBindingPointSupport"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGeneratedCommandsFeaturesNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGeneratedCommandsFeaturesNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGeneratedCommandsFeaturesNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGeneratedCommandsFeaturesNVX where
        type FieldType "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}
        type FieldIsArray "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGeneratedCommandsFeaturesNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGeneratedCommandsFeaturesNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGeneratedCommandsFeaturesNVX where
        type FieldType "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}
        type FieldIsArray "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGeneratedCommandsFeaturesNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGeneratedCommandsFeaturesNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        type FieldType "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = VkBool32
        type FieldOptional "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}
        type FieldIsArray "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance {-# OVERLAPPING #-}
         CanReadField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance {-# OVERLAPPING #-}
         CanWriteField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance Show VkDeviceGeneratedCommandsFeaturesNVX where
        showsPrec d x
          = showString "VkDeviceGeneratedCommandsFeaturesNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "computeBindingPointSupport = " .
                            showsPrec d (getField @"computeBindingPointSupport" x) .
                              showChar '}'
