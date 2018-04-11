#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
       (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         perViewPositionAllComponents;
--   > } VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX registry at www.khronos.org>
data VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## Addr##
                                                                                                                        ByteArray##

instance Eq VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _) ==
          x@(VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _)
          `compare`
          x@(VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        unsafeAddr
          (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type StructFields
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = '["sType", "pNext", "perViewPositionAllComponents"] -- ' closing tick for hsc2hs
        type CUnionType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'True -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "perViewPositionAllComponents"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkBool32
        type FieldOptional "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}
        type FieldIsArray "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

instance {-# OVERLAPPING #-}
         CanReadField "perViewPositionAllComponents"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "perViewPositionAllComponents"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

instance Show
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        showsPrec d x
          = showString
              "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "perViewPositionAllComponents = " .
                            showsPrec d (getField @"perViewPositionAllComponents" x) .
                              showChar '}'
