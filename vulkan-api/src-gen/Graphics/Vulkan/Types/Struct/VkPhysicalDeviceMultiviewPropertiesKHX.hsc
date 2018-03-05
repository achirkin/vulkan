#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewPropertiesKHX
       (VkPhysicalDeviceMultiviewPropertiesKHX(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMultiviewPropertiesKHX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxMultiviewViewCount;
--   >     uint32_t                         maxMultiviewInstanceIndex;
--   > } VkPhysicalDeviceMultiviewPropertiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceMultiviewPropertiesKHX.html VkPhysicalDeviceMultiviewPropertiesKHX registry at www.khronos.org>
data VkPhysicalDeviceMultiviewPropertiesKHX = VkPhysicalDeviceMultiviewPropertiesKHX## Addr##
                                                                                      ByteArray##

instance Eq VkPhysicalDeviceMultiviewPropertiesKHX where
        (VkPhysicalDeviceMultiviewPropertiesKHX## a _) ==
          x@(VkPhysicalDeviceMultiviewPropertiesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewPropertiesKHX where
        (VkPhysicalDeviceMultiviewPropertiesKHX## a _) `compare`
          x@(VkPhysicalDeviceMultiviewPropertiesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewPropertiesKHX where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewPropertiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewPropertiesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMultiviewPropertiesKHX
         where
        unsafeAddr (VkPhysicalDeviceMultiviewPropertiesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMultiviewPropertiesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewPropertiesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewPropertiesKHX where
        type StructFields VkPhysicalDeviceMultiviewPropertiesKHX =
             '["sType", "pNext", "maxMultiviewViewCount", -- ' closing tick for hsc2hs
               "maxMultiviewInstanceIndex"]
        type CUnionType VkPhysicalDeviceMultiviewPropertiesKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMultiviewPropertiesKHX = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMultiviewPropertiesKHX =
             '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMultiviewPropertiesKHX where
        type FieldType "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}
        type FieldIsArray "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceMultiviewPropertiesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceMultiviewPropertiesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMultiviewPropertiesKHX where
        type FieldType "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceMultiviewPropertiesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceMultiviewPropertiesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type FieldType "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32
        type FieldOptional "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}
        type FieldIsArray "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

instance {-# OVERLAPPING #-}
         HasField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type FieldType "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32
        type FieldOptional "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}
        type FieldIsArray "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

instance {-# OVERLAPPING #-}
         CanReadField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

instance Show VkPhysicalDeviceMultiviewPropertiesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceMultiviewPropertiesKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxMultiviewViewCount = " .
                            showsPrec d (getField @"maxMultiviewViewCount" x) .
                              showString ", " .
                                showString "maxMultiviewInstanceIndex = " .
                                  showsPrec d (getField @"maxMultiviewInstanceIndex" x) .
                                    showChar '}'
