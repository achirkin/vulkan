#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2
       (VkPhysicalDeviceFeatures2(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo       (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceFeatures2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceFeatures         features;
--   > } VkPhysicalDeviceFeatures2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceFeatures2.html VkPhysicalDeviceFeatures2 registry at www.khronos.org>
data VkPhysicalDeviceFeatures2 = VkPhysicalDeviceFeatures2## Addr##
                                                            ByteArray##

instance Eq VkPhysicalDeviceFeatures2 where
        (VkPhysicalDeviceFeatures2## a _) ==
          x@(VkPhysicalDeviceFeatures2## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceFeatures2 where
        (VkPhysicalDeviceFeatures2## a _) `compare`
          x@(VkPhysicalDeviceFeatures2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceFeatures2 where
        sizeOf ~_ = #{size VkPhysicalDeviceFeatures2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPhysicalDeviceFeatures2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceFeatures2 where
        unsafeAddr (VkPhysicalDeviceFeatures2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceFeatures2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceFeatures2## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceFeatures2 where
        type StructFields VkPhysicalDeviceFeatures2 =
             '["sType", "pNext", "features"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceFeatures2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceFeatures2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceFeatures2 =
             '[VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceFeatures2 where
        type FieldType "sType" VkPhysicalDeviceFeatures2 = VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceFeatures2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceFeatures2 =
             #{offset VkPhysicalDeviceFeatures2, sType}
        type FieldIsArray "sType" VkPhysicalDeviceFeatures2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceFeatures2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceFeatures2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceFeatures2 where
        type FieldType "pNext" VkPhysicalDeviceFeatures2 = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceFeatures2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceFeatures2 =
             #{offset VkPhysicalDeviceFeatures2, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceFeatures2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceFeatures2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceFeatures2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2, pNext}

instance {-# OVERLAPPING #-}
         HasField "features" VkPhysicalDeviceFeatures2 where
        type FieldType "features" VkPhysicalDeviceFeatures2 =
             VkPhysicalDeviceFeatures
        type FieldOptional "features" VkPhysicalDeviceFeatures2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "features" VkPhysicalDeviceFeatures2 =
             #{offset VkPhysicalDeviceFeatures2, features}
        type FieldIsArray "features" VkPhysicalDeviceFeatures2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures2, features}

instance {-# OVERLAPPING #-}
         CanReadField "features" VkPhysicalDeviceFeatures2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2, features})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2, features}

instance {-# OVERLAPPING #-}
         CanWriteField "features" VkPhysicalDeviceFeatures2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2, features}

instance Show VkPhysicalDeviceFeatures2 where
        showsPrec d x
          = showString "VkPhysicalDeviceFeatures2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "features = " .
                            showsPrec d (getField @"features" x) . showChar '}'
