#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDevicePointClippingPropertiesKHR
       (VkPhysicalDevicePointClippingPropertiesKHR(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkPointClippingBehaviorKHR       (VkPointClippingBehaviorKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDevicePointClippingPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPointClippingBehaviorKHR      pointClippingBehavior;
--   > } VkPhysicalDevicePointClippingPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDevicePointClippingPropertiesKHR.html VkPhysicalDevicePointClippingPropertiesKHR registry at www.khronos.org>
data VkPhysicalDevicePointClippingPropertiesKHR = VkPhysicalDevicePointClippingPropertiesKHR## Addr##
                                                                                              ByteArray##

instance Eq VkPhysicalDevicePointClippingPropertiesKHR where
        (VkPhysicalDevicePointClippingPropertiesKHR## a _) ==
          x@(VkPhysicalDevicePointClippingPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevicePointClippingPropertiesKHR where
        (VkPhysicalDevicePointClippingPropertiesKHR## a _) `compare`
          x@(VkPhysicalDevicePointClippingPropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevicePointClippingPropertiesKHR where
        sizeOf ~_
          = #{size VkPhysicalDevicePointClippingPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevicePointClippingPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        unsafeAddr (VkPhysicalDevicePointClippingPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDevicePointClippingPropertiesKHR## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDevicePointClippingPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDevicePointClippingPropertiesKHR
         where
        type StructFields VkPhysicalDevicePointClippingPropertiesKHR =
             '["sType", "pNext", "pointClippingBehavior"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDevicePointClippingPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDevicePointClippingPropertiesKHR =
             'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDevicePointClippingPropertiesKHR =
             '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDevicePointClippingPropertiesKHR where
        type FieldType "sType" VkPhysicalDevicePointClippingPropertiesKHR =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDevicePointClippingPropertiesKHR
             =
             #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}
        type FieldIsArray "sType"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDevicePointClippingPropertiesKHR where
        type FieldType "pNext" VkPhysicalDevicePointClippingPropertiesKHR =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDevicePointClippingPropertiesKHR
             =
             #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pointClippingBehavior"
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        type FieldType "pointClippingBehavior"
               VkPhysicalDevicePointClippingPropertiesKHR
             = VkPointClippingBehaviorKHR
        type FieldOptional "pointClippingBehavior"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pointClippingBehavior"
               VkPhysicalDevicePointClippingPropertiesKHR
             =
             #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}
        type FieldIsArray "pointClippingBehavior"
               VkPhysicalDevicePointClippingPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}

instance {-# OVERLAPPING #-}
         CanReadField "pointClippingBehavior"
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}

instance {-# OVERLAPPING #-}
         CanWriteField "pointClippingBehavior"
           VkPhysicalDevicePointClippingPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePointClippingPropertiesKHR, pointClippingBehavior}

instance Show VkPhysicalDevicePointClippingPropertiesKHR where
        showsPrec d x
          = showString "VkPhysicalDevicePointClippingPropertiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pointClippingBehavior = " .
                            showsPrec d (getField @"pointClippingBehavior" x) . showChar '}'
