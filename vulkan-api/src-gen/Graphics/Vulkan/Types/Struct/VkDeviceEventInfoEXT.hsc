#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceEventInfoEXT
       (VkDeviceEventInfoEXT(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceEventTypeEXT (VkDeviceEventTypeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceEventInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceEventTypeEXT             deviceEvent;
--   > } VkDeviceEventInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDeviceEventInfoEXTVkDeviceEventInfoEXT registry at www.khronos.org>
data VkDeviceEventInfoEXT = VkDeviceEventInfoEXT## Addr## ByteArray##

instance Eq VkDeviceEventInfoEXT where
        (VkDeviceEventInfoEXT## a _) == x@(VkDeviceEventInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceEventInfoEXT where
        (VkDeviceEventInfoEXT## a _) `compare` x@(VkDeviceEventInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceEventInfoEXT where
        sizeOf ~_ = #{size VkDeviceEventInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceEventInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceEventInfoEXT where
        unsafeAddr (VkDeviceEventInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceEventInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceEventInfoEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceEventInfoEXT where
        type StructFields VkDeviceEventInfoEXT =
             '["sType", "pNext", "deviceEvent"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceEventInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDeviceEventInfoEXT
         where
        type FieldType "sType" VkDeviceEventInfoEXT = VkStructureType
        type FieldOptional "sType" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, sType}
        type FieldIsArray "sType" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceEventInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDeviceEventInfoEXT
         where
        type FieldType "pNext" VkDeviceEventInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, pNext}
        type FieldIsArray "pNext" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceEvent" VkDeviceEventInfoEXT where
        type FieldType "deviceEvent" VkDeviceEventInfoEXT =
             VkDeviceEventTypeEXT
        type FieldOptional "deviceEvent" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceEvent" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, deviceEvent}
        type FieldIsArray "deviceEvent" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceEventInfoEXT, deviceEvent}

instance {-# OVERLAPPING #-}
         CanReadField "deviceEvent" VkDeviceEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, deviceEvent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, deviceEvent}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceEvent" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, deviceEvent}

instance Show VkDeviceEventInfoEXT where
        showsPrec d x
          = showString "VkDeviceEventInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceEvent = " .
                            showsPrec d (getField @"deviceEvent" x) . showChar '}'
