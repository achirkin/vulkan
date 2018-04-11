#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayEventInfoEXT
       (VkDisplayEventInfoEXT(..)) where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Base                                         (Addr##,
                                                                   ByteArray##,
                                                                   byteArrayContents##,
                                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDisplayEventTypeEXT (VkDisplayEventTypeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType       (VkStructureType)
import           System.IO.Unsafe                                 (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayEventInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayEventTypeEXT            displayEvent;
--   > } VkDisplayEventInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDisplayEventInfoEXT VkDisplayEventInfoEXT registry at www.khronos.org>
data VkDisplayEventInfoEXT = VkDisplayEventInfoEXT## Addr##
                                                    ByteArray##

instance Eq VkDisplayEventInfoEXT where
        (VkDisplayEventInfoEXT## a _) == x@(VkDisplayEventInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayEventInfoEXT where
        (VkDisplayEventInfoEXT## a _) `compare`
          x@(VkDisplayEventInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayEventInfoEXT where
        sizeOf ~_ = #{size VkDisplayEventInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayEventInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayEventInfoEXT where
        unsafeAddr (VkDisplayEventInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayEventInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayEventInfoEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayEventInfoEXT where
        type StructFields VkDisplayEventInfoEXT =
             '["sType", "pNext", "displayEvent"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayEventInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDisplayEventInfoEXT
         where
        type FieldType "sType" VkDisplayEventInfoEXT = VkStructureType
        type FieldOptional "sType" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayEventInfoEXT =
             #{offset VkDisplayEventInfoEXT, sType}
        type FieldIsArray "sType" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayEventInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDisplayEventInfoEXT
         where
        type FieldType "pNext" VkDisplayEventInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayEventInfoEXT =
             #{offset VkDisplayEventInfoEXT, pNext}
        type FieldIsArray "pNext" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "displayEvent" VkDisplayEventInfoEXT where
        type FieldType "displayEvent" VkDisplayEventInfoEXT =
             VkDisplayEventTypeEXT
        type FieldOptional "displayEvent" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayEvent" VkDisplayEventInfoEXT =
             #{offset VkDisplayEventInfoEXT, displayEvent}
        type FieldIsArray "displayEvent" VkDisplayEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayEventInfoEXT, displayEvent}

instance {-# OVERLAPPING #-}
         CanReadField "displayEvent" VkDisplayEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayEventInfoEXT, displayEvent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayEventInfoEXT, displayEvent}

instance {-# OVERLAPPING #-}
         CanWriteField "displayEvent" VkDisplayEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayEventInfoEXT, displayEvent}

instance Show VkDisplayEventInfoEXT where
        showsPrec d x
          = showString "VkDisplayEventInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "displayEvent = " .
                            showsPrec d (getField @"displayEvent" x) . showChar '}'
