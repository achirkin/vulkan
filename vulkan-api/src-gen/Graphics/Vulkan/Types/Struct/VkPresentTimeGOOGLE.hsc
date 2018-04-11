#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPresentTimeGOOGLE
       (VkPresentTimeGOOGLE(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPresentTimeGOOGLE {
--   >     uint32_t                         presentID;
--   >     uint64_t                         desiredPresentTime;
--   > } VkPresentTimeGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPresentTimeGOOGLE VkPresentTimeGOOGLE registry at www.khronos.org>
data VkPresentTimeGOOGLE = VkPresentTimeGOOGLE## Addr## ByteArray##

instance Eq VkPresentTimeGOOGLE where
        (VkPresentTimeGOOGLE## a _) == x@(VkPresentTimeGOOGLE## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentTimeGOOGLE where
        (VkPresentTimeGOOGLE## a _) `compare` x@(VkPresentTimeGOOGLE## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentTimeGOOGLE where
        sizeOf ~_ = #{size VkPresentTimeGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentTimeGOOGLE}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentTimeGOOGLE where
        unsafeAddr (VkPresentTimeGOOGLE## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentTimeGOOGLE## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentTimeGOOGLE## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentTimeGOOGLE where
        type StructFields VkPresentTimeGOOGLE =
             '["presentID", "desiredPresentTime"] -- ' closing tick for hsc2hs
        type CUnionType VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentTimeGOOGLE = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "presentID" VkPresentTimeGOOGLE where
        type FieldType "presentID" VkPresentTimeGOOGLE = Word32
        type FieldOptional "presentID" VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs
        type FieldOffset "presentID" VkPresentTimeGOOGLE =
             #{offset VkPresentTimeGOOGLE, presentID}
        type FieldIsArray "presentID" VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentTimeGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         CanReadField "presentID" VkPresentTimeGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimeGOOGLE, presentID})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimeGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         CanWriteField "presentID" VkPresentTimeGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimeGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         HasField "desiredPresentTime" VkPresentTimeGOOGLE where
        type FieldType "desiredPresentTime" VkPresentTimeGOOGLE = Word64
        type FieldOptional "desiredPresentTime" VkPresentTimeGOOGLE =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "desiredPresentTime" VkPresentTimeGOOGLE =
             #{offset VkPresentTimeGOOGLE, desiredPresentTime}
        type FieldIsArray "desiredPresentTime" VkPresentTimeGOOGLE = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentTimeGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         CanReadField "desiredPresentTime" VkPresentTimeGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentTimeGOOGLE, desiredPresentTime})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentTimeGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         CanWriteField "desiredPresentTime" VkPresentTimeGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentTimeGOOGLE, desiredPresentTime}

instance Show VkPresentTimeGOOGLE where
        showsPrec d x
          = showString "VkPresentTimeGOOGLE {" .
              showString "presentID = " .
                showsPrec d (getField @"presentID" x) .
                  showString ", " .
                    showString "desiredPresentTime = " .
                      showsPrec d (getField @"desiredPresentTime" x) . showChar '}'
