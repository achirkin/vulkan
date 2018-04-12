#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PastPresentationTimingGOOGLE
       (VkPastPresentationTimingGOOGLE(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPastPresentationTimingGOOGLE {
--   >     uint32_t                         presentID;
--   >     uint64_t                         desiredPresentTime;
--   >     uint64_t                         actualPresentTime;
--   >     uint64_t                         earliestPresentTime;
--   >     uint64_t                         presentMargin;
--   > } VkPastPresentationTimingGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPastPresentationTimingGOOGLE VkPastPresentationTimingGOOGLE registry at www.khronos.org>
data VkPastPresentationTimingGOOGLE = VkPastPresentationTimingGOOGLE## Addr##
                                                                      ByteArray##

instance Eq VkPastPresentationTimingGOOGLE where
        (VkPastPresentationTimingGOOGLE## a _) ==
          x@(VkPastPresentationTimingGOOGLE## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPastPresentationTimingGOOGLE where
        (VkPastPresentationTimingGOOGLE## a _) `compare`
          x@(VkPastPresentationTimingGOOGLE## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPastPresentationTimingGOOGLE where
        sizeOf ~_ = #{size VkPastPresentationTimingGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPastPresentationTimingGOOGLE}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPastPresentationTimingGOOGLE where
        unsafeAddr (VkPastPresentationTimingGOOGLE## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPastPresentationTimingGOOGLE## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPastPresentationTimingGOOGLE##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPastPresentationTimingGOOGLE where
        type StructFields VkPastPresentationTimingGOOGLE =
             '["presentID", "desiredPresentTime", "actualPresentTime", -- ' closing tick for hsc2hs
               "earliestPresentTime", "presentMargin"]
        type CUnionType VkPastPresentationTimingGOOGLE = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPastPresentationTimingGOOGLE = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPastPresentationTimingGOOGLE = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "presentID" VkPastPresentationTimingGOOGLE where
        type FieldType "presentID" VkPastPresentationTimingGOOGLE = Word32
        type FieldOptional "presentID" VkPastPresentationTimingGOOGLE =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "presentID" VkPastPresentationTimingGOOGLE =
             #{offset VkPastPresentationTimingGOOGLE, presentID}
        type FieldIsArray "presentID" VkPastPresentationTimingGOOGLE =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPastPresentationTimingGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         CanReadField "presentID" VkPastPresentationTimingGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, presentID})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         CanWriteField "presentID" VkPastPresentationTimingGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, presentID}

instance {-# OVERLAPPING #-}
         HasField "desiredPresentTime" VkPastPresentationTimingGOOGLE where
        type FieldType "desiredPresentTime" VkPastPresentationTimingGOOGLE
             = Word64
        type FieldOptional "desiredPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "desiredPresentTime"
               VkPastPresentationTimingGOOGLE
             =
             #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}
        type FieldIsArray "desiredPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         CanReadField "desiredPresentTime" VkPastPresentationTimingGOOGLE
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         CanWriteField "desiredPresentTime" VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

instance {-# OVERLAPPING #-}
         HasField "actualPresentTime" VkPastPresentationTimingGOOGLE where
        type FieldType "actualPresentTime" VkPastPresentationTimingGOOGLE =
             Word64
        type FieldOptional "actualPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "actualPresentTime" VkPastPresentationTimingGOOGLE
             =
             #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}
        type FieldIsArray "actualPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

instance {-# OVERLAPPING #-}
         CanReadField "actualPresentTime" VkPastPresentationTimingGOOGLE
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, actualPresentTime})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

instance {-# OVERLAPPING #-}
         CanWriteField "actualPresentTime" VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

instance {-# OVERLAPPING #-}
         HasField "earliestPresentTime" VkPastPresentationTimingGOOGLE where
        type FieldType "earliestPresentTime" VkPastPresentationTimingGOOGLE
             = Word64
        type FieldOptional "earliestPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "earliestPresentTime"
               VkPastPresentationTimingGOOGLE
             =
             #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}
        type FieldIsArray "earliestPresentTime"
               VkPastPresentationTimingGOOGLE
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

instance {-# OVERLAPPING #-}
         CanReadField "earliestPresentTime" VkPastPresentationTimingGOOGLE
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

instance {-# OVERLAPPING #-}
         CanWriteField "earliestPresentTime" VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

instance {-# OVERLAPPING #-}
         HasField "presentMargin" VkPastPresentationTimingGOOGLE where
        type FieldType "presentMargin" VkPastPresentationTimingGOOGLE =
             Word64
        type FieldOptional "presentMargin" VkPastPresentationTimingGOOGLE =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "presentMargin" VkPastPresentationTimingGOOGLE =
             #{offset VkPastPresentationTimingGOOGLE, presentMargin}
        type FieldIsArray "presentMargin" VkPastPresentationTimingGOOGLE =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPastPresentationTimingGOOGLE, presentMargin}

instance {-# OVERLAPPING #-}
         CanReadField "presentMargin" VkPastPresentationTimingGOOGLE where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, presentMargin})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, presentMargin}

instance {-# OVERLAPPING #-}
         CanWriteField "presentMargin" VkPastPresentationTimingGOOGLE where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, presentMargin}

instance Show VkPastPresentationTimingGOOGLE where
        showsPrec d x
          = showString "VkPastPresentationTimingGOOGLE {" .
              showString "presentID = " .
                showsPrec d (getField @"presentID" x) .
                  showString ", " .
                    showString "desiredPresentTime = " .
                      showsPrec d (getField @"desiredPresentTime" x) .
                        showString ", " .
                          showString "actualPresentTime = " .
                            showsPrec d (getField @"actualPresentTime" x) .
                              showString ", " .
                                showString "earliestPresentTime = " .
                                  showsPrec d (getField @"earliestPresentTime" x) .
                                    showString ", " .
                                      showString "presentMargin = " .
                                        showsPrec d (getField @"presentMargin" x) . showChar '}'
