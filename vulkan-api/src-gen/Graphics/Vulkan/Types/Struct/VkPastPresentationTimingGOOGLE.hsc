#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPastPresentationTimingGOOGLE
       (VkPastPresentationTimingGOOGLE(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkPastPresentationTimingGOOGLE {
--   >     uint32_t                         presentID;
--   >     uint64_t                         desiredPresentTime;
--   >     uint64_t                         actualPresentTime;
--   >     uint64_t                         earliestPresentTime;
--   >     uint64_t                         presentMargin;
--   > } VkPastPresentationTimingGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPastPresentationTimingGOOGLE.html VkPastPresentationTimingGOOGLE registry at www.khronos.org>
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
         HasVkPresentID VkPastPresentationTimingGOOGLE where
        type VkPresentIDMType VkPastPresentationTimingGOOGLE = Word32

        {-# NOINLINE vkPresentID #-}
        vkPresentID x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, presentID})

        {-# INLINE vkPresentIDByteOffset #-}
        vkPresentIDByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, presentID}

        {-# INLINE readVkPresentID #-}
        readVkPresentID p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, presentID}

        {-# INLINE writeVkPresentID #-}
        writeVkPresentID p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, presentID}

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

instance CanReadField "presentID" VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkPresentID

        {-# INLINE readField #-}
        readField = readVkPresentID

instance CanWriteField "presentID" VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkPresentID

instance {-# OVERLAPPING #-}
         HasVkDesiredPresentTime VkPastPresentationTimingGOOGLE where
        type VkDesiredPresentTimeMType VkPastPresentationTimingGOOGLE =
             Word64

        {-# NOINLINE vkDesiredPresentTime #-}
        vkDesiredPresentTime x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime})

        {-# INLINE vkDesiredPresentTimeByteOffset #-}
        vkDesiredPresentTimeByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

        {-# INLINE readVkDesiredPresentTime #-}
        readVkDesiredPresentTime p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

        {-# INLINE writeVkDesiredPresentTime #-}
        writeVkDesiredPresentTime p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}

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

instance CanReadField "desiredPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkDesiredPresentTime

        {-# INLINE readField #-}
        readField = readVkDesiredPresentTime

instance CanWriteField "desiredPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkDesiredPresentTime

instance {-# OVERLAPPING #-}
         HasVkActualPresentTime VkPastPresentationTimingGOOGLE where
        type VkActualPresentTimeMType VkPastPresentationTimingGOOGLE =
             Word64

        {-# NOINLINE vkActualPresentTime #-}
        vkActualPresentTime x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, actualPresentTime})

        {-# INLINE vkActualPresentTimeByteOffset #-}
        vkActualPresentTimeByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

        {-# INLINE readVkActualPresentTime #-}
        readVkActualPresentTime p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

        {-# INLINE writeVkActualPresentTime #-}
        writeVkActualPresentTime p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}

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

instance CanReadField "actualPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkActualPresentTime

        {-# INLINE readField #-}
        readField = readVkActualPresentTime

instance CanWriteField "actualPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkActualPresentTime

instance {-# OVERLAPPING #-}
         HasVkEarliestPresentTime VkPastPresentationTimingGOOGLE where
        type VkEarliestPresentTimeMType VkPastPresentationTimingGOOGLE =
             Word64

        {-# NOINLINE vkEarliestPresentTime #-}
        vkEarliestPresentTime x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime})

        {-# INLINE vkEarliestPresentTimeByteOffset #-}
        vkEarliestPresentTimeByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

        {-# INLINE readVkEarliestPresentTime #-}
        readVkEarliestPresentTime p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

        {-# INLINE writeVkEarliestPresentTime #-}
        writeVkEarliestPresentTime p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}

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

instance CanReadField "earliestPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkEarliestPresentTime

        {-# INLINE readField #-}
        readField = readVkEarliestPresentTime

instance CanWriteField "earliestPresentTime"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkEarliestPresentTime

instance {-# OVERLAPPING #-}
         HasVkPresentMargin VkPastPresentationTimingGOOGLE where
        type VkPresentMarginMType VkPastPresentationTimingGOOGLE = Word64

        {-# NOINLINE vkPresentMargin #-}
        vkPresentMargin x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPastPresentationTimingGOOGLE, presentMargin})

        {-# INLINE vkPresentMarginByteOffset #-}
        vkPresentMarginByteOffset ~_
          = #{offset VkPastPresentationTimingGOOGLE, presentMargin}

        {-# INLINE readVkPresentMargin #-}
        readVkPresentMargin p
          = peekByteOff p #{offset VkPastPresentationTimingGOOGLE, presentMargin}

        {-# INLINE writeVkPresentMargin #-}
        writeVkPresentMargin p
          = pokeByteOff p #{offset VkPastPresentationTimingGOOGLE, presentMargin}

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

instance CanReadField "presentMargin"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkPresentMargin

        {-# INLINE readField #-}
        readField = readVkPresentMargin

instance CanWriteField "presentMargin"
           VkPastPresentationTimingGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkPresentMargin

instance Show VkPastPresentationTimingGOOGLE where
        showsPrec d x
          = showString "VkPastPresentationTimingGOOGLE {" .
              showString "vkPresentID = " .
                showsPrec d (vkPresentID x) .
                  showString ", " .
                    showString "vkDesiredPresentTime = " .
                      showsPrec d (vkDesiredPresentTime x) .
                        showString ", " .
                          showString "vkActualPresentTime = " .
                            showsPrec d (vkActualPresentTime x) .
                              showString ", " .
                                showString "vkEarliestPresentTime = " .
                                  showsPrec d (vkEarliestPresentTime x) .
                                    showString ", " .
                                      showString "vkPresentMargin = " .
                                        showsPrec d (vkPresentMargin x) . showChar '}'
