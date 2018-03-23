#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayPresentInfoKHR
       (VkDisplayPresentInfoKHR(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.VkRect2D         (VkRect2D)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     VkRect2D                         srcRect;
--   >     VkRect2D                         dstRect;
--   >     VkBool32                         persistent;
--   > } VkDisplayPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDisplayPresentInfoKHR.html VkDisplayPresentInfoKHR registry at www.khronos.org>
data VkDisplayPresentInfoKHR = VkDisplayPresentInfoKHR## Addr##
                                                        ByteArray##

instance Eq VkDisplayPresentInfoKHR where
        (VkDisplayPresentInfoKHR## a _) == x@(VkDisplayPresentInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPresentInfoKHR where
        (VkDisplayPresentInfoKHR## a _) `compare`
          x@(VkDisplayPresentInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPresentInfoKHR where
        sizeOf ~_ = #{size VkDisplayPresentInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPresentInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPresentInfoKHR where
        unsafeAddr (VkDisplayPresentInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPresentInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPresentInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPresentInfoKHR where
        type StructFields VkDisplayPresentInfoKHR =
             '["sType", "pNext", "srcRect", "dstRect", "persistent"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPresentInfoKHR = '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayPresentInfoKHR where
        type FieldType "sType" VkDisplayPresentInfoKHR = VkStructureType
        type FieldOptional "sType" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, sType}
        type FieldIsArray "sType" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayPresentInfoKHR where
        type FieldType "pNext" VkDisplayPresentInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, pNext}
        type FieldIsArray "pNext" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcRect" VkDisplayPresentInfoKHR where
        type FieldType "srcRect" VkDisplayPresentInfoKHR = VkRect2D
        type FieldOptional "srcRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcRect" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, srcRect}
        type FieldIsArray "srcRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, srcRect}

instance {-# OVERLAPPING #-}
         CanReadField "srcRect" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, srcRect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, srcRect}

instance {-# OVERLAPPING #-}
         CanWriteField "srcRect" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, srcRect}

instance {-# OVERLAPPING #-}
         HasField "dstRect" VkDisplayPresentInfoKHR where
        type FieldType "dstRect" VkDisplayPresentInfoKHR = VkRect2D
        type FieldOptional "dstRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstRect" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, dstRect}
        type FieldIsArray "dstRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, dstRect}

instance {-# OVERLAPPING #-}
         CanReadField "dstRect" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, dstRect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, dstRect}

instance {-# OVERLAPPING #-}
         CanWriteField "dstRect" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, dstRect}

instance {-# OVERLAPPING #-}
         HasField "persistent" VkDisplayPresentInfoKHR where
        type FieldType "persistent" VkDisplayPresentInfoKHR = VkBool32
        type FieldOptional "persistent" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "persistent" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, persistent}
        type FieldIsArray "persistent" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, persistent}

instance {-# OVERLAPPING #-}
         CanReadField "persistent" VkDisplayPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, persistent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, persistent}

instance {-# OVERLAPPING #-}
         CanWriteField "persistent" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, persistent}

instance Show VkDisplayPresentInfoKHR where
        showsPrec d x
          = showString "VkDisplayPresentInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcRect = " .
                            showsPrec d (getField @"srcRect" x) .
                              showString ", " .
                                showString "dstRect = " .
                                  showsPrec d (getField @"dstRect" x) .
                                    showString ", " .
                                      showString "persistent = " .
                                        showsPrec d (getField @"persistent" x) . showChar '}'
