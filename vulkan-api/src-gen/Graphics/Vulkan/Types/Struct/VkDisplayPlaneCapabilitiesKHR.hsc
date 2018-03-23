#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayPlaneCapabilitiesKHR
       (VkDisplayPlaneCapabilitiesKHR(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDisplayPlaneAlphaFlagsKHR (VkDisplayPlaneAlphaFlagsKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D                (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.VkOffset2D                (VkOffset2D)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayPlaneCapabilitiesKHR {
--   >     VkDisplayPlaneAlphaFlagsKHR      supportedAlpha;
--   >     VkOffset2D                       minSrcPosition;
--   >     VkOffset2D                       maxSrcPosition;
--   >     VkExtent2D                       minSrcExtent;
--   >     VkExtent2D                       maxSrcExtent;
--   >     VkOffset2D                       minDstPosition;
--   >     VkOffset2D                       maxDstPosition;
--   >     VkExtent2D                       minDstExtent;
--   >     VkExtent2D                       maxDstExtent;
--   > } VkDisplayPlaneCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDisplayPlaneCapabilitiesKHR.html VkDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
data VkDisplayPlaneCapabilitiesKHR = VkDisplayPlaneCapabilitiesKHR## Addr##
                                                                    ByteArray##

instance Eq VkDisplayPlaneCapabilitiesKHR where
        (VkDisplayPlaneCapabilitiesKHR## a _) ==
          x@(VkDisplayPlaneCapabilitiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlaneCapabilitiesKHR where
        (VkDisplayPlaneCapabilitiesKHR## a _) `compare`
          x@(VkDisplayPlaneCapabilitiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlaneCapabilitiesKHR where
        sizeOf ~_ = #{size VkDisplayPlaneCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplayPlaneCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPlaneCapabilitiesKHR where
        unsafeAddr (VkDisplayPlaneCapabilitiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPlaneCapabilitiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPlaneCapabilitiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPlaneCapabilitiesKHR where
        type StructFields VkDisplayPlaneCapabilitiesKHR =
             '["supportedAlpha", "minSrcPosition", "maxSrcPosition", -- ' closing tick for hsc2hs
               "minSrcExtent", "maxSrcExtent", "minDstPosition", "maxDstPosition",
               "minDstExtent", "maxDstExtent"]
        type CUnionType VkDisplayPlaneCapabilitiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPlaneCapabilitiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPlaneCapabilitiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "supportedAlpha" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             VkDisplayPlaneAlphaFlagsKHR
        type FieldOptional "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}
        type FieldIsArray "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance {-# OVERLAPPING #-}
         CanReadField "supportedAlpha" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedAlpha" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance {-# OVERLAPPING #-}
         HasField "minSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}
        type FieldIsArray "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance {-# OVERLAPPING #-}
         CanReadField "minSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance {-# OVERLAPPING #-}
         CanWriteField "minSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance {-# OVERLAPPING #-}
         HasField "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}
        type FieldIsArray "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance {-# OVERLAPPING #-}
         CanReadField "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance {-# OVERLAPPING #-}
         HasField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}
        type FieldIsArray "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance {-# OVERLAPPING #-}
         CanReadField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance {-# OVERLAPPING #-}
         HasField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}
        type FieldIsArray "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance {-# OVERLAPPING #-}
         HasField "minDstPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}
        type FieldIsArray "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance {-# OVERLAPPING #-}
         CanReadField "minDstPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance {-# OVERLAPPING #-}
         CanWriteField "minDstPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance {-# OVERLAPPING #-}
         HasField "maxDstPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}
        type FieldIsArray "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance {-# OVERLAPPING #-}
         CanReadField "maxDstPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDstPosition" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance {-# OVERLAPPING #-}
         HasField "minDstExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}
        type FieldIsArray "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance {-# OVERLAPPING #-}
         CanReadField "minDstExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "minDstExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance {-# OVERLAPPING #-}
         HasField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}
        type FieldIsArray "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance Show VkDisplayPlaneCapabilitiesKHR where
        showsPrec d x
          = showString "VkDisplayPlaneCapabilitiesKHR {" .
              showString "supportedAlpha = " .
                showsPrec d (getField @"supportedAlpha" x) .
                  showString ", " .
                    showString "minSrcPosition = " .
                      showsPrec d (getField @"minSrcPosition" x) .
                        showString ", " .
                          showString "maxSrcPosition = " .
                            showsPrec d (getField @"maxSrcPosition" x) .
                              showString ", " .
                                showString "minSrcExtent = " .
                                  showsPrec d (getField @"minSrcExtent" x) .
                                    showString ", " .
                                      showString "maxSrcExtent = " .
                                        showsPrec d (getField @"maxSrcExtent" x) .
                                          showString ", " .
                                            showString "minDstPosition = " .
                                              showsPrec d (getField @"minDstPosition" x) .
                                                showString ", " .
                                                  showString "maxDstPosition = " .
                                                    showsPrec d (getField @"maxDstPosition" x) .
                                                      showString ", " .
                                                        showString "minDstExtent = " .
                                                          showsPrec d (getField @"minDstExtent" x) .
                                                            showString ", " .
                                                              showString "maxDstExtent = " .
                                                                showsPrec d
                                                                  (getField @"maxDstExtent" x)
                                                                  . showChar '}'
