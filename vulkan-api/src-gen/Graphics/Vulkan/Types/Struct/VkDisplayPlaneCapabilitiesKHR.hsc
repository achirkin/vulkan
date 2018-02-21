#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayPlaneCapabilitiesKHR.html VkDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
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
         HasVkSupportedAlpha VkDisplayPlaneCapabilitiesKHR where
        type VkSupportedAlphaMType VkDisplayPlaneCapabilitiesKHR =
             VkDisplayPlaneAlphaFlagsKHR

        {-# NOINLINE vkSupportedAlpha #-}
        vkSupportedAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha})

        {-# INLINE vkSupportedAlphaByteOffset #-}
        vkSupportedAlphaByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

        {-# INLINE readVkSupportedAlpha #-}
        readVkSupportedAlpha p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

        {-# INLINE writeVkSupportedAlpha #-}
        writeVkSupportedAlpha p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

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

instance CanReadField "supportedAlpha"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSupportedAlpha

        {-# INLINE readField #-}
        readField = readVkSupportedAlpha

instance {-# OVERLAPPING #-}
         HasVkMinSrcPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMinSrcPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMinSrcPosition #-}
        vkMinSrcPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition})

        {-# INLINE vkMinSrcPositionByteOffset #-}
        vkMinSrcPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

        {-# INLINE readVkMinSrcPosition #-}
        readVkMinSrcPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

        {-# INLINE writeVkMinSrcPosition #-}
        writeVkMinSrcPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

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

instance CanReadField "minSrcPosition"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinSrcPosition

        {-# INLINE readField #-}
        readField = readVkMinSrcPosition

instance {-# OVERLAPPING #-}
         HasVkMaxSrcPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMaxSrcPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMaxSrcPosition #-}
        vkMaxSrcPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition})

        {-# INLINE vkMaxSrcPositionByteOffset #-}
        vkMaxSrcPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

        {-# INLINE readVkMaxSrcPosition #-}
        readVkMaxSrcPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

        {-# INLINE writeVkMaxSrcPosition #-}
        writeVkMaxSrcPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

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

instance CanReadField "maxSrcPosition"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxSrcPosition

        {-# INLINE readField #-}
        readField = readVkMaxSrcPosition

instance {-# OVERLAPPING #-}
         HasVkMinSrcExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMinSrcExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMinSrcExtent #-}
        vkMinSrcExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent})

        {-# INLINE vkMinSrcExtentByteOffset #-}
        vkMinSrcExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

        {-# INLINE readVkMinSrcExtent #-}
        readVkMinSrcExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

        {-# INLINE writeVkMinSrcExtent #-}
        writeVkMinSrcExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

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

instance CanReadField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinSrcExtent

        {-# INLINE readField #-}
        readField = readVkMinSrcExtent

instance {-# OVERLAPPING #-}
         HasVkMaxSrcExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMaxSrcExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMaxSrcExtent #-}
        vkMaxSrcExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent})

        {-# INLINE vkMaxSrcExtentByteOffset #-}
        vkMaxSrcExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

        {-# INLINE readVkMaxSrcExtent #-}
        readVkMaxSrcExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

        {-# INLINE writeVkMaxSrcExtent #-}
        writeVkMaxSrcExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

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

instance CanReadField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxSrcExtent

        {-# INLINE readField #-}
        readField = readVkMaxSrcExtent

instance {-# OVERLAPPING #-}
         HasVkMinDstPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMinDstPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMinDstPosition #-}
        vkMinDstPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition})

        {-# INLINE vkMinDstPositionByteOffset #-}
        vkMinDstPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

        {-# INLINE readVkMinDstPosition #-}
        readVkMinDstPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

        {-# INLINE writeVkMinDstPosition #-}
        writeVkMinDstPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

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

instance CanReadField "minDstPosition"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinDstPosition

        {-# INLINE readField #-}
        readField = readVkMinDstPosition

instance {-# OVERLAPPING #-}
         HasVkMaxDstPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMaxDstPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMaxDstPosition #-}
        vkMaxDstPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition})

        {-# INLINE vkMaxDstPositionByteOffset #-}
        vkMaxDstPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

        {-# INLINE readVkMaxDstPosition #-}
        readVkMaxDstPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

        {-# INLINE writeVkMaxDstPosition #-}
        writeVkMaxDstPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

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

instance CanReadField "maxDstPosition"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxDstPosition

        {-# INLINE readField #-}
        readField = readVkMaxDstPosition

instance {-# OVERLAPPING #-}
         HasVkMinDstExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMinDstExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMinDstExtent #-}
        vkMinDstExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent})

        {-# INLINE vkMinDstExtentByteOffset #-}
        vkMinDstExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

        {-# INLINE readVkMinDstExtent #-}
        readVkMinDstExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

        {-# INLINE writeVkMinDstExtent #-}
        writeVkMinDstExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

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

instance CanReadField "minDstExtent" VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinDstExtent

        {-# INLINE readField #-}
        readField = readVkMinDstExtent

instance {-# OVERLAPPING #-}
         HasVkMaxDstExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMaxDstExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMaxDstExtent #-}
        vkMaxDstExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent})

        {-# INLINE vkMaxDstExtentByteOffset #-}
        vkMaxDstExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

        {-# INLINE readVkMaxDstExtent #-}
        readVkMaxDstExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

        {-# INLINE writeVkMaxDstExtent #-}
        writeVkMaxDstExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

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

instance CanReadField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxDstExtent

        {-# INLINE readField #-}
        readField = readVkMaxDstExtent

instance Show VkDisplayPlaneCapabilitiesKHR where
        showsPrec d x
          = showString "VkDisplayPlaneCapabilitiesKHR {" .
              showString "vkSupportedAlpha = " .
                showsPrec d (vkSupportedAlpha x) .
                  showString ", " .
                    showString "vkMinSrcPosition = " .
                      showsPrec d (vkMinSrcPosition x) .
                        showString ", " .
                          showString "vkMaxSrcPosition = " .
                            showsPrec d (vkMaxSrcPosition x) .
                              showString ", " .
                                showString "vkMinSrcExtent = " .
                                  showsPrec d (vkMinSrcExtent x) .
                                    showString ", " .
                                      showString "vkMaxSrcExtent = " .
                                        showsPrec d (vkMaxSrcExtent x) .
                                          showString ", " .
                                            showString "vkMinDstPosition = " .
                                              showsPrec d (vkMinDstPosition x) .
                                                showString ", " .
                                                  showString "vkMaxDstPosition = " .
                                                    showsPrec d (vkMaxDstPosition x) .
                                                      showString ", " .
                                                        showString "vkMinDstExtent = " .
                                                          showsPrec d (vkMinDstExtent x) .
                                                            showString ", " .
                                                              showString "vkMaxDstExtent = " .
                                                                showsPrec d (vkMaxDstExtent x) .
                                                                  showChar '}'
