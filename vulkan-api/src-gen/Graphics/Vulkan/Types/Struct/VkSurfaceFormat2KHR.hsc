#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSurfaceFormat2KHR
       (VkSurfaceFormat2KHR(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSurfaceFormatKHR (VkSurfaceFormatKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkSurfaceFormat2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkSurfaceFormatKHR surfaceFormat;
--   > } VkSurfaceFormat2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSurfaceFormat2KHR.html VkSurfaceFormat2KHR registry at www.khronos.org>
data VkSurfaceFormat2KHR = VkSurfaceFormat2KHR## Addr## ByteArray##

instance Eq VkSurfaceFormat2KHR where
        (VkSurfaceFormat2KHR## a _) == x@(VkSurfaceFormat2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceFormat2KHR where
        (VkSurfaceFormat2KHR## a _) `compare` x@(VkSurfaceFormat2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceFormat2KHR where
        sizeOf ~_ = #{size VkSurfaceFormat2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceFormat2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceFormat2KHR where
        unsafeAddr (VkSurfaceFormat2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceFormat2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceFormat2KHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceFormat2KHR where
        type StructFields VkSurfaceFormat2KHR =
             '["sType", "pNext", "surfaceFormat"] -- ' closing tick for hsc2hs
        type CUnionType VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceFormat2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceFormat2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkSurfaceFormat2KHR where
        type VkSTypeMType VkSurfaceFormat2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkSurfaceFormat2KHR
         where
        type FieldType "sType" VkSurfaceFormat2KHR = VkStructureType
        type FieldOptional "sType" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, sType}
        type FieldIsArray "sType" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormat2KHR, sType}

instance CanReadField "sType" VkSurfaceFormat2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSurfaceFormat2KHR where
        type VkPNextMType VkSurfaceFormat2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkSurfaceFormat2KHR
         where
        type FieldType "pNext" VkSurfaceFormat2KHR = Ptr Void
        type FieldOptional "pNext" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, pNext}
        type FieldIsArray "pNext" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormat2KHR, pNext}

instance CanReadField "pNext" VkSurfaceFormat2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-} HasVkSurfaceFormat VkSurfaceFormat2KHR
         where
        type VkSurfaceFormatMType VkSurfaceFormat2KHR = VkSurfaceFormatKHR

        {-# NOINLINE vkSurfaceFormat #-}
        vkSurfaceFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, surfaceFormat})

        {-# INLINE vkSurfaceFormatByteOffset #-}
        vkSurfaceFormatByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, surfaceFormat}

        {-# INLINE readVkSurfaceFormat #-}
        readVkSurfaceFormat p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, surfaceFormat}

        {-# INLINE writeVkSurfaceFormat #-}
        writeVkSurfaceFormat p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, surfaceFormat}

instance {-# OVERLAPPING #-}
         HasField "surfaceFormat" VkSurfaceFormat2KHR where
        type FieldType "surfaceFormat" VkSurfaceFormat2KHR =
             VkSurfaceFormatKHR
        type FieldOptional "surfaceFormat" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surfaceFormat" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, surfaceFormat}
        type FieldIsArray "surfaceFormat" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceFormat2KHR, surfaceFormat}

instance CanReadField "surfaceFormat" VkSurfaceFormat2KHR where
        {-# INLINE getField #-}
        getField = vkSurfaceFormat

        {-# INLINE readField #-}
        readField = readVkSurfaceFormat

instance Show VkSurfaceFormat2KHR where
        showsPrec d x
          = showString "VkSurfaceFormat2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurfaceFormat = " .
                            showsPrec d (vkSurfaceFormat x) . showChar '}'
