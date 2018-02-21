#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkViSurfaceCreateInfoNN
       (VkViSurfaceCreateInfoNN(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkViSurfaceCreateFlagsNN)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkViSurfaceCreateInfoNN {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkViSurfaceCreateFlagsNN   flags;
--   >     void*                            window;
--   > } VkViSurfaceCreateInfoNN;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkViSurfaceCreateInfoNN.html VkViSurfaceCreateInfoNN registry at www.khronos.org>
data VkViSurfaceCreateInfoNN = VkViSurfaceCreateInfoNN## Addr##
                                                        ByteArray##

instance Eq VkViSurfaceCreateInfoNN where
        (VkViSurfaceCreateInfoNN## a _) == x@(VkViSurfaceCreateInfoNN## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkViSurfaceCreateInfoNN where
        (VkViSurfaceCreateInfoNN## a _) `compare`
          x@(VkViSurfaceCreateInfoNN## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkViSurfaceCreateInfoNN where
        sizeOf ~_ = #{size VkViSurfaceCreateInfoNN}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViSurfaceCreateInfoNN}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkViSurfaceCreateInfoNN where
        unsafeAddr (VkViSurfaceCreateInfoNN## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkViSurfaceCreateInfoNN## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkViSurfaceCreateInfoNN## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkViSurfaceCreateInfoNN where
        type StructFields VkViSurfaceCreateInfoNN =
             '["sType", "pNext", "flags", "window"] -- ' closing tick for hsc2hs
        type CUnionType VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type StructExtends VkViSurfaceCreateInfoNN = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkViSurfaceCreateInfoNN
         where
        type VkSTypeMType VkViSurfaceCreateInfoNN = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkViSurfaceCreateInfoNN, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkViSurfaceCreateInfoNN where
        type FieldType "sType" VkViSurfaceCreateInfoNN = VkStructureType
        type FieldOptional "sType" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, sType}
        type FieldIsArray "sType" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, sType}

instance CanReadField "sType" VkViSurfaceCreateInfoNN where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkViSurfaceCreateInfoNN
         where
        type VkPNextMType VkViSurfaceCreateInfoNN = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkViSurfaceCreateInfoNN, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkViSurfaceCreateInfoNN where
        type FieldType "pNext" VkViSurfaceCreateInfoNN = Ptr Void
        type FieldOptional "pNext" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, pNext}
        type FieldIsArray "pNext" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, pNext}

instance CanReadField "pNext" VkViSurfaceCreateInfoNN where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkViSurfaceCreateInfoNN
         where
        type VkFlagsMType VkViSurfaceCreateInfoNN =
             VkViSurfaceCreateFlagsNN

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkViSurfaceCreateInfoNN, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkViSurfaceCreateInfoNN where
        type FieldType "flags" VkViSurfaceCreateInfoNN =
             VkViSurfaceCreateFlagsNN
        type FieldOptional "flags" VkViSurfaceCreateInfoNN = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, flags}
        type FieldIsArray "flags" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, flags}

instance CanReadField "flags" VkViSurfaceCreateInfoNN where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkWindow VkViSurfaceCreateInfoNN
         where
        type VkWindowMType VkViSurfaceCreateInfoNN = Ptr Void

        {-# NOINLINE vkWindow #-}
        vkWindow x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, window})

        {-# INLINE vkWindowByteOffset #-}
        vkWindowByteOffset ~_
          = #{offset VkViSurfaceCreateInfoNN, window}

        {-# INLINE readVkWindow #-}
        readVkWindow p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, window}

        {-# INLINE writeVkWindow #-}
        writeVkWindow p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, window}

instance {-# OVERLAPPING #-}
         HasField "window" VkViSurfaceCreateInfoNN where
        type FieldType "window" VkViSurfaceCreateInfoNN = Ptr Void
        type FieldOptional "window" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, window}
        type FieldIsArray "window" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, window}

instance CanReadField "window" VkViSurfaceCreateInfoNN where
        {-# INLINE getField #-}
        getField = vkWindow

        {-# INLINE readField #-}
        readField = readVkWindow

instance CanWriteField "window" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField = writeVkWindow

instance Show VkViSurfaceCreateInfoNN where
        showsPrec d x
          = showString "VkViSurfaceCreateInfoNN {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkWindow = " . showsPrec d (vkWindow x) . showChar '}'
