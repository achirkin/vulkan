#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkIOSSurfaceCreateInfoMVK
       (VkIOSSurfaceCreateInfoMVK(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkIOSSurfaceCreateFlagsMVK)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkIOSSurfaceCreateInfoMVK {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkIOSSurfaceCreateFlagsMVK     flags;
--   >     const void*                                    pView;
--   > } VkIOSSurfaceCreateInfoMVK;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkIOSSurfaceCreateInfoMVK.html VkIOSSurfaceCreateInfoMVK registry at www.khronos.org>
data VkIOSSurfaceCreateInfoMVK = VkIOSSurfaceCreateInfoMVK## Addr##
                                                            ByteArray##

instance Eq VkIOSSurfaceCreateInfoMVK where
        (VkIOSSurfaceCreateInfoMVK## a _) ==
          x@(VkIOSSurfaceCreateInfoMVK## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkIOSSurfaceCreateInfoMVK where
        (VkIOSSurfaceCreateInfoMVK## a _) `compare`
          x@(VkIOSSurfaceCreateInfoMVK## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkIOSSurfaceCreateInfoMVK where
        sizeOf ~_ = #{size VkIOSSurfaceCreateInfoMVK}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkIOSSurfaceCreateInfoMVK}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkIOSSurfaceCreateInfoMVK where
        unsafeAddr (VkIOSSurfaceCreateInfoMVK## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkIOSSurfaceCreateInfoMVK## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkIOSSurfaceCreateInfoMVK## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkIOSSurfaceCreateInfoMVK where
        type StructFields VkIOSSurfaceCreateInfoMVK =
             '["sType", "pNext", "flags", "pView"] -- ' closing tick for hsc2hs
        type CUnionType VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type StructExtends VkIOSSurfaceCreateInfoMVK = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkIOSSurfaceCreateInfoMVK
         where
        type VkSTypeMType VkIOSSurfaceCreateInfoMVK = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkIOSSurfaceCreateInfoMVK, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkIOSSurfaceCreateInfoMVK where
        type FieldType "sType" VkIOSSurfaceCreateInfoMVK = VkStructureType
        type FieldOptional "sType" VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkIOSSurfaceCreateInfoMVK =
             #{offset VkIOSSurfaceCreateInfoMVK, sType}
        type FieldIsArray "sType" VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIOSSurfaceCreateInfoMVK, sType}

instance CanReadField "sType" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkIOSSurfaceCreateInfoMVK
         where
        type VkPNextMType VkIOSSurfaceCreateInfoMVK = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkIOSSurfaceCreateInfoMVK, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkIOSSurfaceCreateInfoMVK where
        type FieldType "pNext" VkIOSSurfaceCreateInfoMVK = Ptr Void
        type FieldOptional "pNext" VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkIOSSurfaceCreateInfoMVK =
             #{offset VkIOSSurfaceCreateInfoMVK, pNext}
        type FieldIsArray "pNext" VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIOSSurfaceCreateInfoMVK, pNext}

instance CanReadField "pNext" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkIOSSurfaceCreateInfoMVK
         where
        type VkFlagsMType VkIOSSurfaceCreateInfoMVK =
             VkIOSSurfaceCreateFlagsMVK

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkIOSSurfaceCreateInfoMVK, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkIOSSurfaceCreateInfoMVK where
        type FieldType "flags" VkIOSSurfaceCreateInfoMVK =
             VkIOSSurfaceCreateFlagsMVK
        type FieldOptional "flags" VkIOSSurfaceCreateInfoMVK = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkIOSSurfaceCreateInfoMVK =
             #{offset VkIOSSurfaceCreateInfoMVK, flags}
        type FieldIsArray "flags" VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIOSSurfaceCreateInfoMVK, flags}

instance CanReadField "flags" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkPView VkIOSSurfaceCreateInfoMVK
         where
        type VkPViewMType VkIOSSurfaceCreateInfoMVK = Ptr Void

        {-# NOINLINE vkPView #-}
        vkPView x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, pView})

        {-# INLINE vkPViewByteOffset #-}
        vkPViewByteOffset ~_
          = #{offset VkIOSSurfaceCreateInfoMVK, pView}

        {-# INLINE readVkPView #-}
        readVkPView p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pView}

        {-# INLINE writeVkPView #-}
        writeVkPView p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pView}

instance {-# OVERLAPPING #-}
         HasField "pView" VkIOSSurfaceCreateInfoMVK where
        type FieldType "pView" VkIOSSurfaceCreateInfoMVK = Ptr Void
        type FieldOptional "pView" VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pView" VkIOSSurfaceCreateInfoMVK =
             #{offset VkIOSSurfaceCreateInfoMVK, pView}
        type FieldIsArray "pView" VkIOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIOSSurfaceCreateInfoMVK, pView}

instance CanReadField "pView" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE getField #-}
        getField = vkPView

        {-# INLINE readField #-}
        readField = readVkPView

instance CanWriteField "pView" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField = writeVkPView

instance Show VkIOSSurfaceCreateInfoMVK where
        showsPrec d x
          = showString "VkIOSSurfaceCreateInfoMVK {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkPView = " . showsPrec d (vkPView x) . showChar '}'
