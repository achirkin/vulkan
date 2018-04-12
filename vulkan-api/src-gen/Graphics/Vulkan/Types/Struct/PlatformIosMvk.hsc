#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformIosMvk
       (VkIOSSurfaceCreateInfoMVK(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkIOSSurfaceCreateFlagsMVK)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkIOSSurfaceCreateInfoMVK {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkIOSSurfaceCreateFlagsMVK     flags;
--   >     const void*                                    pView;
--   > } VkIOSSurfaceCreateInfoMVK;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIOSSurfaceCreateInfoMVK VkIOSSurfaceCreateInfoMVK registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkIOSSurfaceCreateInfoMVK where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkIOSSurfaceCreateInfoMVK where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkIOSSurfaceCreateInfoMVK where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "pView" VkIOSSurfaceCreateInfoMVK where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIOSSurfaceCreateInfoMVK, pView})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pView}

instance {-# OVERLAPPING #-}
         CanWriteField "pView" VkIOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIOSSurfaceCreateInfoMVK, pView}

instance Show VkIOSSurfaceCreateInfoMVK where
        showsPrec d x
          = showString "VkIOSSurfaceCreateInfoMVK {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "pView = " .
                                  showsPrec d (getField @"pView" x) . showChar '}'
