#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformMacosMvk
       (VkMacOSSurfaceCreateInfoMVK(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkMacOSSurfaceCreateFlagsMVK)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkMacOSSurfaceCreateInfoMVK {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkMacOSSurfaceCreateFlagsMVK   flags;
--   >     const void*                                    pView;
--   > } VkMacOSSurfaceCreateInfoMVK;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMacOSSurfaceCreateInfoMVK VkMacOSSurfaceCreateInfoMVK registry at www.khronos.org>
data VkMacOSSurfaceCreateInfoMVK = VkMacOSSurfaceCreateInfoMVK## Addr##
                                                                ByteArray##

instance Eq VkMacOSSurfaceCreateInfoMVK where
        (VkMacOSSurfaceCreateInfoMVK## a _) ==
          x@(VkMacOSSurfaceCreateInfoMVK## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMacOSSurfaceCreateInfoMVK where
        (VkMacOSSurfaceCreateInfoMVK## a _) `compare`
          x@(VkMacOSSurfaceCreateInfoMVK## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMacOSSurfaceCreateInfoMVK where
        sizeOf ~_ = #{size VkMacOSSurfaceCreateInfoMVK}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMacOSSurfaceCreateInfoMVK}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMacOSSurfaceCreateInfoMVK where
        unsafeAddr (VkMacOSSurfaceCreateInfoMVK## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMacOSSurfaceCreateInfoMVK## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMacOSSurfaceCreateInfoMVK##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMacOSSurfaceCreateInfoMVK where
        type StructFields VkMacOSSurfaceCreateInfoMVK =
             '["sType", "pNext", "flags", "pView"] -- ' closing tick for hsc2hs
        type CUnionType VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMacOSSurfaceCreateInfoMVK = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMacOSSurfaceCreateInfoMVK where
        type FieldType "sType" VkMacOSSurfaceCreateInfoMVK =
             VkStructureType
        type FieldOptional "sType" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMacOSSurfaceCreateInfoMVK =
             #{offset VkMacOSSurfaceCreateInfoMVK, sType}
        type FieldIsArray "sType" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMacOSSurfaceCreateInfoMVK, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMacOSSurfaceCreateInfoMVK where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMacOSSurfaceCreateInfoMVK where
        type FieldType "pNext" VkMacOSSurfaceCreateInfoMVK = Ptr Void
        type FieldOptional "pNext" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMacOSSurfaceCreateInfoMVK =
             #{offset VkMacOSSurfaceCreateInfoMVK, pNext}
        type FieldIsArray "pNext" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMacOSSurfaceCreateInfoMVK where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMacOSSurfaceCreateInfoMVK where
        type FieldType "flags" VkMacOSSurfaceCreateInfoMVK =
             VkMacOSSurfaceCreateFlagsMVK
        type FieldOptional "flags" VkMacOSSurfaceCreateInfoMVK = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMacOSSurfaceCreateInfoMVK =
             #{offset VkMacOSSurfaceCreateInfoMVK, flags}
        type FieldIsArray "flags" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMacOSSurfaceCreateInfoMVK, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkMacOSSurfaceCreateInfoMVK where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, flags}

instance {-# OVERLAPPING #-}
         HasField "pView" VkMacOSSurfaceCreateInfoMVK where
        type FieldType "pView" VkMacOSSurfaceCreateInfoMVK = Ptr Void
        type FieldOptional "pView" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pView" VkMacOSSurfaceCreateInfoMVK =
             #{offset VkMacOSSurfaceCreateInfoMVK, pView}
        type FieldIsArray "pView" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMacOSSurfaceCreateInfoMVK, pView}

instance {-# OVERLAPPING #-}
         CanReadField "pView" VkMacOSSurfaceCreateInfoMVK where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, pView})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pView}

instance {-# OVERLAPPING #-}
         CanWriteField "pView" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pView}

instance Show VkMacOSSurfaceCreateInfoMVK where
        showsPrec d x
          = showString "VkMacOSSurfaceCreateInfoMVK {" .
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
