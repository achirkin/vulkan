#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHX
       (VkDeviceGroupPresentCapabilitiesKHX(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                                (KnownNat,
                                                                              natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                                   (VK_MAX_DEVICE_GROUP_SIZE_KHX,
                                                                              pattern VK_MAX_DEVICE_GROUP_SIZE_KHX)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHX (VkDeviceGroupPresentModeFlagsKHX)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupPresentCapabilitiesKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         presentMask[VK_MAX_DEVICE_GROUP_SIZE_KHX];
--   >     VkDeviceGroupPresentModeFlagsKHX modes;
--   > } VkDeviceGroupPresentCapabilitiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupPresentCapabilitiesKHX.html VkDeviceGroupPresentCapabilitiesKHX registry at www.khronos.org>
data VkDeviceGroupPresentCapabilitiesKHX = VkDeviceGroupPresentCapabilitiesKHX## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupPresentCapabilitiesKHX where
        (VkDeviceGroupPresentCapabilitiesKHX## a _) ==
          x@(VkDeviceGroupPresentCapabilitiesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentCapabilitiesKHX where
        (VkDeviceGroupPresentCapabilitiesKHX## a _) `compare`
          x@(VkDeviceGroupPresentCapabilitiesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentCapabilitiesKHX where
        sizeOf ~_ = #{size VkDeviceGroupPresentCapabilitiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupPresentCapabilitiesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentCapabilitiesKHX
         where
        unsafeAddr (VkDeviceGroupPresentCapabilitiesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentCapabilitiesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentCapabilitiesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentCapabilitiesKHX where
        type StructFields VkDeviceGroupPresentCapabilitiesKHX =
             '["sType", "pNext", "presentMask", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentCapabilitiesKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentCapabilitiesKHX = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentCapabilitiesKHX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "sType" VkDeviceGroupPresentCapabilitiesKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupPresentCapabilitiesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupPresentCapabilitiesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupPresentCapabilitiesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupPresentCapabilitiesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "presentMask" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "presentMask" VkDeviceGroupPresentCapabilitiesKHX =
             Word32
        type FieldOptional "presentMask"
               VkDeviceGroupPresentCapabilitiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "presentMask" VkDeviceGroupPresentCapabilitiesKHX
             =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}
        type FieldIsArray "presentMask" VkDeviceGroupPresentCapabilitiesKHX
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHX) =>
         CanReadFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}
        type FieldArrayLength "presentMask"
               VkDeviceGroupPresentCapabilitiesKHX
             = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}
                      +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHX) =>
         CanWriteFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "modes" VkDeviceGroupPresentCapabilitiesKHX =
             VkDeviceGroupPresentModeFlagsKHX
        type FieldOptional "modes" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}
        type FieldIsArray "modes" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

instance {-# OVERLAPPING #-}
         CanReadField "modes" VkDeviceGroupPresentCapabilitiesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, modes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

instance {-# OVERLAPPING #-}
         CanWriteField "modes" VkDeviceGroupPresentCapabilitiesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

instance Show VkDeviceGroupPresentCapabilitiesKHX where
        showsPrec d x
          = showString "VkDeviceGroupPresentCapabilitiesKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          (showString "presentMask = [" .
                             showsPrec d
                               (let s = sizeOf
                                          (undefined ::
                                             FieldType "presentMask"
                                               VkDeviceGroupPresentCapabilitiesKHX)
                                    o = fieldOffset @"presentMask"
                                          @VkDeviceGroupPresentCapabilitiesKHX
                                    f i
                                      = peekByteOff (unsafePtr x) i ::
                                          IO
                                            (FieldType "presentMask"
                                               VkDeviceGroupPresentCapabilitiesKHX)
                                  in
                                  unsafeDupablePerformIO . mapM f $
                                    map (\ i -> o + i * s) [0 .. VK_MAX_DEVICE_GROUP_SIZE_KHX - 1])
                               . showChar ']')
                            .
                            showString ", " .
                              showString "modes = " .
                                showsPrec d (getField @"modes" x) . showChar '}'
