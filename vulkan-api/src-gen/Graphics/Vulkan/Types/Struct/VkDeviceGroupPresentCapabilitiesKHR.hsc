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
module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHR
       (VkDeviceGroupPresentCapabilitiesKHR(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Base                                                    (Addr##,
                                                                              ByteArray##,
                                                                              Proxy##,
                                                                              byteArrayContents##,
                                                                              plusAddr##,
                                                                              proxy##)
import           GHC.TypeLits                                                (KnownNat,
                                                                              natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                                   (VK_MAX_DEVICE_GROUP_SIZE,
                                                                              pattern VK_MAX_DEVICE_GROUP_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHR (VkDeviceGroupPresentModeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupPresentCapabilitiesKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         presentMask[VK_MAX_DEVICE_GROUP_SIZE];
--   >     VkDeviceGroupPresentModeFlagsKHR modes;
--   > } VkDeviceGroupPresentCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDeviceGroupPresentCapabilitiesKHRVkDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
data VkDeviceGroupPresentCapabilitiesKHR = VkDeviceGroupPresentCapabilitiesKHR## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupPresentCapabilitiesKHR where
        (VkDeviceGroupPresentCapabilitiesKHR## a _) ==
          x@(VkDeviceGroupPresentCapabilitiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentCapabilitiesKHR where
        (VkDeviceGroupPresentCapabilitiesKHR## a _) `compare`
          x@(VkDeviceGroupPresentCapabilitiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentCapabilitiesKHR where
        sizeOf ~_ = #{size VkDeviceGroupPresentCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupPresentCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentCapabilitiesKHR
         where
        unsafeAddr (VkDeviceGroupPresentCapabilitiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentCapabilitiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentCapabilitiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentCapabilitiesKHR where
        type StructFields VkDeviceGroupPresentCapabilitiesKHR =
             '["sType", "pNext", "presentMask", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentCapabilitiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentCapabilitiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentCapabilitiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentCapabilitiesKHR where
        type FieldType "sType" VkDeviceGroupPresentCapabilitiesKHR =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentCapabilitiesKHR =
             #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupPresentCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupPresentCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentCapabilitiesKHR where
        type FieldType "pNext" VkDeviceGroupPresentCapabilitiesKHR =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentCapabilitiesKHR =
             #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupPresentCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupPresentCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "presentMask" VkDeviceGroupPresentCapabilitiesKHR where
        type FieldType "presentMask" VkDeviceGroupPresentCapabilitiesKHR =
             Word32
        type FieldOptional "presentMask"
               VkDeviceGroupPresentCapabilitiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "presentMask" VkDeviceGroupPresentCapabilitiesKHR
             =
             #{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
        type FieldIsArray "presentMask" VkDeviceGroupPresentCapabilitiesKHR
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHR) =>
         CanReadFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHR
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}
        type FieldArrayLength "presentMask"
               VkDeviceGroupPresentCapabilitiesKHR
             = VK_MAX_DEVICE_GROUP_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DEVICE_GROUP_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
                      +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHR) =>
         CanWriteFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHR
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupPresentCapabilitiesKHR where
        type FieldType "modes" VkDeviceGroupPresentCapabilitiesKHR =
             VkDeviceGroupPresentModeFlagsKHR
        type FieldOptional "modes" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupPresentCapabilitiesKHR =
             #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}
        type FieldIsArray "modes" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}

instance {-# OVERLAPPING #-}
         CanReadField "modes" VkDeviceGroupPresentCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHR, modes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}

instance {-# OVERLAPPING #-}
         CanWriteField "modes" VkDeviceGroupPresentCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}

instance Show VkDeviceGroupPresentCapabilitiesKHR where
        showsPrec d x
          = showString "VkDeviceGroupPresentCapabilitiesKHR {" .
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
                                               VkDeviceGroupPresentCapabilitiesKHR)
                                    o = fieldOffset @"presentMask"
                                          @VkDeviceGroupPresentCapabilitiesKHR
                                    f i
                                      = peekByteOff (unsafePtr x) i ::
                                          IO
                                            (FieldType "presentMask"
                                               VkDeviceGroupPresentCapabilitiesKHR)
                                  in
                                  unsafeDupablePerformIO . mapM f $
                                    map (\ i -> o + i * s) [0 .. VK_MAX_DEVICE_GROUP_SIZE - 1])
                               . showChar ']')
                            .
                            showString ", " .
                              showString "modes = " .
                                showsPrec d (getField @"modes" x) . showChar '}'
