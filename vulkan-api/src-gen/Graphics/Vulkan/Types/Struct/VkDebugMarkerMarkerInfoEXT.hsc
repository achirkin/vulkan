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
module Graphics.Vulkan.Types.Struct.VkDebugMarkerMarkerInfoEXT
       (VkDebugMarkerMarkerInfoEXT(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             Proxy##,
                                                             byteArrayContents##,
                                                             plusAddr##, proxy##)
import           GHC.TypeLits                               (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDebugMarkerMarkerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const char* pMarkerName;
--   >     float            color[4];
--   > } VkDebugMarkerMarkerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugMarkerMarkerInfoEXT VkDebugMarkerMarkerInfoEXT registry at www.khronos.org>
data VkDebugMarkerMarkerInfoEXT = VkDebugMarkerMarkerInfoEXT## Addr##
                                                              ByteArray##

instance Eq VkDebugMarkerMarkerInfoEXT where
        (VkDebugMarkerMarkerInfoEXT## a _) ==
          x@(VkDebugMarkerMarkerInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerMarkerInfoEXT where
        (VkDebugMarkerMarkerInfoEXT## a _) `compare`
          x@(VkDebugMarkerMarkerInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerMarkerInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerMarkerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDebugMarkerMarkerInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugMarkerMarkerInfoEXT where
        unsafeAddr (VkDebugMarkerMarkerInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugMarkerMarkerInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugMarkerMarkerInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugMarkerMarkerInfoEXT where
        type StructFields VkDebugMarkerMarkerInfoEXT =
             '["sType", "pNext", "pMarkerName", "color"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugMarkerMarkerInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugMarkerMarkerInfoEXT where
        type FieldType "sType" VkDebugMarkerMarkerInfoEXT = VkStructureType
        type FieldOptional "sType" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugMarkerMarkerInfoEXT =
             #{offset VkDebugMarkerMarkerInfoEXT, sType}
        type FieldIsArray "sType" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerMarkerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugMarkerMarkerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugMarkerMarkerInfoEXT where
        type FieldType "pNext" VkDebugMarkerMarkerInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugMarkerMarkerInfoEXT =
             #{offset VkDebugMarkerMarkerInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerMarkerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugMarkerMarkerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pMarkerName" VkDebugMarkerMarkerInfoEXT where
        type FieldType "pMarkerName" VkDebugMarkerMarkerInfoEXT = CString
        type FieldOptional "pMarkerName" VkDebugMarkerMarkerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pMarkerName" VkDebugMarkerMarkerInfoEXT =
             #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}
        type FieldIsArray "pMarkerName" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

instance {-# OVERLAPPING #-}
         CanReadField "pMarkerName" VkDebugMarkerMarkerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

instance {-# OVERLAPPING #-}
         CanWriteField "pMarkerName" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

instance {-# OVERLAPPING #-}
         HasField "color" VkDebugMarkerMarkerInfoEXT where
        type FieldType "color" VkDebugMarkerMarkerInfoEXT =
             #{type float}
        type FieldOptional "color" VkDebugMarkerMarkerInfoEXT = 'True -- ' closing tick for hsc2hs
        type FieldOffset "color" VkDebugMarkerMarkerInfoEXT =
             #{offset VkDebugMarkerMarkerInfoEXT, color}
        type FieldIsArray "color" VkDebugMarkerMarkerInfoEXT = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerMarkerInfoEXT, color}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "color" idx VkDebugMarkerMarkerInfoEXT) =>
         CanReadFieldArray "color" idx VkDebugMarkerMarkerInfoEXT
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "color" 0 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 1 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 2 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 3 VkDebugMarkerMarkerInfoEXT #-}
        type FieldArrayLength "color" VkDebugMarkerMarkerInfoEXT = 4

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 4

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkDebugMarkerMarkerInfoEXT, color} +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkDebugMarkerMarkerInfoEXT, color} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "color" idx VkDebugMarkerMarkerInfoEXT) =>
         CanWriteFieldArray "color" idx VkDebugMarkerMarkerInfoEXT
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 0 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 1 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 2 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 3 VkDebugMarkerMarkerInfoEXT #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkDebugMarkerMarkerInfoEXT, color} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkDebugMarkerMarkerInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerMarkerInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pMarkerName = " .
                            showsPrec d (getField @"pMarkerName" x) .
                              showString ", " .
                                (showString "color = [" .
                                   showsPrec d
                                     (let s = sizeOf
                                                (undefined ::
                                                   FieldType "color" VkDebugMarkerMarkerInfoEXT)
                                          o = fieldOffset @"color" @VkDebugMarkerMarkerInfoEXT
                                          f i
                                            = peekByteOff (unsafePtr x) i ::
                                                IO (FieldType "color" VkDebugMarkerMarkerInfoEXT)
                                        in
                                        unsafeDupablePerformIO . mapM f $
                                          map (\ i -> o + i * s) [0 .. 4 - 1])
                                     . showChar ']')
                                  . showChar '}'
