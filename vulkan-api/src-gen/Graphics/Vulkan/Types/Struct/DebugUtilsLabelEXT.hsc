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
module Graphics.Vulkan.Types.Struct.DebugUtilsLabelEXT
       (VkDebugUtilsLabelEXT(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           Proxy##,
                                                           byteArrayContents##,
                                                           plusAddr##, proxy##)
import           GHC.TypeLits                             (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkDebugUtilsLabelEXT {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     const char*      pLabelName;
--   >     float                  color[4];
--   > } VkDebugUtilsLabelEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsLabelEXT VkDebugUtilsLabelEXT registry at www.khronos.org>
data VkDebugUtilsLabelEXT = VkDebugUtilsLabelEXT## Addr## ByteArray##

instance Eq VkDebugUtilsLabelEXT where
        (VkDebugUtilsLabelEXT## a _) == x@(VkDebugUtilsLabelEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugUtilsLabelEXT where
        (VkDebugUtilsLabelEXT## a _) `compare` x@(VkDebugUtilsLabelEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugUtilsLabelEXT where
        sizeOf ~_ = #{size VkDebugUtilsLabelEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDebugUtilsLabelEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugUtilsLabelEXT where
        unsafeAddr (VkDebugUtilsLabelEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugUtilsLabelEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugUtilsLabelEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugUtilsLabelEXT where
        type StructFields VkDebugUtilsLabelEXT =
             '["sType", "pNext", "pLabelName", "color"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugUtilsLabelEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugUtilsLabelEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugUtilsLabelEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDebugUtilsLabelEXT
         where
        type FieldType "sType" VkDebugUtilsLabelEXT = VkStructureType
        type FieldOptional "sType" VkDebugUtilsLabelEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugUtilsLabelEXT =
             #{offset VkDebugUtilsLabelEXT, sType}
        type FieldIsArray "sType" VkDebugUtilsLabelEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDebugUtilsLabelEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugUtilsLabelEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsLabelEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsLabelEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugUtilsLabelEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsLabelEXT, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDebugUtilsLabelEXT
         where
        type FieldType "pNext" VkDebugUtilsLabelEXT = Ptr Void
        type FieldOptional "pNext" VkDebugUtilsLabelEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugUtilsLabelEXT =
             #{offset VkDebugUtilsLabelEXT, pNext}
        type FieldIsArray "pNext" VkDebugUtilsLabelEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDebugUtilsLabelEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugUtilsLabelEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsLabelEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsLabelEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugUtilsLabelEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsLabelEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pLabelName" VkDebugUtilsLabelEXT where
        type FieldType "pLabelName" VkDebugUtilsLabelEXT = CString
        type FieldOptional "pLabelName" VkDebugUtilsLabelEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pLabelName" VkDebugUtilsLabelEXT =
             #{offset VkDebugUtilsLabelEXT, pLabelName}
        type FieldIsArray "pLabelName" VkDebugUtilsLabelEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsLabelEXT, pLabelName}

instance {-# OVERLAPPING #-}
         CanReadField "pLabelName" VkDebugUtilsLabelEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsLabelEXT, pLabelName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsLabelEXT, pLabelName}

instance {-# OVERLAPPING #-}
         CanWriteField "pLabelName" VkDebugUtilsLabelEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsLabelEXT, pLabelName}

instance {-# OVERLAPPING #-} HasField "color" VkDebugUtilsLabelEXT
         where
        type FieldType "color" VkDebugUtilsLabelEXT =
             #{type float}
        type FieldOptional "color" VkDebugUtilsLabelEXT = 'True -- ' closing tick for hsc2hs
        type FieldOffset "color" VkDebugUtilsLabelEXT =
             #{offset VkDebugUtilsLabelEXT, color}
        type FieldIsArray "color" VkDebugUtilsLabelEXT = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDebugUtilsLabelEXT, color}

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "color" idx VkDebugUtilsLabelEXT) =>
         CanReadFieldArray "color" idx VkDebugUtilsLabelEXT
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "color" 0 VkDebugUtilsLabelEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 1 VkDebugUtilsLabelEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 2 VkDebugUtilsLabelEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 3 VkDebugUtilsLabelEXT #-}
        type FieldArrayLength "color" VkDebugUtilsLabelEXT = 4

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 4

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkDebugUtilsLabelEXT, color} +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkDebugUtilsLabelEXT, color} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "color" idx VkDebugUtilsLabelEXT) =>
         CanWriteFieldArray "color" idx VkDebugUtilsLabelEXT
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 0 VkDebugUtilsLabelEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 1 VkDebugUtilsLabelEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 2 VkDebugUtilsLabelEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 3 VkDebugUtilsLabelEXT #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkDebugUtilsLabelEXT, color} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkDebugUtilsLabelEXT where
        showsPrec d x
          = showString "VkDebugUtilsLabelEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pLabelName = " .
                            showsPrec d (getField @"pLabelName" x) .
                              showString ", " .
                                (showString "color = [" .
                                   showsPrec d
                                     (let s = sizeOf
                                                (undefined ::
                                                   FieldType "color" VkDebugUtilsLabelEXT)
                                          o = fieldOffset @"color" @VkDebugUtilsLabelEXT
                                          f i
                                            = peekByteOff (unsafePtr x) i ::
                                                IO (FieldType "color" VkDebugUtilsLabelEXT)
                                        in
                                        unsafeDupablePerformIO . mapM f $
                                          map (\ i -> o + i * s) [0 .. 4 - 1])
                                     . showChar ']')
                                  . showChar '}'
