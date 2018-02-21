#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkDebugMarkerMarkerInfoEXT
       (VkDebugMarkerMarkerInfoEXT(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                               (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDebugMarkerMarkerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const char* pMarkerName;
--   >     float            color[4];
--   > } VkDebugMarkerMarkerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDebugMarkerMarkerInfoEXT.html VkDebugMarkerMarkerInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkDebugMarkerMarkerInfoEXT
         where
        type VkSTypeMType VkDebugMarkerMarkerInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDebugMarkerMarkerInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, sType}

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

instance CanReadField "sType" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDebugMarkerMarkerInfoEXT
         where
        type VkPNextMType VkDebugMarkerMarkerInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDebugMarkerMarkerInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pNext}

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

instance CanReadField "pNext" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPMarkerName VkDebugMarkerMarkerInfoEXT where
        type VkPMarkerNameMType VkDebugMarkerMarkerInfoEXT = CString

        {-# NOINLINE vkPMarkerName #-}
        vkPMarkerName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName})

        {-# INLINE vkPMarkerNameByteOffset #-}
        vkPMarkerNameByteOffset ~_
          = #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

        {-# INLINE readVkPMarkerName #-}
        readVkPMarkerName p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

        {-# INLINE writeVkPMarkerName #-}
        writeVkPMarkerName p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

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

instance CanReadField "pMarkerName" VkDebugMarkerMarkerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPMarkerName

        {-# INLINE readField #-}
        readField = readVkPMarkerName

instance CanWriteField "pMarkerName" VkDebugMarkerMarkerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPMarkerName

instance {-# OVERLAPPING #-}
         HasVkColorArray VkDebugMarkerMarkerInfoEXT where
        type VkColorArrayMType VkDebugMarkerMarkerInfoEXT =
             #{type float}

        {-# NOINLINE vkColorArray #-}
        vkColorArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkDebugMarkerMarkerInfoEXT, color}))

        {-# INLINE vkColorArrayByteOffset #-}
        vkColorArrayByteOffset ~_
          = #{offset VkDebugMarkerMarkerInfoEXT, color}

        {-# INLINE readVkColorArray #-}
        readVkColorArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkDebugMarkerMarkerInfoEXT, color})

        {-# INLINE writeVkColorArray #-}
        writeVkColorArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkDebugMarkerMarkerInfoEXT, color})

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

instance (KnownNat idx,
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
        getFieldArray x
          = vkColorArray x (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkColorArray x (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
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
        writeFieldArray x
          = writeVkColorArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance Show VkDebugMarkerMarkerInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerMarkerInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPMarkerName = " .
                            showsPrec d (vkPMarkerName x) .
                              showString ", " .
                                showString "vkColorArray = [" .
                                  showsPrec d (map (vkColorArray x) [1 .. 4]) .
                                    showChar ']' . showChar '}'
