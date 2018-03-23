#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayPowerInfoEXT
       (VkDisplayPowerInfoEXT(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDisplayPowerStateEXT (VkDisplayPowerStateEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayPowerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayPowerStateEXT           powerState;
--   > } VkDisplayPowerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDisplayPowerInfoEXT.html VkDisplayPowerInfoEXT registry at www.khronos.org>
data VkDisplayPowerInfoEXT = VkDisplayPowerInfoEXT## Addr##
                                                    ByteArray##

instance Eq VkDisplayPowerInfoEXT where
        (VkDisplayPowerInfoEXT## a _) == x@(VkDisplayPowerInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPowerInfoEXT where
        (VkDisplayPowerInfoEXT## a _) `compare`
          x@(VkDisplayPowerInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPowerInfoEXT where
        sizeOf ~_ = #{size VkDisplayPowerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPowerInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPowerInfoEXT where
        unsafeAddr (VkDisplayPowerInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPowerInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPowerInfoEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPowerInfoEXT where
        type StructFields VkDisplayPowerInfoEXT =
             '["sType", "pNext", "powerState"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPowerInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDisplayPowerInfoEXT
         where
        type FieldType "sType" VkDisplayPowerInfoEXT = VkStructureType
        type FieldOptional "sType" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayPowerInfoEXT =
             #{offset VkDisplayPowerInfoEXT, sType}
        type FieldIsArray "sType" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPowerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplayPowerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDisplayPowerInfoEXT
         where
        type FieldType "pNext" VkDisplayPowerInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayPowerInfoEXT =
             #{offset VkDisplayPowerInfoEXT, pNext}
        type FieldIsArray "pNext" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPowerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplayPowerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "powerState" VkDisplayPowerInfoEXT where
        type FieldType "powerState" VkDisplayPowerInfoEXT =
             VkDisplayPowerStateEXT
        type FieldOptional "powerState" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "powerState" VkDisplayPowerInfoEXT =
             #{offset VkDisplayPowerInfoEXT, powerState}
        type FieldIsArray "powerState" VkDisplayPowerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPowerInfoEXT, powerState}

instance {-# OVERLAPPING #-}
         CanReadField "powerState" VkDisplayPowerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPowerInfoEXT, powerState})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplayPowerInfoEXT, powerState}

instance {-# OVERLAPPING #-}
         CanWriteField "powerState" VkDisplayPowerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplayPowerInfoEXT, powerState}

instance Show VkDisplayPowerInfoEXT where
        showsPrec d x
          = showString "VkDisplayPowerInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "powerState = " .
                            showsPrec d (getField @"powerState" x) . showChar '}'
