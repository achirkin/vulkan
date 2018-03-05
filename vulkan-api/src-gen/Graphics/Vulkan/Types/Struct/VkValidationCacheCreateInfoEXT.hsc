#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkValidationCacheCreateInfoEXT
       (VkValidationCacheCreateInfoEXT(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkValidationCacheCreateFlagsEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheCreateFlagsEXT    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkValidationCacheCreateInfoEXT.html VkValidationCacheCreateInfoEXT registry at www.khronos.org>
data VkValidationCacheCreateInfoEXT = VkValidationCacheCreateInfoEXT## Addr##
                                                                      ByteArray##

instance Eq VkValidationCacheCreateInfoEXT where
        (VkValidationCacheCreateInfoEXT## a _) ==
          x@(VkValidationCacheCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkValidationCacheCreateInfoEXT where
        (VkValidationCacheCreateInfoEXT## a _) `compare`
          x@(VkValidationCacheCreateInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkValidationCacheCreateInfoEXT where
        sizeOf ~_ = #{size VkValidationCacheCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkValidationCacheCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkValidationCacheCreateInfoEXT where
        unsafeAddr (VkValidationCacheCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkValidationCacheCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkValidationCacheCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkValidationCacheCreateInfoEXT where
        type StructFields VkValidationCacheCreateInfoEXT =
             '["sType", "pNext", "flags", "initialDataSize", "pInitialData"] -- ' closing tick for hsc2hs
        type CUnionType VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkValidationCacheCreateInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkValidationCacheCreateInfoEXT where
        type FieldType "sType" VkValidationCacheCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, sType}
        type FieldIsArray "sType" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkValidationCacheCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkValidationCacheCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkValidationCacheCreateInfoEXT where
        type FieldType "pNext" VkValidationCacheCreateInfoEXT = Ptr Void
        type FieldOptional "pNext" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkValidationCacheCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkValidationCacheCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkValidationCacheCreateInfoEXT where
        type FieldType "flags" VkValidationCacheCreateInfoEXT =
             VkValidationCacheCreateFlagsEXT
        type FieldOptional "flags" VkValidationCacheCreateInfoEXT = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, flags}
        type FieldIsArray "flags" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkValidationCacheCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkValidationCacheCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "initialDataSize" VkValidationCacheCreateInfoEXT where
        type FieldType "initialDataSize" VkValidationCacheCreateInfoEXT =
             CSize
        type FieldOptional "initialDataSize" VkValidationCacheCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "initialDataSize" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, initialDataSize}
        type FieldIsArray "initialDataSize" VkValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, initialDataSize}

instance {-# OVERLAPPING #-}
         CanReadField "initialDataSize" VkValidationCacheCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, initialDataSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, initialDataSize}

instance {-# OVERLAPPING #-}
         CanWriteField "initialDataSize" VkValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, initialDataSize}

instance {-# OVERLAPPING #-}
         HasField "pInitialData" VkValidationCacheCreateInfoEXT where
        type FieldType "pInitialData" VkValidationCacheCreateInfoEXT =
             Ptr Void
        type FieldOptional "pInitialData" VkValidationCacheCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pInitialData" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, pInitialData}
        type FieldIsArray "pInitialData" VkValidationCacheCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, pInitialData}

instance {-# OVERLAPPING #-}
         CanReadField "pInitialData" VkValidationCacheCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, pInitialData})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, pInitialData}

instance {-# OVERLAPPING #-}
         CanWriteField "pInitialData" VkValidationCacheCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, pInitialData}

instance Show VkValidationCacheCreateInfoEXT where
        showsPrec d x
          = showString "VkValidationCacheCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "initialDataSize = " .
                                  showsPrec d (getField @"initialDataSize" x) .
                                    showString ", " .
                                      showString "pInitialData = " .
                                        showsPrec d (getField @"pInitialData" x) . showChar '}'
