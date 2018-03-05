#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDebugMarkerObjectNameInfoEXT
       (VkDebugMarkerObjectNameInfoEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT (VkDebugReportObjectTypeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkDebugMarkerObjectNameInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportObjectTypeEXT       objectType;
--   >     uint64_t                         object;
--   >     const char* pObjectName;
--   > } VkDebugMarkerObjectNameInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDebugMarkerObjectNameInfoEXT.html VkDebugMarkerObjectNameInfoEXT registry at www.khronos.org>
data VkDebugMarkerObjectNameInfoEXT = VkDebugMarkerObjectNameInfoEXT## Addr##
                                                                      ByteArray##

instance Eq VkDebugMarkerObjectNameInfoEXT where
        (VkDebugMarkerObjectNameInfoEXT## a _) ==
          x@(VkDebugMarkerObjectNameInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerObjectNameInfoEXT where
        (VkDebugMarkerObjectNameInfoEXT## a _) `compare`
          x@(VkDebugMarkerObjectNameInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerObjectNameInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerObjectNameInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugMarkerObjectNameInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugMarkerObjectNameInfoEXT where
        unsafeAddr (VkDebugMarkerObjectNameInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugMarkerObjectNameInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugMarkerObjectNameInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugMarkerObjectNameInfoEXT where
        type StructFields VkDebugMarkerObjectNameInfoEXT =
             '["sType", "pNext", "objectType", "object", "pObjectName"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugMarkerObjectNameInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "sType" VkDebugMarkerObjectNameInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, sType}
        type FieldIsArray "sType" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "pNext" VkDebugMarkerObjectNameInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "objectType" VkDebugMarkerObjectNameInfoEXT =
             VkDebugReportObjectTypeEXT
        type FieldOptional "objectType" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, objectType}
        type FieldIsArray "objectType" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanReadField "objectType" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, objectType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanWriteField "objectType" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "object" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "object" VkDebugMarkerObjectNameInfoEXT = Word64
        type FieldOptional "object" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "object" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, object}
        type FieldIsArray "object" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance {-# OVERLAPPING #-}
         CanReadField "object" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, object})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance {-# OVERLAPPING #-}
         CanWriteField "object" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance {-# OVERLAPPING #-}
         HasField "pObjectName" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             CString
        type FieldOptional "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}
        type FieldIsArray "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance {-# OVERLAPPING #-}
         CanReadField "pObjectName" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance {-# OVERLAPPING #-}
         CanWriteField "pObjectName" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance Show VkDebugMarkerObjectNameInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerObjectNameInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectType = " .
                            showsPrec d (getField @"objectType" x) .
                              showString ", " .
                                showString "object = " .
                                  showsPrec d (getField @"object" x) .
                                    showString ", " .
                                      showString "pObjectName = " .
                                        showsPrec d (getField @"pObjectName" x) . showChar '}'
