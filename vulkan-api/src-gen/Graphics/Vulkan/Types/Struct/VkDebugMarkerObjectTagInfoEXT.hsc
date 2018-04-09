#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDebugMarkerObjectTagInfoEXT
       (VkDebugMarkerObjectTagInfoEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT (VkDebugReportObjectTypeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkDebugMarkerObjectTagInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportObjectTypeEXT       objectType;
--   >     uint64_t                         object;
--   >     uint64_t                         tagName;
--   >     size_t                           tagSize;
--   >     const void*        pTag;
--   > } VkDebugMarkerObjectTagInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDebugMarkerObjectTagInfoEXTVkDebugMarkerObjectTagInfoEXT registry at www.khronos.org>
data VkDebugMarkerObjectTagInfoEXT = VkDebugMarkerObjectTagInfoEXT## Addr##
                                                                    ByteArray##

instance Eq VkDebugMarkerObjectTagInfoEXT where
        (VkDebugMarkerObjectTagInfoEXT## a _) ==
          x@(VkDebugMarkerObjectTagInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerObjectTagInfoEXT where
        (VkDebugMarkerObjectTagInfoEXT## a _) `compare`
          x@(VkDebugMarkerObjectTagInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerObjectTagInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerObjectTagInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugMarkerObjectTagInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugMarkerObjectTagInfoEXT where
        unsafeAddr (VkDebugMarkerObjectTagInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugMarkerObjectTagInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugMarkerObjectTagInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugMarkerObjectTagInfoEXT where
        type StructFields VkDebugMarkerObjectTagInfoEXT =
             '["sType", "pNext", "objectType", "object", "tagName", "tagSize", -- ' closing tick for hsc2hs
               "pTag"]
        type CUnionType VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugMarkerObjectTagInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "sType" VkDebugMarkerObjectTagInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, sType}
        type FieldIsArray "sType" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "pNext" VkDebugMarkerObjectTagInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "objectType" VkDebugMarkerObjectTagInfoEXT =
             VkDebugReportObjectTypeEXT
        type FieldOptional "objectType" VkDebugMarkerObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, objectType}
        type FieldIsArray "objectType" VkDebugMarkerObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanReadField "objectType" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, objectType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanWriteField "objectType" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "object" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "object" VkDebugMarkerObjectTagInfoEXT = Word64
        type FieldOptional "object" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "object" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, object}
        type FieldIsArray "object" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance {-# OVERLAPPING #-}
         CanReadField "object" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, object})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance {-# OVERLAPPING #-}
         CanWriteField "object" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance {-# OVERLAPPING #-}
         HasField "tagName" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "tagName" VkDebugMarkerObjectTagInfoEXT = Word64
        type FieldOptional "tagName" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagName" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, tagName}
        type FieldIsArray "tagName" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         CanReadField "tagName" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, tagName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         CanWriteField "tagName" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         HasField "tagSize" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "tagSize" VkDebugMarkerObjectTagInfoEXT = CSize
        type FieldOptional "tagSize" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagSize" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}
        type FieldIsArray "tagSize" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         CanReadField "tagSize" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, tagSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         CanWriteField "tagSize" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         HasField "pTag" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "pTag" VkDebugMarkerObjectTagInfoEXT = Ptr Void
        type FieldOptional "pTag" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pTag" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, pTag}
        type FieldIsArray "pTag" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         CanReadField "pTag" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, pTag})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         CanWriteField "pTag" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance Show VkDebugMarkerObjectTagInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerObjectTagInfoEXT {" .
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
                                      showString "tagName = " .
                                        showsPrec d (getField @"tagName" x) .
                                          showString ", " .
                                            showString "tagSize = " .
                                              showsPrec d (getField @"tagSize" x) .
                                                showString ", " .
                                                  showString "pTag = " .
                                                    showsPrec d (getField @"pTag" x) . showChar '}'
