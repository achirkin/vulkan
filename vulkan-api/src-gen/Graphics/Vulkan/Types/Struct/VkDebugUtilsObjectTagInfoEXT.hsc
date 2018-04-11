#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDebugUtilsObjectTagInfoEXT
       (VkDebugUtilsObjectTagInfoEXT(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkObjectType    (VkObjectType)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDebugUtilsObjectTagInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkObjectType                           objectType;
--   >     uint64_t                               objectHandle;
--   >     uint64_t                               tagName;
--   >     size_t                                 tagSize;
--   >     const void*              pTag;
--   > } VkDebugUtilsObjectTagInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXT registry at www.khronos.org>
data VkDebugUtilsObjectTagInfoEXT = VkDebugUtilsObjectTagInfoEXT## Addr##
                                                                  ByteArray##

instance Eq VkDebugUtilsObjectTagInfoEXT where
        (VkDebugUtilsObjectTagInfoEXT## a _) ==
          x@(VkDebugUtilsObjectTagInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugUtilsObjectTagInfoEXT where
        (VkDebugUtilsObjectTagInfoEXT## a _) `compare`
          x@(VkDebugUtilsObjectTagInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugUtilsObjectTagInfoEXT where
        sizeOf ~_ = #{size VkDebugUtilsObjectTagInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugUtilsObjectTagInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugUtilsObjectTagInfoEXT where
        unsafeAddr (VkDebugUtilsObjectTagInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugUtilsObjectTagInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugUtilsObjectTagInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugUtilsObjectTagInfoEXT where
        type StructFields VkDebugUtilsObjectTagInfoEXT =
             '["sType", "pNext", "objectType", "objectHandle", "tagName", -- ' closing tick for hsc2hs
               "tagSize", "pTag"]
        type CUnionType VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugUtilsObjectTagInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "sType" VkDebugUtilsObjectTagInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, sType}
        type FieldIsArray "sType" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "pNext" VkDebugUtilsObjectTagInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "objectType" VkDebugUtilsObjectTagInfoEXT =
             VkObjectType
        type FieldOptional "objectType" VkDebugUtilsObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, objectType}
        type FieldIsArray "objectType" VkDebugUtilsObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanReadField "objectType" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, objectType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanWriteField "objectType" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "objectHandle" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "objectHandle" VkDebugUtilsObjectTagInfoEXT = Word64
        type FieldOptional "objectHandle" VkDebugUtilsObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectHandle" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}
        type FieldIsArray "objectHandle" VkDebugUtilsObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         CanReadField "objectHandle" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         CanWriteField "objectHandle" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         HasField "tagName" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "tagName" VkDebugUtilsObjectTagInfoEXT = Word64
        type FieldOptional "tagName" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagName" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, tagName}
        type FieldIsArray "tagName" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         CanReadField "tagName" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, tagName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         CanWriteField "tagName" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         HasField "tagSize" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "tagSize" VkDebugUtilsObjectTagInfoEXT = CSize
        type FieldOptional "tagSize" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagSize" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}
        type FieldIsArray "tagSize" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         CanReadField "tagSize" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, tagSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         CanWriteField "tagSize" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         HasField "pTag" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "pTag" VkDebugUtilsObjectTagInfoEXT = Ptr Void
        type FieldOptional "pTag" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pTag" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, pTag}
        type FieldIsArray "pTag" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         CanReadField "pTag" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, pTag})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         CanWriteField "pTag" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, pTag}

instance Show VkDebugUtilsObjectTagInfoEXT where
        showsPrec d x
          = showString "VkDebugUtilsObjectTagInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectType = " .
                            showsPrec d (getField @"objectType" x) .
                              showString ", " .
                                showString "objectHandle = " .
                                  showsPrec d (getField @"objectHandle" x) .
                                    showString ", " .
                                      showString "tagName = " .
                                        showsPrec d (getField @"tagName" x) .
                                          showString ", " .
                                            showString "tagSize = " .
                                              showsPrec d (getField @"tagSize" x) .
                                                showString ", " .
                                                  showString "pTag = " .
                                                    showsPrec d (getField @"pTag" x) . showChar '}'
