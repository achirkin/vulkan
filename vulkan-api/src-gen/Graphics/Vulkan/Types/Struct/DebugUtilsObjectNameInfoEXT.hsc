#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DebugUtilsObjectNameInfoEXT
       (VkDebugUtilsObjectNameInfoEXT(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Object        (VkObjectType)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkDebugUtilsObjectNameInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     VkObjectType                                           objectType;
--   >     uint64_t                                               objectHandle;
--   >     const char*      pObjectName;
--   > } VkDebugUtilsObjectNameInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsObjectNameInfoEXT VkDebugUtilsObjectNameInfoEXT registry at www.khronos.org>
data VkDebugUtilsObjectNameInfoEXT = VkDebugUtilsObjectNameInfoEXT## Addr##
                                                                    ByteArray##

instance Eq VkDebugUtilsObjectNameInfoEXT where
        (VkDebugUtilsObjectNameInfoEXT## a _) ==
          x@(VkDebugUtilsObjectNameInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugUtilsObjectNameInfoEXT where
        (VkDebugUtilsObjectNameInfoEXT## a _) `compare`
          x@(VkDebugUtilsObjectNameInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugUtilsObjectNameInfoEXT where
        sizeOf ~_ = #{size VkDebugUtilsObjectNameInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugUtilsObjectNameInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugUtilsObjectNameInfoEXT where
        unsafeAddr (VkDebugUtilsObjectNameInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugUtilsObjectNameInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugUtilsObjectNameInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugUtilsObjectNameInfoEXT where
        type StructFields VkDebugUtilsObjectNameInfoEXT =
             '["sType", "pNext", "objectType", "objectHandle", "pObjectName"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugUtilsObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugUtilsObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugUtilsObjectNameInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugUtilsObjectNameInfoEXT where
        type FieldType "sType" VkDebugUtilsObjectNameInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugUtilsObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugUtilsObjectNameInfoEXT =
             #{offset VkDebugUtilsObjectNameInfoEXT, sType}
        type FieldIsArray "sType" VkDebugUtilsObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugUtilsObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectNameInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugUtilsObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugUtilsObjectNameInfoEXT where
        type FieldType "pNext" VkDebugUtilsObjectNameInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugUtilsObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugUtilsObjectNameInfoEXT =
             #{offset VkDebugUtilsObjectNameInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugUtilsObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugUtilsObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectNameInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugUtilsObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugUtilsObjectNameInfoEXT where
        type FieldType "objectType" VkDebugUtilsObjectNameInfoEXT =
             VkObjectType
        type FieldOptional "objectType" VkDebugUtilsObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugUtilsObjectNameInfoEXT =
             #{offset VkDebugUtilsObjectNameInfoEXT, objectType}
        type FieldIsArray "objectType" VkDebugUtilsObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanReadField "objectType" VkDebugUtilsObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectNameInfoEXT, objectType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanWriteField "objectType" VkDebugUtilsObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "objectHandle" VkDebugUtilsObjectNameInfoEXT where
        type FieldType "objectHandle" VkDebugUtilsObjectNameInfoEXT =
             Word64
        type FieldOptional "objectHandle" VkDebugUtilsObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectHandle" VkDebugUtilsObjectNameInfoEXT =
             #{offset VkDebugUtilsObjectNameInfoEXT, objectHandle}
        type FieldIsArray "objectHandle" VkDebugUtilsObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectNameInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         CanReadField "objectHandle" VkDebugUtilsObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectNameInfoEXT, objectHandle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         CanWriteField "objectHandle" VkDebugUtilsObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         HasField "pObjectName" VkDebugUtilsObjectNameInfoEXT where
        type FieldType "pObjectName" VkDebugUtilsObjectNameInfoEXT =
             CString
        type FieldOptional "pObjectName" VkDebugUtilsObjectNameInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pObjectName" VkDebugUtilsObjectNameInfoEXT =
             #{offset VkDebugUtilsObjectNameInfoEXT, pObjectName}
        type FieldIsArray "pObjectName" VkDebugUtilsObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectNameInfoEXT, pObjectName}

instance {-# OVERLAPPING #-}
         CanReadField "pObjectName" VkDebugUtilsObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectNameInfoEXT, pObjectName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, pObjectName}

instance {-# OVERLAPPING #-}
         CanWriteField "pObjectName" VkDebugUtilsObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectNameInfoEXT, pObjectName}

instance Show VkDebugUtilsObjectNameInfoEXT where
        showsPrec d x
          = showString "VkDebugUtilsObjectNameInfoEXT {" .
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
                                      showString "pObjectName = " .
                                        showsPrec d (getField @"pObjectName" x) . showChar '}'
