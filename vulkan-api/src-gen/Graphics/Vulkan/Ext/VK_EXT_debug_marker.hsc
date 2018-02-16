#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_debug_marker
       (-- * Vulkan extension: @VK_EXT_debug_marker@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @baldurk@baldurk.org@
        --
        -- author: @Baldur Karlsson@
        --
        -- type: @device@
        --
        -- Extension number: @23@
        --
        -- Required extensions: 'VK_EXT_debug_report'.
        --

        -- ** Required extensions: 'VK_EXT_debug_report'.
        VkDebugMarkerObjectNameInfoEXT(..),
        VkDebugMarkerObjectTagInfoEXT(..), VkDebugMarkerMarkerInfoEXT(..),
        vkDebugMarkerSetObjectTagEXT, vkDebugMarkerSetObjectNameEXT,
        vkCmdDebugMarkerBeginEXT, vkCmdDebugMarkerEndEXT,
        vkCmdDebugMarkerInsertEXT, VK_EXT_DEBUG_MARKER_SPEC_VERSION,
        pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION,
        VK_EXT_DEBUG_MARKER_EXTENSION_NAME,
        pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.TypeLits                     (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDebugMarkerObjectNameInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportObjectTypeEXT       objectType;
--   >     uint64_t                         object;
--   >     const char* pObjectName;
--   > } VkDebugMarkerObjectNameInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDebugMarkerObjectNameInfoEXT.html VkDebugMarkerObjectNameInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         HasVkSType VkDebugMarkerObjectNameInfoEXT where
        type VkSTypeMType VkDebugMarkerObjectNameInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "sType" VkDebugMarkerObjectNameInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance CanReadField "sType" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDebugMarkerObjectNameInfoEXT where
        type VkPNextMType VkDebugMarkerObjectNameInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "pNext" VkDebugMarkerObjectNameInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance CanReadField "pNext" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkObjectType VkDebugMarkerObjectNameInfoEXT where
        type VkObjectTypeMType VkDebugMarkerObjectNameInfoEXT =
             VkDebugReportObjectTypeEXT

        {-# NOINLINE vkObjectType #-}
        vkObjectType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, objectType})

        {-# INLINE vkObjectTypeByteOffset #-}
        vkObjectTypeByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

        {-# INLINE readVkObjectType #-}
        readVkObjectType p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

        {-# INLINE writeVkObjectType #-}
        writeVkObjectType p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "objectType" VkDebugMarkerObjectNameInfoEXT =
             VkDebugReportObjectTypeEXT
        type FieldOptional "objectType" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance CanReadField "objectType" VkDebugMarkerObjectNameInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkObjectType

        {-# INLINE readField #-}
        readField = readVkObjectType

instance CanWriteField "objectType" VkDebugMarkerObjectNameInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkObjectType

instance {-# OVERLAPPING #-}
         HasVkObject VkDebugMarkerObjectNameInfoEXT where
        type VkObjectMType VkDebugMarkerObjectNameInfoEXT = Word64

        {-# NOINLINE vkObject #-}
        vkObject x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, object})

        {-# INLINE vkObjectByteOffset #-}
        vkObjectByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, object}

        {-# INLINE readVkObject #-}
        readVkObject p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, object}

        {-# INLINE writeVkObject #-}
        writeVkObject p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance {-# OVERLAPPING #-}
         HasField "object" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "object" VkDebugMarkerObjectNameInfoEXT = Word64
        type FieldOptional "object" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "object" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, object}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance CanReadField "object" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE getField #-}
        getField = vkObject

        {-# INLINE readField #-}
        readField = readVkObject

instance CanWriteField "object" VkDebugMarkerObjectNameInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkObject

instance {-# OVERLAPPING #-}
         HasVkPObjectName VkDebugMarkerObjectNameInfoEXT where
        type VkPObjectNameMType VkDebugMarkerObjectNameInfoEXT = CString

        {-# NOINLINE vkPObjectName #-}
        vkPObjectName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName})

        {-# INLINE vkPObjectNameByteOffset #-}
        vkPObjectNameByteOffset ~_
          = #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

        {-# INLINE readVkPObjectName #-}
        readVkPObjectName p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

        {-# INLINE writeVkPObjectName #-}
        writeVkPObjectName p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance {-# OVERLAPPING #-}
         HasField "pObjectName" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             CString
        type FieldOptional "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance CanReadField "pObjectName" VkDebugMarkerObjectNameInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPObjectName

        {-# INLINE readField #-}
        readField = readVkPObjectName

instance CanWriteField "pObjectName" VkDebugMarkerObjectNameInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPObjectName

instance Show VkDebugMarkerObjectNameInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerObjectNameInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkObjectType = " .
                            showsPrec d (vkObjectType x) .
                              showString ", " .
                                showString "vkObject = " .
                                  showsPrec d (vkObject x) .
                                    showString ", " .
                                      showString "vkPObjectName = " .
                                        showsPrec d (vkPObjectName x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDebugMarkerObjectTagInfoEXT.html VkDebugMarkerObjectTagInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         HasVkSType VkDebugMarkerObjectTagInfoEXT where
        type VkSTypeMType VkDebugMarkerObjectTagInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "sType" VkDebugMarkerObjectTagInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance CanReadField "sType" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDebugMarkerObjectTagInfoEXT where
        type VkPNextMType VkDebugMarkerObjectTagInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "pNext" VkDebugMarkerObjectTagInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance CanReadField "pNext" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkObjectType VkDebugMarkerObjectTagInfoEXT where
        type VkObjectTypeMType VkDebugMarkerObjectTagInfoEXT =
             VkDebugReportObjectTypeEXT

        {-# NOINLINE vkObjectType #-}
        vkObjectType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, objectType})

        {-# INLINE vkObjectTypeByteOffset #-}
        vkObjectTypeByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

        {-# INLINE readVkObjectType #-}
        readVkObjectType p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

        {-# INLINE writeVkObjectType #-}
        writeVkObjectType p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "objectType" VkDebugMarkerObjectTagInfoEXT =
             VkDebugReportObjectTypeEXT
        type FieldOptional "objectType" VkDebugMarkerObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance CanReadField "objectType" VkDebugMarkerObjectTagInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkObjectType

        {-# INLINE readField #-}
        readField = readVkObjectType

instance CanWriteField "objectType" VkDebugMarkerObjectTagInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkObjectType

instance {-# OVERLAPPING #-}
         HasVkObject VkDebugMarkerObjectTagInfoEXT where
        type VkObjectMType VkDebugMarkerObjectTagInfoEXT = Word64

        {-# NOINLINE vkObject #-}
        vkObject x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, object})

        {-# INLINE vkObjectByteOffset #-}
        vkObjectByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, object}

        {-# INLINE readVkObject #-}
        readVkObject p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, object}

        {-# INLINE writeVkObject #-}
        writeVkObject p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance {-# OVERLAPPING #-}
         HasField "object" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "object" VkDebugMarkerObjectTagInfoEXT = Word64
        type FieldOptional "object" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "object" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, object}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance CanReadField "object" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE getField #-}
        getField = vkObject

        {-# INLINE readField #-}
        readField = readVkObject

instance CanWriteField "object" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkObject

instance {-# OVERLAPPING #-}
         HasVkTagName VkDebugMarkerObjectTagInfoEXT where
        type VkTagNameMType VkDebugMarkerObjectTagInfoEXT = Word64

        {-# NOINLINE vkTagName #-}
        vkTagName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, tagName})

        {-# INLINE vkTagNameByteOffset #-}
        vkTagNameByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

        {-# INLINE readVkTagName #-}
        readVkTagName p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

        {-# INLINE writeVkTagName #-}
        writeVkTagName p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         HasField "tagName" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "tagName" VkDebugMarkerObjectTagInfoEXT = Word64
        type FieldOptional "tagName" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagName" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance CanReadField "tagName" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE getField #-}
        getField = vkTagName

        {-# INLINE readField #-}
        readField = readVkTagName

instance CanWriteField "tagName" VkDebugMarkerObjectTagInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkTagName

instance {-# OVERLAPPING #-}
         HasVkTagSize VkDebugMarkerObjectTagInfoEXT where
        type VkTagSizeMType VkDebugMarkerObjectTagInfoEXT =
             #{type size_t}

        {-# NOINLINE vkTagSize #-}
        vkTagSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, tagSize})

        {-# INLINE vkTagSizeByteOffset #-}
        vkTagSizeByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

        {-# INLINE readVkTagSize #-}
        readVkTagSize p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

        {-# INLINE writeVkTagSize #-}
        writeVkTagSize p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         HasField "tagSize" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "tagSize" VkDebugMarkerObjectTagInfoEXT =
             #{type size_t}
        type FieldOptional "tagSize" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagSize" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance CanReadField "tagSize" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE getField #-}
        getField = vkTagSize

        {-# INLINE readField #-}
        readField = readVkTagSize

instance CanWriteField "tagSize" VkDebugMarkerObjectTagInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkTagSize

instance {-# OVERLAPPING #-}
         HasVkPTag VkDebugMarkerObjectTagInfoEXT where
        type VkPTagMType VkDebugMarkerObjectTagInfoEXT = Ptr Void

        {-# NOINLINE vkPTag #-}
        vkPTag x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, pTag})

        {-# INLINE vkPTagByteOffset #-}
        vkPTagByteOffset ~_
          = #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

        {-# INLINE readVkPTag #-}
        readVkPTag p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

        {-# INLINE writeVkPTag #-}
        writeVkPTag p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         HasField "pTag" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "pTag" VkDebugMarkerObjectTagInfoEXT = Ptr Void
        type FieldOptional "pTag" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pTag" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance CanReadField "pTag" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE getField #-}
        getField = vkPTag

        {-# INLINE readField #-}
        readField = readVkPTag

instance CanWriteField "pTag" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPTag

instance Show VkDebugMarkerObjectTagInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerObjectTagInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkObjectType = " .
                            showsPrec d (vkObjectType x) .
                              showString ", " .
                                showString "vkObject = " .
                                  showsPrec d (vkObject x) .
                                    showString ", " .
                                      showString "vkTagName = " .
                                        showsPrec d (vkTagName x) .
                                          showString ", " .
                                            showString "vkTagSize = " .
                                              showsPrec d (vkTagSize x) .
                                                showString ", " .
                                                  showString "vkPTag = " .
                                                    showsPrec d (vkPTag x) . showChar '}'

-- | > typedef struct VkDebugMarkerMarkerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const char* pMarkerName;
--   >     float            color[4];
--   > } VkDebugMarkerMarkerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDebugMarkerMarkerInfoEXT.html VkDebugMarkerMarkerInfoEXT registry at www.khronos.org>
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

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectTagEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectTagInfoEXT* pTagInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDebugMarkerSetObjectTagEXT.html vkDebugMarkerSetObjectTagEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugMarkerSetObjectTagEXT"
               vkDebugMarkerSetObjectTagEXT ::
               VkDevice -- ^ device
                        -> Ptr VkDebugMarkerObjectTagInfoEXT -- ^ pTagInfo
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectNameEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectNameInfoEXT* pNameInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDebugMarkerSetObjectNameEXT.html vkDebugMarkerSetObjectNameEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugMarkerSetObjectNameEXT"
               vkDebugMarkerSetObjectNameEXT ::
               VkDevice -- ^ device
                        -> Ptr VkDebugMarkerObjectNameInfoEXT -- ^ pNameInfo
                                                              -> IO VkResult

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerBeginEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerBeginEXT.html vkCmdDebugMarkerBeginEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerBeginEXT"
               vkCmdDebugMarkerBeginEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                                 -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerEndEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerEndEXT.html vkCmdDebugMarkerEndEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerEndEXT"
               vkCmdDebugMarkerEndEXT :: VkCommandBuffer -- ^ commandBuffer
                                                         -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerInsertEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerInsertEXT.html vkCmdDebugMarkerInsertEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerInsertEXT"
               vkCmdDebugMarkerInsertEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                                 -> IO ()

pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

type VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString

pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME <-
        (is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME -> True)
  where VK_EXT_DEBUG_MARKER_EXTENSION_NAME
          = _VK_EXT_DEBUG_MARKER_EXTENSION_NAME

{-# INLINE _VK_EXT_DEBUG_MARKER_EXTENSION_NAME #-}

_VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString
_VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  = Ptr "VK_EXT_debug_marker\NUL"##

{-# INLINE is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME #-}

is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  = eqCStrings _VK_EXT_DEBUG_MARKER_EXTENSION_NAME

type VK_EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT =
        VkStructureType 1000022000

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT =
        VkStructureType 1000022001

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT =
        VkStructureType 1000022002
