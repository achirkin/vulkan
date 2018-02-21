#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDebugMarkerObjectTagInfoEXT
       (VkDebugMarkerObjectTagInfoEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT (VkDebugReportObjectTypeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDebugMarkerObjectTagInfoEXT.html VkDebugMarkerObjectTagInfoEXT registry at www.khronos.org>
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
        type FieldIsArray "sType" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "objectType" VkDebugMarkerObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "object" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "tagName" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type VkTagSizeMType VkDebugMarkerObjectTagInfoEXT = CSize

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
        type FieldIsArray "pTag" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

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
