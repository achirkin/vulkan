#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDebugMarkerObjectNameInfoEXT
       (VkDebugMarkerObjectNameInfoEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT (VkDebugReportObjectTypeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
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
        type FieldIsArray "sType" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "objectType" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "object" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

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
