#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkApplicationInfo
       (VkApplicationInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkApplicationInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     const char*     pApplicationName;
--   >     uint32_t        applicationVersion;
--   >     const char*     pEngineName;
--   >     uint32_t        engineVersion;
--   >     uint32_t        apiVersion;
--   > } VkApplicationInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkApplicationInfo.html VkApplicationInfo registry at www.khronos.org>
data VkApplicationInfo = VkApplicationInfo## Addr## ByteArray##

instance Eq VkApplicationInfo where
        (VkApplicationInfo## a _) == x@(VkApplicationInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkApplicationInfo where
        (VkApplicationInfo## a _) `compare` x@(VkApplicationInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkApplicationInfo where
        sizeOf ~_ = #{size VkApplicationInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkApplicationInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkApplicationInfo where
        unsafeAddr (VkApplicationInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkApplicationInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkApplicationInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkApplicationInfo where
        type StructFields VkApplicationInfo =
             '["sType", "pNext", "pApplicationName", "applicationVersion", -- ' closing tick for hsc2hs
               "pEngineName", "engineVersion", "apiVersion"]
        type CUnionType VkApplicationInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkApplicationInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkApplicationInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkApplicationInfo where
        type VkSTypeMType VkApplicationInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkApplicationInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkApplicationInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkApplicationInfo, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkApplicationInfo
         where
        type FieldType "sType" VkApplicationInfo = VkStructureType
        type FieldOptional "sType" VkApplicationInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkApplicationInfo =
             #{offset VkApplicationInfo, sType}
        type FieldIsArray "sType" VkApplicationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkApplicationInfo, sType}

instance CanReadField "sType" VkApplicationInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkApplicationInfo where
        type VkPNextMType VkApplicationInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkApplicationInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkApplicationInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkApplicationInfo, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkApplicationInfo
         where
        type FieldType "pNext" VkApplicationInfo = Ptr Void
        type FieldOptional "pNext" VkApplicationInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkApplicationInfo =
             #{offset VkApplicationInfo, pNext}
        type FieldIsArray "pNext" VkApplicationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkApplicationInfo, pNext}

instance CanReadField "pNext" VkApplicationInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPApplicationName VkApplicationInfo where
        type VkPApplicationNameMType VkApplicationInfo = CString

        {-# NOINLINE vkPApplicationName #-}
        vkPApplicationName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, pApplicationName})

        {-# INLINE vkPApplicationNameByteOffset #-}
        vkPApplicationNameByteOffset ~_
          = #{offset VkApplicationInfo, pApplicationName}

        {-# INLINE readVkPApplicationName #-}
        readVkPApplicationName p
          = peekByteOff p #{offset VkApplicationInfo, pApplicationName}

        {-# INLINE writeVkPApplicationName #-}
        writeVkPApplicationName p
          = pokeByteOff p #{offset VkApplicationInfo, pApplicationName}

instance {-# OVERLAPPING #-}
         HasField "pApplicationName" VkApplicationInfo where
        type FieldType "pApplicationName" VkApplicationInfo = CString
        type FieldOptional "pApplicationName" VkApplicationInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pApplicationName" VkApplicationInfo =
             #{offset VkApplicationInfo, pApplicationName}
        type FieldIsArray "pApplicationName" VkApplicationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkApplicationInfo, pApplicationName}

instance CanReadField "pApplicationName" VkApplicationInfo where
        {-# INLINE getField #-}
        getField = vkPApplicationName

        {-# INLINE readField #-}
        readField = readVkPApplicationName

instance CanWriteField "pApplicationName" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPApplicationName

instance {-# OVERLAPPING #-}
         HasVkApplicationVersion VkApplicationInfo where
        type VkApplicationVersionMType VkApplicationInfo = Word32

        {-# NOINLINE vkApplicationVersion #-}
        vkApplicationVersion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, applicationVersion})

        {-# INLINE vkApplicationVersionByteOffset #-}
        vkApplicationVersionByteOffset ~_
          = #{offset VkApplicationInfo, applicationVersion}

        {-# INLINE readVkApplicationVersion #-}
        readVkApplicationVersion p
          = peekByteOff p #{offset VkApplicationInfo, applicationVersion}

        {-# INLINE writeVkApplicationVersion #-}
        writeVkApplicationVersion p
          = pokeByteOff p #{offset VkApplicationInfo, applicationVersion}

instance {-# OVERLAPPING #-}
         HasField "applicationVersion" VkApplicationInfo where
        type FieldType "applicationVersion" VkApplicationInfo = Word32
        type FieldOptional "applicationVersion" VkApplicationInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "applicationVersion" VkApplicationInfo =
             #{offset VkApplicationInfo, applicationVersion}
        type FieldIsArray "applicationVersion" VkApplicationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkApplicationInfo, applicationVersion}

instance CanReadField "applicationVersion" VkApplicationInfo where
        {-# INLINE getField #-}
        getField = vkApplicationVersion

        {-# INLINE readField #-}
        readField = readVkApplicationVersion

instance CanWriteField "applicationVersion" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkApplicationVersion

instance {-# OVERLAPPING #-} HasVkPEngineName VkApplicationInfo
         where
        type VkPEngineNameMType VkApplicationInfo = CString

        {-# NOINLINE vkPEngineName #-}
        vkPEngineName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, pEngineName})

        {-# INLINE vkPEngineNameByteOffset #-}
        vkPEngineNameByteOffset ~_
          = #{offset VkApplicationInfo, pEngineName}

        {-# INLINE readVkPEngineName #-}
        readVkPEngineName p
          = peekByteOff p #{offset VkApplicationInfo, pEngineName}

        {-# INLINE writeVkPEngineName #-}
        writeVkPEngineName p
          = pokeByteOff p #{offset VkApplicationInfo, pEngineName}

instance {-# OVERLAPPING #-}
         HasField "pEngineName" VkApplicationInfo where
        type FieldType "pEngineName" VkApplicationInfo = CString
        type FieldOptional "pEngineName" VkApplicationInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pEngineName" VkApplicationInfo =
             #{offset VkApplicationInfo, pEngineName}
        type FieldIsArray "pEngineName" VkApplicationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkApplicationInfo, pEngineName}

instance CanReadField "pEngineName" VkApplicationInfo where
        {-# INLINE getField #-}
        getField = vkPEngineName

        {-# INLINE readField #-}
        readField = readVkPEngineName

instance CanWriteField "pEngineName" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPEngineName

instance {-# OVERLAPPING #-} HasVkEngineVersion VkApplicationInfo
         where
        type VkEngineVersionMType VkApplicationInfo = Word32

        {-# NOINLINE vkEngineVersion #-}
        vkEngineVersion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, engineVersion})

        {-# INLINE vkEngineVersionByteOffset #-}
        vkEngineVersionByteOffset ~_
          = #{offset VkApplicationInfo, engineVersion}

        {-# INLINE readVkEngineVersion #-}
        readVkEngineVersion p
          = peekByteOff p #{offset VkApplicationInfo, engineVersion}

        {-# INLINE writeVkEngineVersion #-}
        writeVkEngineVersion p
          = pokeByteOff p #{offset VkApplicationInfo, engineVersion}

instance {-# OVERLAPPING #-}
         HasField "engineVersion" VkApplicationInfo where
        type FieldType "engineVersion" VkApplicationInfo = Word32
        type FieldOptional "engineVersion" VkApplicationInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "engineVersion" VkApplicationInfo =
             #{offset VkApplicationInfo, engineVersion}
        type FieldIsArray "engineVersion" VkApplicationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkApplicationInfo, engineVersion}

instance CanReadField "engineVersion" VkApplicationInfo where
        {-# INLINE getField #-}
        getField = vkEngineVersion

        {-# INLINE readField #-}
        readField = readVkEngineVersion

instance CanWriteField "engineVersion" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkEngineVersion

instance {-# OVERLAPPING #-} HasVkApiVersion VkApplicationInfo
         where
        type VkApiVersionMType VkApplicationInfo = Word32

        {-# NOINLINE vkApiVersion #-}
        vkApiVersion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, apiVersion})

        {-# INLINE vkApiVersionByteOffset #-}
        vkApiVersionByteOffset ~_
          = #{offset VkApplicationInfo, apiVersion}

        {-# INLINE readVkApiVersion #-}
        readVkApiVersion p
          = peekByteOff p #{offset VkApplicationInfo, apiVersion}

        {-# INLINE writeVkApiVersion #-}
        writeVkApiVersion p
          = pokeByteOff p #{offset VkApplicationInfo, apiVersion}

instance {-# OVERLAPPING #-}
         HasField "apiVersion" VkApplicationInfo where
        type FieldType "apiVersion" VkApplicationInfo = Word32
        type FieldOptional "apiVersion" VkApplicationInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "apiVersion" VkApplicationInfo =
             #{offset VkApplicationInfo, apiVersion}
        type FieldIsArray "apiVersion" VkApplicationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkApplicationInfo, apiVersion}

instance CanReadField "apiVersion" VkApplicationInfo where
        {-# INLINE getField #-}
        getField = vkApiVersion

        {-# INLINE readField #-}
        readField = readVkApiVersion

instance CanWriteField "apiVersion" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkApiVersion

instance Show VkApplicationInfo where
        showsPrec d x
          = showString "VkApplicationInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPApplicationName = " .
                            showsPrec d (vkPApplicationName x) .
                              showString ", " .
                                showString "vkApplicationVersion = " .
                                  showsPrec d (vkApplicationVersion x) .
                                    showString ", " .
                                      showString "vkPEngineName = " .
                                        showsPrec d (vkPEngineName x) .
                                          showString ", " .
                                            showString "vkEngineVersion = " .
                                              showsPrec d (vkEngineVersion x) .
                                                showString ", " .
                                                  showString "vkApiVersion = " .
                                                    showsPrec d (vkApiVersion x) . showChar '}'
