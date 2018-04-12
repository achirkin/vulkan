#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ApplicationInfo
       (VkApplicationInfo(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkApplicationInfo VkApplicationInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "sType" VkApplicationInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkApplicationInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkApplicationInfo, sType}

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

instance {-# OVERLAPPING #-} CanReadField "pNext" VkApplicationInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkApplicationInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkApplicationInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "pApplicationName" VkApplicationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, pApplicationName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkApplicationInfo, pApplicationName}

instance {-# OVERLAPPING #-}
         CanWriteField "pApplicationName" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkApplicationInfo, pApplicationName}

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

instance {-# OVERLAPPING #-}
         CanReadField "applicationVersion" VkApplicationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, applicationVersion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkApplicationInfo, applicationVersion}

instance {-# OVERLAPPING #-}
         CanWriteField "applicationVersion" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkApplicationInfo, applicationVersion}

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

instance {-# OVERLAPPING #-}
         CanReadField "pEngineName" VkApplicationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, pEngineName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkApplicationInfo, pEngineName}

instance {-# OVERLAPPING #-}
         CanWriteField "pEngineName" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkApplicationInfo, pEngineName}

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

instance {-# OVERLAPPING #-}
         CanReadField "engineVersion" VkApplicationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, engineVersion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkApplicationInfo, engineVersion}

instance {-# OVERLAPPING #-}
         CanWriteField "engineVersion" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkApplicationInfo, engineVersion}

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

instance {-# OVERLAPPING #-}
         CanReadField "apiVersion" VkApplicationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkApplicationInfo, apiVersion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkApplicationInfo, apiVersion}

instance {-# OVERLAPPING #-}
         CanWriteField "apiVersion" VkApplicationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkApplicationInfo, apiVersion}

instance Show VkApplicationInfo where
        showsPrec d x
          = showString "VkApplicationInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pApplicationName = " .
                            showsPrec d (getField @"pApplicationName" x) .
                              showString ", " .
                                showString "applicationVersion = " .
                                  showsPrec d (getField @"applicationVersion" x) .
                                    showString ", " .
                                      showString "pEngineName = " .
                                        showsPrec d (getField @"pEngineName" x) .
                                          showString ", " .
                                            showString "engineVersion = " .
                                              showsPrec d (getField @"engineVersion" x) .
                                                showString ", " .
                                                  showString "apiVersion = " .
                                                    showsPrec d (getField @"apiVersion" x) .
                                                      showChar '}'
