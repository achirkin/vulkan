#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.InstanceCreateInfo
       (VkInstanceCreateInfo(..)) where
import           Foreign.Storable                             (Storable (..))
import           GHC.Base                                     (Addr##,
                                                               ByteArray##,
                                                               byteArrayContents##,
                                                               plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks               (VkInstanceCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.ApplicationInfo (VkApplicationInfo)
import           System.IO.Unsafe                             (unsafeDupablePerformIO)

-- | > typedef struct VkInstanceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     VkInstanceCreateFlags  flags;
--   >     const VkApplicationInfo* pApplicationInfo;
--   >     uint32_t               enabledLayerCount;
--   >     const char* const*      ppEnabledLayerNames;
--   >     uint32_t               enabledExtensionCount;
--   >     const char* const*      ppEnabledExtensionNames;
--   > } VkInstanceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkInstanceCreateInfo VkInstanceCreateInfo registry at www.khronos.org>
data VkInstanceCreateInfo = VkInstanceCreateInfo## Addr## ByteArray##

instance Eq VkInstanceCreateInfo where
        (VkInstanceCreateInfo## a _) == x@(VkInstanceCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkInstanceCreateInfo where
        (VkInstanceCreateInfo## a _) `compare` x@(VkInstanceCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkInstanceCreateInfo where
        sizeOf ~_ = #{size VkInstanceCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkInstanceCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkInstanceCreateInfo where
        unsafeAddr (VkInstanceCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkInstanceCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkInstanceCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkInstanceCreateInfo where
        type StructFields VkInstanceCreateInfo =
             '["sType", "pNext", "flags", "pApplicationInfo", -- ' closing tick for hsc2hs
               "enabledLayerCount", "ppEnabledLayerNames",
               "enabledExtensionCount", "ppEnabledExtensionNames"]
        type CUnionType VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkInstanceCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkInstanceCreateInfo
         where
        type FieldType "sType" VkInstanceCreateInfo = VkStructureType
        type FieldOptional "sType" VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkInstanceCreateInfo =
             #{offset VkInstanceCreateInfo, sType}
        type FieldIsArray "sType" VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkInstanceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkInstanceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInstanceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInstanceCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkInstanceCreateInfo
         where
        type FieldType "pNext" VkInstanceCreateInfo = Ptr Void
        type FieldOptional "pNext" VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkInstanceCreateInfo =
             #{offset VkInstanceCreateInfo, pNext}
        type FieldIsArray "pNext" VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkInstanceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkInstanceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInstanceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInstanceCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkInstanceCreateInfo
         where
        type FieldType "flags" VkInstanceCreateInfo = VkInstanceCreateFlags
        type FieldOptional "flags" VkInstanceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkInstanceCreateInfo =
             #{offset VkInstanceCreateInfo, flags}
        type FieldIsArray "flags" VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkInstanceCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkInstanceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInstanceCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInstanceCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "pApplicationInfo" VkInstanceCreateInfo where
        type FieldType "pApplicationInfo" VkInstanceCreateInfo =
             Ptr VkApplicationInfo
        type FieldOptional "pApplicationInfo" VkInstanceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pApplicationInfo" VkInstanceCreateInfo =
             #{offset VkInstanceCreateInfo, pApplicationInfo}
        type FieldIsArray "pApplicationInfo" VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInstanceCreateInfo, pApplicationInfo}

instance {-# OVERLAPPING #-}
         CanReadField "pApplicationInfo" VkInstanceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, pApplicationInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInstanceCreateInfo, pApplicationInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "pApplicationInfo" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInstanceCreateInfo, pApplicationInfo}

instance {-# OVERLAPPING #-}
         HasField "enabledLayerCount" VkInstanceCreateInfo where
        type FieldType "enabledLayerCount" VkInstanceCreateInfo = Word32
        type FieldOptional "enabledLayerCount" VkInstanceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "enabledLayerCount" VkInstanceCreateInfo =
             #{offset VkInstanceCreateInfo, enabledLayerCount}
        type FieldIsArray "enabledLayerCount" VkInstanceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInstanceCreateInfo, enabledLayerCount}

instance {-# OVERLAPPING #-}
         CanReadField "enabledLayerCount" VkInstanceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, enabledLayerCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInstanceCreateInfo, enabledLayerCount}

instance {-# OVERLAPPING #-}
         CanWriteField "enabledLayerCount" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInstanceCreateInfo, enabledLayerCount}

instance {-# OVERLAPPING #-}
         HasField "ppEnabledLayerNames" VkInstanceCreateInfo where
        type FieldType "ppEnabledLayerNames" VkInstanceCreateInfo =
             Ptr CString
        type FieldOptional "ppEnabledLayerNames" VkInstanceCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "ppEnabledLayerNames" VkInstanceCreateInfo =
             #{offset VkInstanceCreateInfo, ppEnabledLayerNames}
        type FieldIsArray "ppEnabledLayerNames" VkInstanceCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInstanceCreateInfo, ppEnabledLayerNames}

instance {-# OVERLAPPING #-}
         CanReadField "ppEnabledLayerNames" VkInstanceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, ppEnabledLayerNames})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInstanceCreateInfo, ppEnabledLayerNames}

instance {-# OVERLAPPING #-}
         CanWriteField "ppEnabledLayerNames" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInstanceCreateInfo, ppEnabledLayerNames}

instance {-# OVERLAPPING #-}
         HasField "enabledExtensionCount" VkInstanceCreateInfo where
        type FieldType "enabledExtensionCount" VkInstanceCreateInfo =
             Word32
        type FieldOptional "enabledExtensionCount" VkInstanceCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "enabledExtensionCount" VkInstanceCreateInfo =
             #{offset VkInstanceCreateInfo, enabledExtensionCount}
        type FieldIsArray "enabledExtensionCount" VkInstanceCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInstanceCreateInfo, enabledExtensionCount}

instance {-# OVERLAPPING #-}
         CanReadField "enabledExtensionCount" VkInstanceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, enabledExtensionCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInstanceCreateInfo, enabledExtensionCount}

instance {-# OVERLAPPING #-}
         CanWriteField "enabledExtensionCount" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInstanceCreateInfo, enabledExtensionCount}

instance {-# OVERLAPPING #-}
         HasField "ppEnabledExtensionNames" VkInstanceCreateInfo where
        type FieldType "ppEnabledExtensionNames" VkInstanceCreateInfo =
             Ptr CString
        type FieldOptional "ppEnabledExtensionNames" VkInstanceCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "ppEnabledExtensionNames" VkInstanceCreateInfo =
             #{offset VkInstanceCreateInfo, ppEnabledExtensionNames}
        type FieldIsArray "ppEnabledExtensionNames" VkInstanceCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkInstanceCreateInfo, ppEnabledExtensionNames}

instance {-# OVERLAPPING #-}
         CanReadField "ppEnabledExtensionNames" VkInstanceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, ppEnabledExtensionNames})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkInstanceCreateInfo, ppEnabledExtensionNames}

instance {-# OVERLAPPING #-}
         CanWriteField "ppEnabledExtensionNames" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkInstanceCreateInfo, ppEnabledExtensionNames}

instance Show VkInstanceCreateInfo where
        showsPrec d x
          = showString "VkInstanceCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "pApplicationInfo = " .
                                  showsPrec d (getField @"pApplicationInfo" x) .
                                    showString ", " .
                                      showString "enabledLayerCount = " .
                                        showsPrec d (getField @"enabledLayerCount" x) .
                                          showString ", " .
                                            showString "ppEnabledLayerNames = " .
                                              showsPrec d (getField @"ppEnabledLayerNames" x) .
                                                showString ", " .
                                                  showString "enabledExtensionCount = " .
                                                    showsPrec d
                                                      (getField @"enabledExtensionCount" x)
                                                      .
                                                      showString ", " .
                                                        showString "ppEnabledExtensionNames = " .
                                                          showsPrec d
                                                            (getField @"ppEnabledExtensionNames" x)
                                                            . showChar '}'
