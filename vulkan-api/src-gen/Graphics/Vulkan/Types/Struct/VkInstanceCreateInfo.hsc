#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo
       (VkInstanceCreateInfo(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                 (VkInstanceCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkApplicationInfo (VkApplicationInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkInstanceCreateInfo.html VkInstanceCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkInstanceCreateInfo where
        type VkSTypeMType VkInstanceCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkInstanceCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkInstanceCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkInstanceCreateInfo, sType}

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

instance CanReadField "sType" VkInstanceCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkInstanceCreateInfo where
        type VkPNextMType VkInstanceCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkInstanceCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkInstanceCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkInstanceCreateInfo, pNext}

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

instance CanReadField "pNext" VkInstanceCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkInstanceCreateInfo where
        type VkFlagsMType VkInstanceCreateInfo = VkInstanceCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkInstanceCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkInstanceCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkInstanceCreateInfo, flags}

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

instance CanReadField "flags" VkInstanceCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkInstanceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkPApplicationInfo VkInstanceCreateInfo where
        type VkPApplicationInfoMType VkInstanceCreateInfo =
             Ptr VkApplicationInfo

        {-# NOINLINE vkPApplicationInfo #-}
        vkPApplicationInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, pApplicationInfo})

        {-# INLINE vkPApplicationInfoByteOffset #-}
        vkPApplicationInfoByteOffset ~_
          = #{offset VkInstanceCreateInfo, pApplicationInfo}

        {-# INLINE readVkPApplicationInfo #-}
        readVkPApplicationInfo p
          = peekByteOff p #{offset VkInstanceCreateInfo, pApplicationInfo}

        {-# INLINE writeVkPApplicationInfo #-}
        writeVkPApplicationInfo p
          = pokeByteOff p #{offset VkInstanceCreateInfo, pApplicationInfo}

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

instance CanReadField "pApplicationInfo" VkInstanceCreateInfo where
        {-# INLINE getField #-}
        getField = vkPApplicationInfo

        {-# INLINE readField #-}
        readField = readVkPApplicationInfo

instance CanWriteField "pApplicationInfo" VkInstanceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPApplicationInfo

instance {-# OVERLAPPING #-}
         HasVkEnabledLayerCount VkInstanceCreateInfo where
        type VkEnabledLayerCountMType VkInstanceCreateInfo = Word32

        {-# NOINLINE vkEnabledLayerCount #-}
        vkEnabledLayerCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, enabledLayerCount})

        {-# INLINE vkEnabledLayerCountByteOffset #-}
        vkEnabledLayerCountByteOffset ~_
          = #{offset VkInstanceCreateInfo, enabledLayerCount}

        {-# INLINE readVkEnabledLayerCount #-}
        readVkEnabledLayerCount p
          = peekByteOff p #{offset VkInstanceCreateInfo, enabledLayerCount}

        {-# INLINE writeVkEnabledLayerCount #-}
        writeVkEnabledLayerCount p
          = pokeByteOff p #{offset VkInstanceCreateInfo, enabledLayerCount}

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

instance CanReadField "enabledLayerCount" VkInstanceCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkEnabledLayerCount

        {-# INLINE readField #-}
        readField = readVkEnabledLayerCount

instance CanWriteField "enabledLayerCount" VkInstanceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkEnabledLayerCount

instance {-# OVERLAPPING #-}
         HasVkPpEnabledLayerNames VkInstanceCreateInfo where
        type VkPpEnabledLayerNamesMType VkInstanceCreateInfo = Ptr CString

        {-# NOINLINE vkPpEnabledLayerNames #-}
        vkPpEnabledLayerNames x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, ppEnabledLayerNames})

        {-# INLINE vkPpEnabledLayerNamesByteOffset #-}
        vkPpEnabledLayerNamesByteOffset ~_
          = #{offset VkInstanceCreateInfo, ppEnabledLayerNames}

        {-# INLINE readVkPpEnabledLayerNames #-}
        readVkPpEnabledLayerNames p
          = peekByteOff p #{offset VkInstanceCreateInfo, ppEnabledLayerNames}

        {-# INLINE writeVkPpEnabledLayerNames #-}
        writeVkPpEnabledLayerNames p
          = pokeByteOff p #{offset VkInstanceCreateInfo, ppEnabledLayerNames}

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

instance CanReadField "ppEnabledLayerNames" VkInstanceCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPpEnabledLayerNames

        {-# INLINE readField #-}
        readField = readVkPpEnabledLayerNames

instance CanWriteField "ppEnabledLayerNames" VkInstanceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPpEnabledLayerNames

instance {-# OVERLAPPING #-}
         HasVkEnabledExtensionCount VkInstanceCreateInfo where
        type VkEnabledExtensionCountMType VkInstanceCreateInfo = Word32

        {-# NOINLINE vkEnabledExtensionCount #-}
        vkEnabledExtensionCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, enabledExtensionCount})

        {-# INLINE vkEnabledExtensionCountByteOffset #-}
        vkEnabledExtensionCountByteOffset ~_
          = #{offset VkInstanceCreateInfo, enabledExtensionCount}

        {-# INLINE readVkEnabledExtensionCount #-}
        readVkEnabledExtensionCount p
          = peekByteOff p #{offset VkInstanceCreateInfo, enabledExtensionCount}

        {-# INLINE writeVkEnabledExtensionCount #-}
        writeVkEnabledExtensionCount p
          = pokeByteOff p #{offset VkInstanceCreateInfo, enabledExtensionCount}

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

instance CanReadField "enabledExtensionCount" VkInstanceCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkEnabledExtensionCount

        {-# INLINE readField #-}
        readField = readVkEnabledExtensionCount

instance CanWriteField "enabledExtensionCount" VkInstanceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkEnabledExtensionCount

instance {-# OVERLAPPING #-}
         HasVkPpEnabledExtensionNames VkInstanceCreateInfo where
        type VkPpEnabledExtensionNamesMType VkInstanceCreateInfo =
             Ptr CString

        {-# NOINLINE vkPpEnabledExtensionNames #-}
        vkPpEnabledExtensionNames x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkInstanceCreateInfo, ppEnabledExtensionNames})

        {-# INLINE vkPpEnabledExtensionNamesByteOffset #-}
        vkPpEnabledExtensionNamesByteOffset ~_
          = #{offset VkInstanceCreateInfo, ppEnabledExtensionNames}

        {-# INLINE readVkPpEnabledExtensionNames #-}
        readVkPpEnabledExtensionNames p
          = peekByteOff p #{offset VkInstanceCreateInfo, ppEnabledExtensionNames}

        {-# INLINE writeVkPpEnabledExtensionNames #-}
        writeVkPpEnabledExtensionNames p
          = pokeByteOff p #{offset VkInstanceCreateInfo, ppEnabledExtensionNames}

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

instance CanReadField "ppEnabledExtensionNames"
           VkInstanceCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPpEnabledExtensionNames

        {-# INLINE readField #-}
        readField = readVkPpEnabledExtensionNames

instance CanWriteField "ppEnabledExtensionNames"
           VkInstanceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPpEnabledExtensionNames

instance Show VkInstanceCreateInfo where
        showsPrec d x
          = showString "VkInstanceCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkPApplicationInfo = " .
                                  showsPrec d (vkPApplicationInfo x) .
                                    showString ", " .
                                      showString "vkEnabledLayerCount = " .
                                        showsPrec d (vkEnabledLayerCount x) .
                                          showString ", " .
                                            showString "vkPpEnabledLayerNames = " .
                                              showsPrec d (vkPpEnabledLayerNames x) .
                                                showString ", " .
                                                  showString "vkEnabledExtensionCount = " .
                                                    showsPrec d (vkEnabledExtensionCount x) .
                                                      showString ", " .
                                                        showString "vkPpEnabledExtensionNames = " .
                                                          showsPrec d (vkPpEnabledExtensionNames x)
                                                            . showChar '}'
