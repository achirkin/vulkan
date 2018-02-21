#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDebugReportCallbackCreateInfoEXT
       (VkDebugReportCallbackCreateInfoEXT(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDebugReportFlagsEXT  (VkDebugReportFlagsEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Funcpointers                (PFN_vkDebugReportCallbackEXT)
import           Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo (VkInstanceCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkDebugReportCallbackCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportFlagsEXT            flags;
--   >     PFN_vkDebugReportCallbackEXT     pfnCallback;
--   >     void*            pUserData;
--   > } VkDebugReportCallbackCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDebugReportCallbackCreateInfoEXT.html VkDebugReportCallbackCreateInfoEXT registry at www.khronos.org>
data VkDebugReportCallbackCreateInfoEXT = VkDebugReportCallbackCreateInfoEXT## Addr##
                                                                              ByteArray##

instance Eq VkDebugReportCallbackCreateInfoEXT where
        (VkDebugReportCallbackCreateInfoEXT## a _) ==
          x@(VkDebugReportCallbackCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugReportCallbackCreateInfoEXT where
        (VkDebugReportCallbackCreateInfoEXT## a _) `compare`
          x@(VkDebugReportCallbackCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugReportCallbackCreateInfoEXT where
        sizeOf ~_ = #{size VkDebugReportCallbackCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugReportCallbackCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugReportCallbackCreateInfoEXT where
        unsafeAddr (VkDebugReportCallbackCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugReportCallbackCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugReportCallbackCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugReportCallbackCreateInfoEXT where
        type StructFields VkDebugReportCallbackCreateInfoEXT =
             '["sType", "pNext", "flags", "pfnCallback", "pUserData"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugReportCallbackCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugReportCallbackCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugReportCallbackCreateInfoEXT =
             '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDebugReportCallbackCreateInfoEXT where
        type VkSTypeMType VkDebugReportCallbackCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "sType" VkDebugReportCallbackCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, sType}
        type FieldIsArray "sType" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, sType}

instance CanReadField "sType" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDebugReportCallbackCreateInfoEXT where
        type VkPNextMType VkDebugReportCallbackCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pNext" VkDebugReportCallbackCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

instance CanReadField "pNext" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkDebugReportCallbackCreateInfoEXT where
        type VkFlagsMType VkDebugReportCallbackCreateInfoEXT =
             VkDebugReportFlagsEXT

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "flags" VkDebugReportCallbackCreateInfoEXT =
             VkDebugReportFlagsEXT
        type FieldOptional "flags" VkDebugReportCallbackCreateInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, flags}
        type FieldIsArray "flags" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, flags}

instance CanReadField "flags" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkPfnCallback VkDebugReportCallbackCreateInfoEXT where
        type VkPfnCallbackMType VkDebugReportCallbackCreateInfoEXT =
             PFN_vkDebugReportCallbackEXT

        {-# NOINLINE vkPfnCallback #-}
        vkPfnCallback x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback})

        {-# INLINE vkPfnCallbackByteOffset #-}
        vkPfnCallbackByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

        {-# INLINE readVkPfnCallback #-}
        readVkPfnCallback p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

        {-# INLINE writeVkPfnCallback #-}
        writeVkPfnCallback p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

instance {-# OVERLAPPING #-}
         HasField "pfnCallback" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pfnCallback" VkDebugReportCallbackCreateInfoEXT =
             PFN_vkDebugReportCallbackEXT
        type FieldOptional "pfnCallback" VkDebugReportCallbackCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pfnCallback" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}
        type FieldIsArray "pfnCallback" VkDebugReportCallbackCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

instance CanReadField "pfnCallback"
           VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPfnCallback

        {-# INLINE readField #-}
        readField = readVkPfnCallback

instance CanWriteField "pfnCallback"
           VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPfnCallback

instance {-# OVERLAPPING #-}
         HasVkPUserData VkDebugReportCallbackCreateInfoEXT where
        type VkPUserDataMType VkDebugReportCallbackCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPUserData #-}
        vkPUserData x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pUserData})

        {-# INLINE vkPUserDataByteOffset #-}
        vkPUserDataByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

        {-# INLINE readVkPUserData #-}
        readVkPUserData p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

        {-# INLINE writeVkPUserData #-}
        writeVkPUserData p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance {-# OVERLAPPING #-}
         HasField "pUserData" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pUserData" VkDebugReportCallbackCreateInfoEXT =
             Ptr Void
        type FieldOptional "pUserData" VkDebugReportCallbackCreateInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pUserData" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}
        type FieldIsArray "pUserData" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance CanReadField "pUserData"
           VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPUserData

        {-# INLINE readField #-}
        readField = readVkPUserData

instance CanWriteField "pUserData"
           VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPUserData

instance Show VkDebugReportCallbackCreateInfoEXT where
        showsPrec d x
          = showString "VkDebugReportCallbackCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkPfnCallback = " .
                                  showsPrec d (vkPfnCallback x) .
                                    showString ", " .
                                      showString "vkPUserData = " .
                                        showsPrec d (vkPUserData x) . showChar '}'
