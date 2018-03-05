#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "pfnCallback" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

instance {-# OVERLAPPING #-}
         CanWriteField "pfnCallback" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

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

instance {-# OVERLAPPING #-}
         CanReadField "pUserData" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pUserData})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance {-# OVERLAPPING #-}
         CanWriteField "pUserData" VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance Show VkDebugReportCallbackCreateInfoEXT where
        showsPrec d x
          = showString "VkDebugReportCallbackCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "pfnCallback = " .
                                  showsPrec d (getField @"pfnCallback" x) .
                                    showString ", " .
                                      showString "pUserData = " .
                                        showsPrec d (getField @"pUserData" x) . showChar '}'
