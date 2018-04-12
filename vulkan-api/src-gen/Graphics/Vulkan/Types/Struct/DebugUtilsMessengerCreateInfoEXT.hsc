#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DebugUtilsMessengerCreateInfoEXT
       (VkDebugUtilsMessengerCreateInfoEXT(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                  (VkDebugUtilsMessengerCreateFlagsEXT)
import           Graphics.Vulkan.Types.Enum.Debug                (VkDebugUtilsMessageSeverityFlagsEXT,
                                                                  VkDebugUtilsMessageTypeFlagsEXT)
import           Graphics.Vulkan.Types.Enum.StructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Funcpointers              (PFN_vkDebugUtilsMessengerCallbackEXT)
import           Graphics.Vulkan.Types.Struct.InstanceCreateInfo (VkInstanceCreateInfo)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkDebugUtilsMessengerCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                          pNext;
--   >     VkDebugUtilsMessengerCreateFlagsEXT  flags;
--   >     VkDebugUtilsMessageSeverityFlagsEXT                  messageSeverity;
--   >     VkDebugUtilsMessageTypeFlagsEXT                      messageType;
--   >     PFN_vkDebugUtilsMessengerCallbackEXT                 pfnUserCallback;
--   >     void*                                pUserData;
--   > } VkDebugUtilsMessengerCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsMessengerCreateInfoEXT VkDebugUtilsMessengerCreateInfoEXT registry at www.khronos.org>
data VkDebugUtilsMessengerCreateInfoEXT = VkDebugUtilsMessengerCreateInfoEXT## Addr##
                                                                              ByteArray##

instance Eq VkDebugUtilsMessengerCreateInfoEXT where
        (VkDebugUtilsMessengerCreateInfoEXT## a _) ==
          x@(VkDebugUtilsMessengerCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugUtilsMessengerCreateInfoEXT where
        (VkDebugUtilsMessengerCreateInfoEXT## a _) `compare`
          x@(VkDebugUtilsMessengerCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugUtilsMessengerCreateInfoEXT where
        sizeOf ~_ = #{size VkDebugUtilsMessengerCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugUtilsMessengerCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugUtilsMessengerCreateInfoEXT where
        unsafeAddr (VkDebugUtilsMessengerCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugUtilsMessengerCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugUtilsMessengerCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugUtilsMessengerCreateInfoEXT where
        type StructFields VkDebugUtilsMessengerCreateInfoEXT =
             '["sType", "pNext", "flags", "messageSeverity", "messageType", -- ' closing tick for hsc2hs
               "pfnUserCallback", "pUserData"]
        type CUnionType VkDebugUtilsMessengerCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugUtilsMessengerCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugUtilsMessengerCreateInfoEXT =
             '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugUtilsMessengerCreateInfoEXT where
        type FieldType "sType" VkDebugUtilsMessengerCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugUtilsMessengerCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugUtilsMessengerCreateInfoEXT =
             #{offset VkDebugUtilsMessengerCreateInfoEXT, sType}
        type FieldIsArray "sType" VkDebugUtilsMessengerCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugUtilsMessengerCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugUtilsMessengerCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugUtilsMessengerCreateInfoEXT where
        type FieldType "pNext" VkDebugUtilsMessengerCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkDebugUtilsMessengerCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugUtilsMessengerCreateInfoEXT =
             #{offset VkDebugUtilsMessengerCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugUtilsMessengerCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugUtilsMessengerCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugUtilsMessengerCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDebugUtilsMessengerCreateInfoEXT where
        type FieldType "flags" VkDebugUtilsMessengerCreateInfoEXT =
             VkDebugUtilsMessengerCreateFlagsEXT
        type FieldOptional "flags" VkDebugUtilsMessengerCreateInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDebugUtilsMessengerCreateInfoEXT =
             #{offset VkDebugUtilsMessengerCreateInfoEXT, flags}
        type FieldIsArray "flags" VkDebugUtilsMessengerCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDebugUtilsMessengerCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCreateInfoEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDebugUtilsMessengerCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "messageSeverity" VkDebugUtilsMessengerCreateInfoEXT where
        type FieldType "messageSeverity" VkDebugUtilsMessengerCreateInfoEXT
             = VkDebugUtilsMessageSeverityFlagsEXT
        type FieldOptional "messageSeverity"
               VkDebugUtilsMessengerCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "messageSeverity"
               VkDebugUtilsMessengerCreateInfoEXT
             =
             #{offset VkDebugUtilsMessengerCreateInfoEXT, messageSeverity}
        type FieldIsArray "messageSeverity"
               VkDebugUtilsMessengerCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCreateInfoEXT, messageSeverity}

instance {-# OVERLAPPING #-}
         CanReadField "messageSeverity" VkDebugUtilsMessengerCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCreateInfoEXT, messageSeverity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, messageSeverity}

instance {-# OVERLAPPING #-}
         CanWriteField "messageSeverity" VkDebugUtilsMessengerCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, messageSeverity}

instance {-# OVERLAPPING #-}
         HasField "messageType" VkDebugUtilsMessengerCreateInfoEXT where
        type FieldType "messageType" VkDebugUtilsMessengerCreateInfoEXT =
             VkDebugUtilsMessageTypeFlagsEXT
        type FieldOptional "messageType" VkDebugUtilsMessengerCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "messageType" VkDebugUtilsMessengerCreateInfoEXT =
             #{offset VkDebugUtilsMessengerCreateInfoEXT, messageType}
        type FieldIsArray "messageType" VkDebugUtilsMessengerCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCreateInfoEXT, messageType}

instance {-# OVERLAPPING #-}
         CanReadField "messageType" VkDebugUtilsMessengerCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCreateInfoEXT, messageType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, messageType}

instance {-# OVERLAPPING #-}
         CanWriteField "messageType" VkDebugUtilsMessengerCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, messageType}

instance {-# OVERLAPPING #-}
         HasField "pfnUserCallback" VkDebugUtilsMessengerCreateInfoEXT where
        type FieldType "pfnUserCallback" VkDebugUtilsMessengerCreateInfoEXT
             = PFN_vkDebugUtilsMessengerCallbackEXT
        type FieldOptional "pfnUserCallback"
               VkDebugUtilsMessengerCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pfnUserCallback"
               VkDebugUtilsMessengerCreateInfoEXT
             =
             #{offset VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback}
        type FieldIsArray "pfnUserCallback"
               VkDebugUtilsMessengerCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback}

instance {-# OVERLAPPING #-}
         CanReadField "pfnUserCallback" VkDebugUtilsMessengerCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback}

instance {-# OVERLAPPING #-}
         CanWriteField "pfnUserCallback" VkDebugUtilsMessengerCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback}

instance {-# OVERLAPPING #-}
         HasField "pUserData" VkDebugUtilsMessengerCreateInfoEXT where
        type FieldType "pUserData" VkDebugUtilsMessengerCreateInfoEXT =
             Ptr Void
        type FieldOptional "pUserData" VkDebugUtilsMessengerCreateInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pUserData" VkDebugUtilsMessengerCreateInfoEXT =
             #{offset VkDebugUtilsMessengerCreateInfoEXT, pUserData}
        type FieldIsArray "pUserData" VkDebugUtilsMessengerCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCreateInfoEXT, pUserData}

instance {-# OVERLAPPING #-}
         CanReadField "pUserData" VkDebugUtilsMessengerCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCreateInfoEXT, pUserData})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, pUserData}

instance {-# OVERLAPPING #-}
         CanWriteField "pUserData" VkDebugUtilsMessengerCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCreateInfoEXT, pUserData}

instance Show VkDebugUtilsMessengerCreateInfoEXT where
        showsPrec d x
          = showString "VkDebugUtilsMessengerCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "messageSeverity = " .
                                  showsPrec d (getField @"messageSeverity" x) .
                                    showString ", " .
                                      showString "messageType = " .
                                        showsPrec d (getField @"messageType" x) .
                                          showString ", " .
                                            showString "pfnUserCallback = " .
                                              showsPrec d (getField @"pfnUserCallback" x) .
                                                showString ", " .
                                                  showString "pUserData = " .
                                                    showsPrec d (getField @"pUserData" x) .
                                                      showChar '}'
