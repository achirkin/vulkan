#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DebugUtilsMessengerCallbackDataEXT
       (VkDebugUtilsMessengerCallbackDataEXT(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                           (VkDebugUtilsMessengerCallbackDataFlagsEXT)
import           Graphics.Vulkan.Types.Enum.StructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.DebugUtilsLabelEXT          (VkDebugUtilsLabelEXT)
import           Graphics.Vulkan.Types.Struct.DebugUtilsObjectNameInfoEXT (VkDebugUtilsObjectNameInfoEXT)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkDebugUtilsMessengerCallbackDataEXT {
--   >     VkStructureType sType;
--   >     const void*                                                        pNext;
--   >     VkDebugUtilsMessengerCallbackDataFlagsEXT                          flags;
--   >     const char*                                  pMessageIdName;
--   >     int32_t                                                            messageIdNumber;
--   >     const char*                                                  pMessage;
--   >     uint32_t                                                           queueLabelCount;
--   >     VkDebugUtilsLabelEXT*  pQueueLabels;
--   >     uint32_t                                                           cmdBufLabelCount;
--   >     VkDebugUtilsLabelEXT* pCmdBufLabels;
--   >     uint32_t                                                                           objectCount;
--   >     VkDebugUtilsObjectNameInfoEXT*             pObjects;
--   > } VkDebugUtilsMessengerCallbackDataEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsMessengerCallbackDataEXT VkDebugUtilsMessengerCallbackDataEXT registry at www.khronos.org>
data VkDebugUtilsMessengerCallbackDataEXT = VkDebugUtilsMessengerCallbackDataEXT## Addr##
                                                                                  ByteArray##

instance Eq VkDebugUtilsMessengerCallbackDataEXT where
        (VkDebugUtilsMessengerCallbackDataEXT## a _) ==
          x@(VkDebugUtilsMessengerCallbackDataEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugUtilsMessengerCallbackDataEXT where
        (VkDebugUtilsMessengerCallbackDataEXT## a _) `compare`
          x@(VkDebugUtilsMessengerCallbackDataEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugUtilsMessengerCallbackDataEXT where
        sizeOf ~_
          = #{size VkDebugUtilsMessengerCallbackDataEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugUtilsMessengerCallbackDataEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugUtilsMessengerCallbackDataEXT
         where
        unsafeAddr (VkDebugUtilsMessengerCallbackDataEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugUtilsMessengerCallbackDataEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugUtilsMessengerCallbackDataEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugUtilsMessengerCallbackDataEXT where
        type StructFields VkDebugUtilsMessengerCallbackDataEXT =
             '["sType", "pNext", "flags", "pMessageIdName", "messageIdNumber", -- ' closing tick for hsc2hs
               "pMessage", "queueLabelCount", "pQueueLabels", "cmdBufLabelCount",
               "pCmdBufLabels", "objectCount", "pObjects"]
        type CUnionType VkDebugUtilsMessengerCallbackDataEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugUtilsMessengerCallbackDataEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugUtilsMessengerCallbackDataEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugUtilsMessengerCallbackDataEXT where
        type FieldType "sType" VkDebugUtilsMessengerCallbackDataEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugUtilsMessengerCallbackDataEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugUtilsMessengerCallbackDataEXT =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, sType}
        type FieldIsArray "sType" VkDebugUtilsMessengerCallbackDataEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugUtilsMessengerCallbackDataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugUtilsMessengerCallbackDataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugUtilsMessengerCallbackDataEXT where
        type FieldType "pNext" VkDebugUtilsMessengerCallbackDataEXT =
             Ptr Void
        type FieldOptional "pNext" VkDebugUtilsMessengerCallbackDataEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugUtilsMessengerCallbackDataEXT =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, pNext}
        type FieldIsArray "pNext" VkDebugUtilsMessengerCallbackDataEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugUtilsMessengerCallbackDataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugUtilsMessengerCallbackDataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDebugUtilsMessengerCallbackDataEXT where
        type FieldType "flags" VkDebugUtilsMessengerCallbackDataEXT =
             VkDebugUtilsMessengerCallbackDataFlagsEXT
        type FieldOptional "flags" VkDebugUtilsMessengerCallbackDataEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDebugUtilsMessengerCallbackDataEXT =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, flags}
        type FieldIsArray "flags" VkDebugUtilsMessengerCallbackDataEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDebugUtilsMessengerCallbackDataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDebugUtilsMessengerCallbackDataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "pMessageIdName" VkDebugUtilsMessengerCallbackDataEXT
         where
        type FieldType "pMessageIdName"
               VkDebugUtilsMessengerCallbackDataEXT
             = CString
        type FieldOptional "pMessageIdName"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pMessageIdName"
               VkDebugUtilsMessengerCallbackDataEXT
             =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessageIdName}
        type FieldIsArray "pMessageIdName"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessageIdName}

instance {-# OVERLAPPING #-}
         CanReadField "pMessageIdName" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessageIdName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessageIdName}

instance {-# OVERLAPPING #-}
         CanWriteField "pMessageIdName" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessageIdName}

instance {-# OVERLAPPING #-}
         HasField "messageIdNumber" VkDebugUtilsMessengerCallbackDataEXT
         where
        type FieldType "messageIdNumber"
               VkDebugUtilsMessengerCallbackDataEXT
             = Int32
        type FieldOptional "messageIdNumber"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "messageIdNumber"
               VkDebugUtilsMessengerCallbackDataEXT
             =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, messageIdNumber}
        type FieldIsArray "messageIdNumber"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, messageIdNumber}

instance {-# OVERLAPPING #-}
         CanReadField "messageIdNumber" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, messageIdNumber})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, messageIdNumber}

instance {-# OVERLAPPING #-}
         CanWriteField "messageIdNumber"
           VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, messageIdNumber}

instance {-# OVERLAPPING #-}
         HasField "pMessage" VkDebugUtilsMessengerCallbackDataEXT where
        type FieldType "pMessage" VkDebugUtilsMessengerCallbackDataEXT =
             CString
        type FieldOptional "pMessage" VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pMessage" VkDebugUtilsMessengerCallbackDataEXT =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessage}
        type FieldIsArray "pMessage" VkDebugUtilsMessengerCallbackDataEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessage}

instance {-# OVERLAPPING #-}
         CanReadField "pMessage" VkDebugUtilsMessengerCallbackDataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessage}

instance {-# OVERLAPPING #-}
         CanWriteField "pMessage" VkDebugUtilsMessengerCallbackDataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessage}

instance {-# OVERLAPPING #-}
         HasField "queueLabelCount" VkDebugUtilsMessengerCallbackDataEXT
         where
        type FieldType "queueLabelCount"
               VkDebugUtilsMessengerCallbackDataEXT
             = Word32
        type FieldOptional "queueLabelCount"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "queueLabelCount"
               VkDebugUtilsMessengerCallbackDataEXT
             =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, queueLabelCount}
        type FieldIsArray "queueLabelCount"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, queueLabelCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueLabelCount" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, queueLabelCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, queueLabelCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueLabelCount"
           VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, queueLabelCount}

instance {-# OVERLAPPING #-}
         HasField "pQueueLabels" VkDebugUtilsMessengerCallbackDataEXT where
        type FieldType "pQueueLabels" VkDebugUtilsMessengerCallbackDataEXT
             = Ptr VkDebugUtilsLabelEXT
        type FieldOptional "pQueueLabels"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pQueueLabels"
               VkDebugUtilsMessengerCallbackDataEXT
             =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, pQueueLabels}
        type FieldIsArray "pQueueLabels"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, pQueueLabels}

instance {-# OVERLAPPING #-}
         CanReadField "pQueueLabels" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, pQueueLabels})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pQueueLabels}

instance {-# OVERLAPPING #-}
         CanWriteField "pQueueLabels" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pQueueLabels}

instance {-# OVERLAPPING #-}
         HasField "cmdBufLabelCount" VkDebugUtilsMessengerCallbackDataEXT
         where
        type FieldType "cmdBufLabelCount"
               VkDebugUtilsMessengerCallbackDataEXT
             = Word32
        type FieldOptional "cmdBufLabelCount"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "cmdBufLabelCount"
               VkDebugUtilsMessengerCallbackDataEXT
             =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, cmdBufLabelCount}
        type FieldIsArray "cmdBufLabelCount"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, cmdBufLabelCount}

instance {-# OVERLAPPING #-}
         CanReadField "cmdBufLabelCount"
           VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, cmdBufLabelCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, cmdBufLabelCount}

instance {-# OVERLAPPING #-}
         CanWriteField "cmdBufLabelCount"
           VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, cmdBufLabelCount}

instance {-# OVERLAPPING #-}
         HasField "pCmdBufLabels" VkDebugUtilsMessengerCallbackDataEXT where
        type FieldType "pCmdBufLabels" VkDebugUtilsMessengerCallbackDataEXT
             = Ptr VkDebugUtilsLabelEXT
        type FieldOptional "pCmdBufLabels"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pCmdBufLabels"
               VkDebugUtilsMessengerCallbackDataEXT
             =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, pCmdBufLabels}
        type FieldIsArray "pCmdBufLabels"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, pCmdBufLabels}

instance {-# OVERLAPPING #-}
         CanReadField "pCmdBufLabels" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, pCmdBufLabels})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pCmdBufLabels}

instance {-# OVERLAPPING #-}
         CanWriteField "pCmdBufLabels" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pCmdBufLabels}

instance {-# OVERLAPPING #-}
         HasField "objectCount" VkDebugUtilsMessengerCallbackDataEXT where
        type FieldType "objectCount" VkDebugUtilsMessengerCallbackDataEXT =
             Word32
        type FieldOptional "objectCount"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "objectCount" VkDebugUtilsMessengerCallbackDataEXT
             =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, objectCount}
        type FieldIsArray "objectCount"
               VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, objectCount}

instance {-# OVERLAPPING #-}
         CanReadField "objectCount" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, objectCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, objectCount}

instance {-# OVERLAPPING #-}
         CanWriteField "objectCount" VkDebugUtilsMessengerCallbackDataEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, objectCount}

instance {-# OVERLAPPING #-}
         HasField "pObjects" VkDebugUtilsMessengerCallbackDataEXT where
        type FieldType "pObjects" VkDebugUtilsMessengerCallbackDataEXT =
             Ptr VkDebugUtilsObjectNameInfoEXT
        type FieldOptional "pObjects" VkDebugUtilsMessengerCallbackDataEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pObjects" VkDebugUtilsMessengerCallbackDataEXT =
             #{offset VkDebugUtilsMessengerCallbackDataEXT, pObjects}
        type FieldIsArray "pObjects" VkDebugUtilsMessengerCallbackDataEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsMessengerCallbackDataEXT, pObjects}

instance {-# OVERLAPPING #-}
         CanReadField "pObjects" VkDebugUtilsMessengerCallbackDataEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsMessengerCallbackDataEXT, pObjects})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pObjects}

instance {-# OVERLAPPING #-}
         CanWriteField "pObjects" VkDebugUtilsMessengerCallbackDataEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsMessengerCallbackDataEXT, pObjects}

instance Show VkDebugUtilsMessengerCallbackDataEXT where
        showsPrec d x
          = showString "VkDebugUtilsMessengerCallbackDataEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "pMessageIdName = " .
                                  showsPrec d (getField @"pMessageIdName" x) .
                                    showString ", " .
                                      showString "messageIdNumber = " .
                                        showsPrec d (getField @"messageIdNumber" x) .
                                          showString ", " .
                                            showString "pMessage = " .
                                              showsPrec d (getField @"pMessage" x) .
                                                showString ", " .
                                                  showString "queueLabelCount = " .
                                                    showsPrec d (getField @"queueLabelCount" x) .
                                                      showString ", " .
                                                        showString "pQueueLabels = " .
                                                          showsPrec d (getField @"pQueueLabels" x) .
                                                            showString ", " .
                                                              showString "cmdBufLabelCount = " .
                                                                showsPrec d
                                                                  (getField @"cmdBufLabelCount" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "pCmdBufLabels = " .
                                                                      showsPrec d
                                                                        (getField @"pCmdBufLabels"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "objectCount = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"objectCount"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "pObjects = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"pObjects"
                                                                                       x)
                                                                                    . showChar '}'
