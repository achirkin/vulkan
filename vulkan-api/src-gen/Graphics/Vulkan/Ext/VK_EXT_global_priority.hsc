#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_EXT_global_priority
       (-- * Vulkan extension: @VK_EXT_global_priority@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Andres Rodriguez @lostgoat@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @175@
        VkDeviceQueueGlobalPriorityCreateInfoEXT(..),
        VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION,
        pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION,
        VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME,
        pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT,
        pattern VK_ERROR_NOT_PERMITTED_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkQueueGlobalPriorityEXT,
                                                   VkResult (..),
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceQueueGlobalPriorityCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                    pNext;
--   >     VkQueueGlobalPriorityEXT       globalPriority;
--   > } VkDeviceQueueGlobalPriorityCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceQueueGlobalPriorityCreateInfoEXT.html VkDeviceQueueGlobalPriorityCreateInfoEXT registry at www.khronos.org>
data VkDeviceQueueGlobalPriorityCreateInfoEXT = VkDeviceQueueGlobalPriorityCreateInfoEXT## ByteArray##

instance Eq VkDeviceQueueGlobalPriorityCreateInfoEXT where
        (VkDeviceQueueGlobalPriorityCreateInfoEXT## a) ==
          (VkDeviceQueueGlobalPriorityCreateInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceQueueGlobalPriorityCreateInfoEXT where
        (VkDeviceQueueGlobalPriorityCreateInfoEXT## a) `compare`
          (VkDeviceQueueGlobalPriorityCreateInfoEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceQueueGlobalPriorityCreateInfoEXT where
        sizeOf ~_
          = #{size VkDeviceQueueGlobalPriorityCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceQueueGlobalPriorityCreateInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDeviceQueueGlobalPriorityCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkDeviceQueueGlobalPriorityCreateInfoEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceQueueGlobalPriorityCreateInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceQueueGlobalPriorityCreateInfoEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkDeviceQueueGlobalPriorityCreateInfoEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDeviceQueueGlobalPriorityCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkDeviceQueueGlobalPriorityCreateInfoEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceQueueGlobalPriorityCreateInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceQueueGlobalPriorityCreateInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDeviceQueueGlobalPriorityCreateInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceQueueGlobalPriorityCreateInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceQueueGlobalPriorityCreateInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceQueueGlobalPriorityCreateInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type VkSTypeMType VkDeviceQueueGlobalPriorityCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type FieldType "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

instance CanReadField "sType"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type VkPNextMType VkDeviceQueueGlobalPriorityCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type FieldType "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkGlobalPriority VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type VkGlobalPriorityMType VkDeviceQueueGlobalPriorityCreateInfoEXT
             = VkQueueGlobalPriorityEXT

        {-# NOINLINE vkGlobalPriority #-}
        vkGlobalPriority x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority})

        {-# INLINE vkGlobalPriorityByteOffset #-}
        vkGlobalPriorityByteOffset ~_
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

        {-# INLINE readVkGlobalPriority #-}
        readVkGlobalPriority p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

        {-# INLINE writeVkGlobalPriority #-}
        writeVkGlobalPriority p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

instance {-# OVERLAPPING #-}
         HasField "globalPriority" VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        type FieldType "globalPriority"
               VkDeviceQueueGlobalPriorityCreateInfoEXT
             = VkQueueGlobalPriorityEXT
        type FieldOptional "globalPriority"
               VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "globalPriority"
               VkDeviceQueueGlobalPriorityCreateInfoEXT
             =
             #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

instance CanReadField "globalPriority"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkGlobalPriority

        {-# INLINE readField #-}
        readField = readVkGlobalPriority

instance CanWriteField "globalPriority"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkGlobalPriority

instance Show VkDeviceQueueGlobalPriorityCreateInfoEXT where
        showsPrec d x
          = showString "VkDeviceQueueGlobalPriorityCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkGlobalPriority = " .
                            showsPrec d (vkGlobalPriority x) . showChar '}'

pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2

type VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2

pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: CString

pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME <-
        (is_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME -> True)
  where VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
          = _VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME

_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME #-}
_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  = Ptr "VK_EXT_global_priority\NUL"##

is_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME #-}
is_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  = (_VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME ==)

type VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME =
     "VK_EXT_global_priority"

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
        = VkStructureType 1000174000

pattern VK_ERROR_NOT_PERMITTED_EXT :: VkResult

pattern VK_ERROR_NOT_PERMITTED_EXT = VkResult (-1000174001)
