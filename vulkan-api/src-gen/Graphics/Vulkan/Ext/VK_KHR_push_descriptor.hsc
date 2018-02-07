#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_push_descriptor
       (-- * Vulkan extension: @VK_KHR_push_descriptor@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @81@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDevicePushDescriptorPropertiesKHR(..),
        vkCmdPushDescriptorSetKHR, VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION,
        pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION,
        VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME,
        pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR,
        pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkWriteDescriptorSet)
import           Graphics.Vulkan.Common           (VkCommandBuffer, VkDescriptorSetLayoutCreateFlagBits (..),
                                                   VkPipelineBindPoint,
                                                   VkPipelineLayout,
                                                   VkStructureType,
                                                   VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkPhysicalDevicePushDescriptorPropertiesKHR = VkPhysicalDevicePushDescriptorPropertiesKHR## ByteArray##

instance Eq VkPhysicalDevicePushDescriptorPropertiesKHR where
        (VkPhysicalDevicePushDescriptorPropertiesKHR## a) ==
          (VkPhysicalDevicePushDescriptorPropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevicePushDescriptorPropertiesKHR where
        (VkPhysicalDevicePushDescriptorPropertiesKHR## a) `compare`
          (VkPhysicalDevicePushDescriptorPropertiesKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevicePushDescriptorPropertiesKHR where
        sizeOf ~_
          = #{size VkPhysicalDevicePushDescriptorPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevicePushDescriptorPropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDevicePushDescriptorPropertiesKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDevicePushDescriptorPropertiesKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDevicePushDescriptorPropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDevicePushDescriptorPropertiesKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDevicePushDescriptorPropertiesKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDevicePushDescriptorPropertiesKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDevicePushDescriptorPropertiesKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDevicePushDescriptorPropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDevicePushDescriptorPropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDevicePushDescriptorPropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDevicePushDescriptorPropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDevicePushDescriptorPropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDevicePushDescriptorPropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDevicePushDescriptorPropertiesKHR where
        type VkSTypeMType VkPhysicalDevicePushDescriptorPropertiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDevicePushDescriptorPropertiesKHR where
        type VkPNextMType VkPhysicalDevicePushDescriptorPropertiesKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkMaxPushDescriptors VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        type VkMaxPushDescriptorsMType
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = Word32

        {-# NOINLINE vkMaxPushDescriptors #-}
        vkMaxPushDescriptors x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors})

        {-# INLINE vkMaxPushDescriptorsByteOffset #-}
        vkMaxPushDescriptorsByteOffset ~_
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

        {-# INLINE readVkMaxPushDescriptors #-}
        readVkMaxPushDescriptors p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

        {-# INLINE writeVkMaxPushDescriptors #-}
        writeVkMaxPushDescriptors p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

instance Show VkPhysicalDevicePushDescriptorPropertiesKHR where
        showsPrec d x
          = showString "VkPhysicalDevicePushDescriptorPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxPushDescriptors = " .
                            showsPrec d (vkMaxPushDescriptors x) . showChar '}'

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdPushDescriptorSetKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdPushDescriptorSetKHR.html vkCmdPushDescriptorSetKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPushDescriptorSetKHR"
               vkCmdPushDescriptorSetKHR ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineBindPoint -- ^ pipelineBindPoint
                                     ->
                   VkPipelineLayout -- ^ layout
                                    ->
                     Word32 -- ^ set
                            -> Word32 -- ^ descriptorWriteCount
                                      -> Ptr VkWriteDescriptorSet -- ^ pDescriptorWrites
                                                                  -> IO ()

pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 1

type VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 1

pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: CString

pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME <-
        (is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME -> True)
  where VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
          = _VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME

_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME #-}
_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  = Ptr "VK_KHR_push_descriptor\NUL"##

is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME #-}
is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  = (_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME ==)

type VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME =
     "VK_KHR_push_descriptor"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
        = VkStructureType 1000080000

-- | Descriptors are pushed via flink:vkCmdPushDescriptorSetKHR
--
--   bitpos = @0@
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR ::
        VkDescriptorSetLayoutCreateFlagBits

pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR =
        VkDescriptorSetLayoutCreateFlagBits 1
