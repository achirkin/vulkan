#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHX_device_group_creation
       (-- * Vulkan extension: @VK_KHX_device_group_creation@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHX@
        --
        -- type: @instance@
        --
        -- Extension number: @71@
        VkPhysicalDeviceGroupPropertiesKHX(..),
        VkDeviceGroupDeviceCreateInfoKHX(..),
        vkEnumeratePhysicalDeviceGroupsKHX,
        VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION,
        pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION,
        VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX,
        pattern VK_MAX_DEVICE_GROUP_SIZE_KHX,
        VK_MAX_DEVICE_GROUP_SIZE_KHX(),
        pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkBool32, VkInstance,
                                                   VkMemoryHeapFlagBits (..),
                                                   VkPhysicalDevice, VkResult,
                                                   VkStructureType,
                                                   VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkPhysicalDeviceGroupPropertiesKHX = VkPhysicalDeviceGroupPropertiesKHX## ByteArray##

instance Eq VkPhysicalDeviceGroupPropertiesKHX where
        (VkPhysicalDeviceGroupPropertiesKHX## a) ==
          (VkPhysicalDeviceGroupPropertiesKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceGroupPropertiesKHX where
        (VkPhysicalDeviceGroupPropertiesKHX## a) `compare`
          (VkPhysicalDeviceGroupPropertiesKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceGroupPropertiesKHX where
        sizeOf ~_ = #{size VkPhysicalDeviceGroupPropertiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceGroupPropertiesKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceGroupPropertiesKHX),
            I## a <- alignment (undefined :: VkPhysicalDeviceGroupPropertiesKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceGroupPropertiesKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceGroupPropertiesKHX## ba)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceGroupPropertiesKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceGroupPropertiesKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceGroupPropertiesKHX),
            I## a <- alignment (undefined :: VkPhysicalDeviceGroupPropertiesKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceGroupPropertiesKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceGroupPropertiesKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceGroupPropertiesKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceGroupPropertiesKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceGroupPropertiesKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceGroupPropertiesKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceGroupPropertiesKHX where
        type VkSTypeMType VkPhysicalDeviceGroupPropertiesKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceGroupPropertiesKHX where
        type VkPNextMType VkPhysicalDeviceGroupPropertiesKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasVkPhysicalDeviceCount VkPhysicalDeviceGroupPropertiesKHX where
        type VkPhysicalDeviceCountMType VkPhysicalDeviceGroupPropertiesKHX
             = Word32

        {-# NOINLINE vkPhysicalDeviceCount #-}
        vkPhysicalDeviceCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount})

        {-# INLINE vkPhysicalDeviceCountByteOffset #-}
        vkPhysicalDeviceCountByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

        {-# INLINE readVkPhysicalDeviceCount #-}
        readVkPhysicalDeviceCount p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

        {-# INLINE writeVkPhysicalDeviceCount #-}
        writeVkPhysicalDeviceCount p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         HasVkPhysicalDevicesArray VkPhysicalDeviceGroupPropertiesKHX where
        type VkPhysicalDevicesArrayMType VkPhysicalDeviceGroupPropertiesKHX
             = VkPhysicalDevice

        {-# NOINLINE vkPhysicalDevicesArray #-}
        vkPhysicalDevicesArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: VkPhysicalDevice) +
                    #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}))

        {-# INLINE vkPhysicalDevicesArrayByteOffset #-}
        vkPhysicalDevicesArrayByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}

        {-# INLINE readVkPhysicalDevicesArray #-}
        readVkPhysicalDevicesArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: VkPhysicalDevice) +
                 #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices})

        {-# INLINE writeVkPhysicalDevicesArray #-}
        writeVkPhysicalDevicesArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: VkPhysicalDevice) +
                 #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices})

instance {-# OVERLAPPING #-}
         HasVkSubsetAllocation VkPhysicalDeviceGroupPropertiesKHX where
        type VkSubsetAllocationMType VkPhysicalDeviceGroupPropertiesKHX =
             VkBool32

        {-# NOINLINE vkSubsetAllocation #-}
        vkSubsetAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation})

        {-# INLINE vkSubsetAllocationByteOffset #-}
        vkSubsetAllocationByteOffset ~_
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

        {-# INLINE readVkSubsetAllocation #-}
        readVkSubsetAllocation p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

        {-# INLINE writeVkSubsetAllocation #-}
        writeVkSubsetAllocation p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

instance Show VkPhysicalDeviceGroupPropertiesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceGroupPropertiesKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPhysicalDeviceCount = " .
                            showsPrec d (vkPhysicalDeviceCount x) .
                              showString ", " .
                                showString "vkPhysicalDevicesArray = [" .
                                  showsPrec d
                                    (map (vkPhysicalDevicesArray x)
                                       [1 .. VK_MAX_DEVICE_GROUP_SIZE_KHX])
                                    .
                                    showChar ']' .
                                      showString ", " .
                                        showString "vkSubsetAllocation = " .
                                          showsPrec d (vkSubsetAllocation x) . showChar '}'

data VkDeviceGroupDeviceCreateInfoKHX = VkDeviceGroupDeviceCreateInfoKHX## ByteArray##

instance Eq VkDeviceGroupDeviceCreateInfoKHX where
        (VkDeviceGroupDeviceCreateInfoKHX## a) ==
          (VkDeviceGroupDeviceCreateInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupDeviceCreateInfoKHX where
        (VkDeviceGroupDeviceCreateInfoKHX## a) `compare`
          (VkDeviceGroupDeviceCreateInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupDeviceCreateInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupDeviceCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupDeviceCreateInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDeviceGroupDeviceCreateInfoKHX),
            I## a <- alignment (undefined :: VkDeviceGroupDeviceCreateInfoKHX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGroupDeviceCreateInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGroupDeviceCreateInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkDeviceGroupDeviceCreateInfoKHX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGroupDeviceCreateInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDeviceGroupDeviceCreateInfoKHX),
            I## a <- alignment (undefined :: VkDeviceGroupDeviceCreateInfoKHX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGroupDeviceCreateInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGroupDeviceCreateInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDeviceGroupDeviceCreateInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGroupDeviceCreateInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGroupDeviceCreateInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGroupDeviceCreateInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupDeviceCreateInfoKHX where
        type VkSTypeMType VkDeviceGroupDeviceCreateInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupDeviceCreateInfoKHX where
        type VkPNextMType VkDeviceGroupDeviceCreateInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasVkPhysicalDeviceCount VkDeviceGroupDeviceCreateInfoKHX where
        type VkPhysicalDeviceCountMType VkDeviceGroupDeviceCreateInfoKHX =
             Word32

        {-# NOINLINE vkPhysicalDeviceCount #-}
        vkPhysicalDeviceCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount})

        {-# INLINE vkPhysicalDeviceCountByteOffset #-}
        vkPhysicalDeviceCountByteOffset ~_
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount}

        {-# INLINE readVkPhysicalDeviceCount #-}
        readVkPhysicalDeviceCount p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount}

        {-# INLINE writeVkPhysicalDeviceCount #-}
        writeVkPhysicalDeviceCount p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         HasVkPPhysicalDevices VkDeviceGroupDeviceCreateInfoKHX where
        type VkPPhysicalDevicesMType VkDeviceGroupDeviceCreateInfoKHX =
             Ptr VkPhysicalDevice

        {-# NOINLINE vkPPhysicalDevices #-}
        vkPPhysicalDevices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices})

        {-# INLINE vkPPhysicalDevicesByteOffset #-}
        vkPPhysicalDevicesByteOffset ~_
          = #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices}

        {-# INLINE readVkPPhysicalDevices #-}
        readVkPPhysicalDevices p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices}

        {-# INLINE writeVkPPhysicalDevices #-}
        writeVkPPhysicalDevices p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfoKHX, pPhysicalDevices}

instance Show VkDeviceGroupDeviceCreateInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupDeviceCreateInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPhysicalDeviceCount = " .
                            showsPrec d (vkPhysicalDeviceCount x) .
                              showString ", " .
                                showString "vkPPhysicalDevices = " .
                                  showsPrec d (vkPPhysicalDevices x) . showChar '}'

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceGroupsKHX
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceGroupCount
--   >     , VkPhysicalDeviceGroupPropertiesKHX* pPhysicalDeviceGroupProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumeratePhysicalDeviceGroupsKHX.html vkEnumeratePhysicalDeviceGroupsKHX registry at www.khronos.org>
foreign import ccall unsafe "vkEnumeratePhysicalDeviceGroupsKHX"
               vkEnumeratePhysicalDeviceGroupsKHX ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupPropertiesKHX -- ^ pPhysicalDeviceGroupProperties
                                                                      -> IO VkResult

pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

type VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString

pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME <-
        (is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME -> True)
  where VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
          = _VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME

_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString

{-# INLINE _VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}
_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = Ptr "VK_KHX_device_group_creation\NUL"##

is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}
is_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = (_VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME ==)

type VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME =
     "VK_KHX_device_group_creation"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX =
        VkStructureType 1000070000

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX =
        VkStructureType 1000070001

-- | If set, heap allocations allocate multiple instances by default
--
--   bitpos = @1@
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX ::
        VkMemoryHeapFlagBits

pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX =
        VkMemoryHeapFlagBits 2
