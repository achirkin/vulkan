#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSwapchainCounterCreateInfoEXT
       (VkSwapchainCounterCreateInfoEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkSurfaceCounterFlagsEXT   (VkSurfaceCounterFlagsEXT)
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR (VkSwapchainCreateInfoKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSwapchainCounterCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSurfaceCounterFlagsEXT         surfaceCounters;
--   > } VkSwapchainCounterCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSwapchainCounterCreateInfoEXT.html VkSwapchainCounterCreateInfoEXT registry at www.khronos.org>
data VkSwapchainCounterCreateInfoEXT = VkSwapchainCounterCreateInfoEXT## Addr##
                                                                        ByteArray##

instance Eq VkSwapchainCounterCreateInfoEXT where
        (VkSwapchainCounterCreateInfoEXT## a _) ==
          x@(VkSwapchainCounterCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSwapchainCounterCreateInfoEXT where
        (VkSwapchainCounterCreateInfoEXT## a _) `compare`
          x@(VkSwapchainCounterCreateInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSwapchainCounterCreateInfoEXT where
        sizeOf ~_ = #{size VkSwapchainCounterCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSwapchainCounterCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSwapchainCounterCreateInfoEXT where
        unsafeAddr (VkSwapchainCounterCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSwapchainCounterCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSwapchainCounterCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSwapchainCounterCreateInfoEXT where
        type StructFields VkSwapchainCounterCreateInfoEXT =
             '["sType", "pNext", "surfaceCounters"] -- ' closing tick for hsc2hs
        type CUnionType VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSwapchainCounterCreateInfoEXT =
             '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkSwapchainCounterCreateInfoEXT where
        type VkSTypeMType VkSwapchainCounterCreateInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSwapchainCounterCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSwapchainCounterCreateInfoEXT where
        type FieldType "sType" VkSwapchainCounterCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSwapchainCounterCreateInfoEXT =
             #{offset VkSwapchainCounterCreateInfoEXT, sType}
        type FieldIsArray "sType" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, sType}

instance CanReadField "sType" VkSwapchainCounterCreateInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSwapchainCounterCreateInfoEXT where
        type VkPNextMType VkSwapchainCounterCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSwapchainCounterCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSwapchainCounterCreateInfoEXT where
        type FieldType "pNext" VkSwapchainCounterCreateInfoEXT = Ptr Void
        type FieldOptional "pNext" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSwapchainCounterCreateInfoEXT =
             #{offset VkSwapchainCounterCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkSwapchainCounterCreateInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, pNext}

instance CanReadField "pNext" VkSwapchainCounterCreateInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSurfaceCounters VkSwapchainCounterCreateInfoEXT where
        type VkSurfaceCountersMType VkSwapchainCounterCreateInfoEXT =
             VkSurfaceCounterFlagsEXT

        {-# NOINLINE vkSurfaceCounters #-}
        vkSurfaceCounters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters})

        {-# INLINE vkSurfaceCountersByteOffset #-}
        vkSurfaceCountersByteOffset ~_
          = #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

        {-# INLINE readVkSurfaceCounters #-}
        readVkSurfaceCounters p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

        {-# INLINE writeVkSurfaceCounters #-}
        writeVkSurfaceCounters p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance {-# OVERLAPPING #-}
         HasField "surfaceCounters" VkSwapchainCounterCreateInfoEXT where
        type FieldType "surfaceCounters" VkSwapchainCounterCreateInfoEXT =
             VkSurfaceCounterFlagsEXT
        type FieldOptional "surfaceCounters"
               VkSwapchainCounterCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "surfaceCounters" VkSwapchainCounterCreateInfoEXT
             =
             #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}
        type FieldIsArray "surfaceCounters" VkSwapchainCounterCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance CanReadField "surfaceCounters"
           VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSurfaceCounters

        {-# INLINE readField #-}
        readField = readVkSurfaceCounters

instance CanWriteField "surfaceCounters"
           VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSurfaceCounters

instance Show VkSwapchainCounterCreateInfoEXT where
        showsPrec d x
          = showString "VkSwapchainCounterCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurfaceCounters = " .
                            showsPrec d (vkSurfaceCounters x) . showChar '}'
