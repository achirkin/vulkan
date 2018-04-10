#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSwapchainCounterCreateInfoEXT
       (VkSwapchainCounterCreateInfoEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkSurfaceCounterFlagsEXT   (VkSurfaceCounterFlagsEXT)
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR (VkSwapchainCreateInfoKHR)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSwapchainCounterCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSurfaceCounterFlagsEXT         surfaceCounters;
--   > } VkSwapchainCounterCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkSwapchainCounterCreateInfoEXT VkSwapchainCounterCreateInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSwapchainCounterCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSwapchainCounterCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSwapchainCounterCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSwapchainCounterCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "surfaceCounters" VkSwapchainCounterCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance {-# OVERLAPPING #-}
         CanWriteField "surfaceCounters" VkSwapchainCounterCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSwapchainCounterCreateInfoEXT, surfaceCounters}

instance Show VkSwapchainCounterCreateInfoEXT where
        showsPrec d x
          = showString "VkSwapchainCounterCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "surfaceCounters = " .
                            showsPrec d (getField @"surfaceCounters" x) . showChar '}'
