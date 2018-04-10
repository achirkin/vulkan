#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceQueueGlobalPriorityCreateInfoEXT
       (VkDeviceQueueGlobalPriorityCreateInfoEXT(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Base                                             (Addr##, ByteArray##,
                                                                       byteArrayContents##,
                                                                       plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkQueueGlobalPriorityEXT  (VkQueueGlobalPriorityEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo (VkDeviceQueueCreateInfo)
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceQueueGlobalPriorityCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                    pNext;
--   >     VkQueueGlobalPriorityEXT       globalPriority;
--   > } VkDeviceQueueGlobalPriorityCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDeviceQueueGlobalPriorityCreateInfoEXT VkDeviceQueueGlobalPriorityCreateInfoEXT registry at www.khronos.org>
data VkDeviceQueueGlobalPriorityCreateInfoEXT = VkDeviceQueueGlobalPriorityCreateInfoEXT## Addr##
                                                                                          ByteArray##

instance Eq VkDeviceQueueGlobalPriorityCreateInfoEXT where
        (VkDeviceQueueGlobalPriorityCreateInfoEXT## a _) ==
          x@(VkDeviceQueueGlobalPriorityCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceQueueGlobalPriorityCreateInfoEXT where
        (VkDeviceQueueGlobalPriorityCreateInfoEXT## a _) `compare`
          x@(VkDeviceQueueGlobalPriorityCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceQueueGlobalPriorityCreateInfoEXT where
        sizeOf ~_
          = #{size VkDeviceQueueGlobalPriorityCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceQueueGlobalPriorityCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        unsafeAddr (VkDeviceQueueGlobalPriorityCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceQueueGlobalPriorityCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceQueueGlobalPriorityCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        type StructFields VkDeviceQueueGlobalPriorityCreateInfoEXT =
             '["sType", "pNext", "globalPriority"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceQueueGlobalPriorityCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceQueueGlobalPriorityCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceQueueGlobalPriorityCreateInfoEXT =
             '[VkDeviceQueueCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type FieldType "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}
        type FieldIsArray "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type FieldType "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

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
        type FieldIsArray "globalPriority"
               VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

instance {-# OVERLAPPING #-}
         CanReadField "globalPriority"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

instance {-# OVERLAPPING #-}
         CanWriteField "globalPriority"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

instance Show VkDeviceQueueGlobalPriorityCreateInfoEXT where
        showsPrec d x
          = showString "VkDeviceQueueGlobalPriorityCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "globalPriority = " .
                            showsPrec d (getField @"globalPriority" x) . showChar '}'
