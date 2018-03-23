#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewFeatures
       (VkPhysicalDeviceMultiviewFeatures(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                        (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo        (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2 (VkPhysicalDeviceFeatures2)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMultiviewFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         multiview;
--   >     VkBool32                         multiviewGeometryShader;
--   >     VkBool32                         multiviewTessellationShader;
--   > } VkPhysicalDeviceMultiviewFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceMultiviewFeatures.html VkPhysicalDeviceMultiviewFeatures registry at www.khronos.org>
data VkPhysicalDeviceMultiviewFeatures = VkPhysicalDeviceMultiviewFeatures## Addr##
                                                                            ByteArray##

instance Eq VkPhysicalDeviceMultiviewFeatures where
        (VkPhysicalDeviceMultiviewFeatures## a _) ==
          x@(VkPhysicalDeviceMultiviewFeatures## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewFeatures where
        (VkPhysicalDeviceMultiviewFeatures## a _) `compare`
          x@(VkPhysicalDeviceMultiviewFeatures## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewFeatures where
        sizeOf ~_ = #{size VkPhysicalDeviceMultiviewFeatures}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewFeatures}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMultiviewFeatures where
        unsafeAddr (VkPhysicalDeviceMultiviewFeatures## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMultiviewFeatures## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewFeatures##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewFeatures where
        type StructFields VkPhysicalDeviceMultiviewFeatures =
             '["sType", "pNext", "multiview", "multiviewGeometryShader", -- ' closing tick for hsc2hs
               "multiviewTessellationShader"]
        type CUnionType VkPhysicalDeviceMultiviewFeatures = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMultiviewFeatures = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMultiviewFeatures =
             '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMultiviewFeatures where
        type FieldType "sType" VkPhysicalDeviceMultiviewFeatures =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMultiviewFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMultiviewFeatures =
             #{offset VkPhysicalDeviceMultiviewFeatures, sType}
        type FieldIsArray "sType" VkPhysicalDeviceMultiviewFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeatures, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceMultiviewFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeatures, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceMultiviewFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMultiviewFeatures where
        type FieldType "pNext" VkPhysicalDeviceMultiviewFeatures = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMultiviewFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMultiviewFeatures =
             #{offset VkPhysicalDeviceMultiviewFeatures, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceMultiviewFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceMultiviewFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeatures, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceMultiviewFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, pNext}

instance {-# OVERLAPPING #-}
         HasField "multiview" VkPhysicalDeviceMultiviewFeatures where
        type FieldType "multiview" VkPhysicalDeviceMultiviewFeatures =
             VkBool32
        type FieldOptional "multiview" VkPhysicalDeviceMultiviewFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "multiview" VkPhysicalDeviceMultiviewFeatures =
             #{offset VkPhysicalDeviceMultiviewFeatures, multiview}
        type FieldIsArray "multiview" VkPhysicalDeviceMultiviewFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeatures, multiview}

instance {-# OVERLAPPING #-}
         CanReadField "multiview" VkPhysicalDeviceMultiviewFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeatures, multiview})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, multiview}

instance {-# OVERLAPPING #-}
         CanWriteField "multiview" VkPhysicalDeviceMultiviewFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, multiview}

instance {-# OVERLAPPING #-}
         HasField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeatures
         where
        type FieldType "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeatures
             = VkBool32
        type FieldOptional "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeatures
             =
             #{offset VkPhysicalDeviceMultiviewFeatures, multiviewGeometryShader}
        type FieldIsArray "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeatures, multiviewGeometryShader}

instance {-# OVERLAPPING #-}
         CanReadField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeatures, multiviewGeometryShader})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, multiviewGeometryShader}

instance {-# OVERLAPPING #-}
         CanWriteField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, multiviewGeometryShader}

instance {-# OVERLAPPING #-}
         HasField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeatures
         where
        type FieldType "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeatures
             = VkBool32
        type FieldOptional "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeatures
             =
             #{offset VkPhysicalDeviceMultiviewFeatures, multiviewTessellationShader}
        type FieldIsArray "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeatures, multiviewTessellationShader}

instance {-# OVERLAPPING #-}
         CanReadField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeatures, multiviewTessellationShader})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, multiviewTessellationShader}

instance {-# OVERLAPPING #-}
         CanWriteField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeatures, multiviewTessellationShader}

instance Show VkPhysicalDeviceMultiviewFeatures where
        showsPrec d x
          = showString "VkPhysicalDeviceMultiviewFeatures {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "multiview = " .
                            showsPrec d (getField @"multiview" x) .
                              showString ", " .
                                showString "multiviewGeometryShader = " .
                                  showsPrec d (getField @"multiviewGeometryShader" x) .
                                    showString ", " .
                                      showString "multiviewTessellationShader = " .
                                        showsPrec d (getField @"multiviewTessellationShader" x) .
                                          showChar '}'
