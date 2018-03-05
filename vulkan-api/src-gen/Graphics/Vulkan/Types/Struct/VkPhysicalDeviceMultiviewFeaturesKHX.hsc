#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewFeaturesKHX
       (VkPhysicalDeviceMultiviewFeaturesKHX(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                           (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo           (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR (VkPhysicalDeviceFeatures2KHR)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMultiviewFeaturesKHX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         multiview;
--   >     VkBool32                         multiviewGeometryShader;
--   >     VkBool32                         multiviewTessellationShader;
--   > } VkPhysicalDeviceMultiviewFeaturesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceMultiviewFeaturesKHX.html VkPhysicalDeviceMultiviewFeaturesKHX registry at www.khronos.org>
data VkPhysicalDeviceMultiviewFeaturesKHX = VkPhysicalDeviceMultiviewFeaturesKHX## Addr##
                                                                                  ByteArray##

instance Eq VkPhysicalDeviceMultiviewFeaturesKHX where
        (VkPhysicalDeviceMultiviewFeaturesKHX## a _) ==
          x@(VkPhysicalDeviceMultiviewFeaturesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewFeaturesKHX where
        (VkPhysicalDeviceMultiviewFeaturesKHX## a _) `compare`
          x@(VkPhysicalDeviceMultiviewFeaturesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewFeaturesKHX where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewFeaturesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewFeaturesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMultiviewFeaturesKHX
         where
        unsafeAddr (VkPhysicalDeviceMultiviewFeaturesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMultiviewFeaturesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewFeaturesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewFeaturesKHX where
        type StructFields VkPhysicalDeviceMultiviewFeaturesKHX =
             '["sType", "pNext", "multiview", "multiviewGeometryShader", -- ' closing tick for hsc2hs
               "multiviewTessellationShader"]
        type CUnionType VkPhysicalDeviceMultiviewFeaturesKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMultiviewFeaturesKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMultiviewFeaturesKHX =
             '[VkPhysicalDeviceFeatures2KHR, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMultiviewFeaturesKHX where
        type FieldType "sType" VkPhysicalDeviceMultiviewFeaturesKHX =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMultiviewFeaturesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMultiviewFeaturesKHX =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}
        type FieldIsArray "sType" VkPhysicalDeviceMultiviewFeaturesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceMultiviewFeaturesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceMultiviewFeaturesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMultiviewFeaturesKHX where
        type FieldType "pNext" VkPhysicalDeviceMultiviewFeaturesKHX =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMultiviewFeaturesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMultiviewFeaturesKHX =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceMultiviewFeaturesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceMultiviewFeaturesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceMultiviewFeaturesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "multiview" VkPhysicalDeviceMultiviewFeaturesKHX where
        type FieldType "multiview" VkPhysicalDeviceMultiviewFeaturesKHX =
             VkBool32
        type FieldOptional "multiview" VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "multiview" VkPhysicalDeviceMultiviewFeaturesKHX =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}
        type FieldIsArray "multiview" VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

instance {-# OVERLAPPING #-}
         CanReadField "multiview" VkPhysicalDeviceMultiviewFeaturesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

instance {-# OVERLAPPING #-}
         CanWriteField "multiview" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

instance {-# OVERLAPPING #-}
         HasField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        type FieldType "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = VkBool32
        type FieldOptional "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}
        type FieldIsArray "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

instance {-# OVERLAPPING #-}
         CanReadField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

instance {-# OVERLAPPING #-}
         CanWriteField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

instance {-# OVERLAPPING #-}
         HasField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        type FieldType "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = VkBool32
        type FieldOptional "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}
        type FieldIsArray "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

instance {-# OVERLAPPING #-}
         CanReadField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

instance {-# OVERLAPPING #-}
         CanWriteField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

instance Show VkPhysicalDeviceMultiviewFeaturesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceMultiviewFeaturesKHX {" .
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
