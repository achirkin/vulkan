#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkPhysicalDeviceMultiviewFeaturesKHX where
        type VkSTypeMType VkPhysicalDeviceMultiviewFeaturesKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

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

instance CanReadField "sType" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceMultiviewFeaturesKHX where
        type VkPNextMType VkPhysicalDeviceMultiviewFeaturesKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

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

instance CanReadField "pNext" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMultiview VkPhysicalDeviceMultiviewFeaturesKHX where
        type VkMultiviewMType VkPhysicalDeviceMultiviewFeaturesKHX =
             VkBool32

        {-# NOINLINE vkMultiview #-}
        vkMultiview x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview})

        {-# INLINE vkMultiviewByteOffset #-}
        vkMultiviewByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

        {-# INLINE readVkMultiview #-}
        readVkMultiview p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

        {-# INLINE writeVkMultiview #-}
        writeVkMultiview p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

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

instance CanReadField "multiview"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkMultiview

        {-# INLINE readField #-}
        readField = readVkMultiview

instance CanWriteField "multiview"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMultiview

instance {-# OVERLAPPING #-}
         HasVkMultiviewGeometryShader VkPhysicalDeviceMultiviewFeaturesKHX
         where
        type VkMultiviewGeometryShaderMType
               VkPhysicalDeviceMultiviewFeaturesKHX
             = VkBool32

        {-# NOINLINE vkMultiviewGeometryShader #-}
        vkMultiviewGeometryShader x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader})

        {-# INLINE vkMultiviewGeometryShaderByteOffset #-}
        vkMultiviewGeometryShaderByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

        {-# INLINE readVkMultiviewGeometryShader #-}
        readVkMultiviewGeometryShader p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

        {-# INLINE writeVkMultiviewGeometryShader #-}
        writeVkMultiviewGeometryShader p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

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

instance CanReadField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkMultiviewGeometryShader

        {-# INLINE readField #-}
        readField = readVkMultiviewGeometryShader

instance CanWriteField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMultiviewGeometryShader

instance {-# OVERLAPPING #-}
         HasVkMultiviewTessellationShader
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        type VkMultiviewTessellationShaderMType
               VkPhysicalDeviceMultiviewFeaturesKHX
             = VkBool32

        {-# NOINLINE vkMultiviewTessellationShader #-}
        vkMultiviewTessellationShader x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader})

        {-# INLINE vkMultiviewTessellationShaderByteOffset #-}
        vkMultiviewTessellationShaderByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

        {-# INLINE readVkMultiviewTessellationShader #-}
        readVkMultiviewTessellationShader p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

        {-# INLINE writeVkMultiviewTessellationShader #-}
        writeVkMultiviewTessellationShader p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

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

instance CanReadField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkMultiviewTessellationShader

        {-# INLINE readField #-}
        readField = readVkMultiviewTessellationShader

instance CanWriteField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMultiviewTessellationShader

instance Show VkPhysicalDeviceMultiviewFeaturesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceMultiviewFeaturesKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMultiview = " .
                            showsPrec d (vkMultiview x) .
                              showString ", " .
                                showString "vkMultiviewGeometryShader = " .
                                  showsPrec d (vkMultiviewGeometryShader x) .
                                    showString ", " .
                                      showString "vkMultiviewTessellationShader = " .
                                        showsPrec d (vkMultiviewTessellationShader x) . showChar '}'
