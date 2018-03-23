#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceShaderDrawParameterFeatures
       (VkPhysicalDeviceShaderDrawParameterFeatures(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                        (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2 (VkPhysicalDeviceFeatures2)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceShaderDrawParameterFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         shaderDrawParameters;
--   > } VkPhysicalDeviceShaderDrawParameterFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceShaderDrawParameterFeatures.html VkPhysicalDeviceShaderDrawParameterFeatures registry at www.khronos.org>
data VkPhysicalDeviceShaderDrawParameterFeatures = VkPhysicalDeviceShaderDrawParameterFeatures## Addr##
                                                                                                ByteArray##

instance Eq VkPhysicalDeviceShaderDrawParameterFeatures where
        (VkPhysicalDeviceShaderDrawParameterFeatures## a _) ==
          x@(VkPhysicalDeviceShaderDrawParameterFeatures## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceShaderDrawParameterFeatures where
        (VkPhysicalDeviceShaderDrawParameterFeatures## a _) `compare`
          x@(VkPhysicalDeviceShaderDrawParameterFeatures## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceShaderDrawParameterFeatures where
        sizeOf ~_
          = #{size VkPhysicalDeviceShaderDrawParameterFeatures}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceShaderDrawParameterFeatures}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceShaderDrawParameterFeatures
         where
        unsafeAddr (VkPhysicalDeviceShaderDrawParameterFeatures## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceShaderDrawParameterFeatures## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceShaderDrawParameterFeatures##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceShaderDrawParameterFeatures
         where
        type StructFields VkPhysicalDeviceShaderDrawParameterFeatures =
             '["sType", "pNext", "shaderDrawParameters"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceShaderDrawParameterFeatures =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceShaderDrawParameterFeatures =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceShaderDrawParameterFeatures =
             '[VkPhysicalDeviceFeatures2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceShaderDrawParameterFeatures where
        type FieldType "sType" VkPhysicalDeviceShaderDrawParameterFeatures
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceShaderDrawParameterFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceShaderDrawParameterFeatures
             =
             #{offset VkPhysicalDeviceShaderDrawParameterFeatures, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceShaderDrawParameterFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderDrawParameterFeatures, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceShaderDrawParameterFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderDrawParameterFeatures, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderDrawParameterFeatures, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceShaderDrawParameterFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderDrawParameterFeatures, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceShaderDrawParameterFeatures where
        type FieldType "pNext" VkPhysicalDeviceShaderDrawParameterFeatures
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceShaderDrawParameterFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceShaderDrawParameterFeatures
             =
             #{offset VkPhysicalDeviceShaderDrawParameterFeatures, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceShaderDrawParameterFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderDrawParameterFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceShaderDrawParameterFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderDrawParameterFeatures, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderDrawParameterFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceShaderDrawParameterFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderDrawParameterFeatures, pNext}

instance {-# OVERLAPPING #-}
         HasField "shaderDrawParameters"
           VkPhysicalDeviceShaderDrawParameterFeatures
         where
        type FieldType "shaderDrawParameters"
               VkPhysicalDeviceShaderDrawParameterFeatures
             = VkBool32
        type FieldOptional "shaderDrawParameters"
               VkPhysicalDeviceShaderDrawParameterFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderDrawParameters"
               VkPhysicalDeviceShaderDrawParameterFeatures
             =
             #{offset VkPhysicalDeviceShaderDrawParameterFeatures, shaderDrawParameters}
        type FieldIsArray "shaderDrawParameters"
               VkPhysicalDeviceShaderDrawParameterFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderDrawParameterFeatures, shaderDrawParameters}

instance {-# OVERLAPPING #-}
         CanReadField "shaderDrawParameters"
           VkPhysicalDeviceShaderDrawParameterFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderDrawParameterFeatures, shaderDrawParameters})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderDrawParameterFeatures, shaderDrawParameters}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderDrawParameters"
           VkPhysicalDeviceShaderDrawParameterFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderDrawParameterFeatures, shaderDrawParameters}

instance Show VkPhysicalDeviceShaderDrawParameterFeatures where
        showsPrec d x
          = showString "VkPhysicalDeviceShaderDrawParameterFeatures {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "shaderDrawParameters = " .
                            showsPrec d (getField @"shaderDrawParameters" x) . showChar '}'
