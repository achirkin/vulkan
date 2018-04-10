#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineTessellationDomainOriginStateCreateInfo
       (VkPipelineTessellationDomainOriginStateCreateInfo(..)) where
import           Foreign.Storable
                                                                                     (Storable (..))
import           GHC.Base
                                                                                     (Addr##,
                                                                                     ByteArray##,
                                                                                     byteArrayContents##,
                                                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                     (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkTessellationDomainOrigin
                                                                                     (VkTessellationDomainOrigin)
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
                                                                                     (VkPipelineTessellationStateCreateInfo)
import           System.IO.Unsafe
                                                                                     (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineTessellationDomainOriginStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkTessellationDomainOrigin    domainOrigin;
--   > } VkPipelineTessellationDomainOriginStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineTessellationDomainOriginStateCreateInfo VkPipelineTessellationDomainOriginStateCreateInfo registry at www.khronos.org>
data VkPipelineTessellationDomainOriginStateCreateInfo = VkPipelineTessellationDomainOriginStateCreateInfo## Addr##
                                                                                                            ByteArray##

instance Eq VkPipelineTessellationDomainOriginStateCreateInfo where
        (VkPipelineTessellationDomainOriginStateCreateInfo## a _) ==
          x@(VkPipelineTessellationDomainOriginStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineTessellationDomainOriginStateCreateInfo
         where
        (VkPipelineTessellationDomainOriginStateCreateInfo## a _) `compare`
          x@(VkPipelineTessellationDomainOriginStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineTessellationDomainOriginStateCreateInfo
         where
        sizeOf ~_
          = #{size VkPipelineTessellationDomainOriginStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineTessellationDomainOriginStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        unsafeAddr (VkPipelineTessellationDomainOriginStateCreateInfo## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineTessellationDomainOriginStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineTessellationDomainOriginStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        type StructFields VkPipelineTessellationDomainOriginStateCreateInfo
             = '["sType", "pNext", "domainOrigin"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineTessellationDomainOriginStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPipelineTessellationDomainOriginStateCreateInfo
             = '[VkPipelineTessellationStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineTessellationDomainOriginStateCreateInfo
         where
        type FieldType "sType"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineTessellationDomainOriginStateCreateInfo
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}
        type FieldIsArray "sType"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineTessellationDomainOriginStateCreateInfo
         where
        type FieldType "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfo
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}
        type FieldIsArray "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        type FieldType "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = VkTessellationDomainOrigin
        type FieldOptional "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfo
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}
        type FieldIsArray "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}

instance {-# OVERLAPPING #-}
         CanReadField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}

instance {-# OVERLAPPING #-}
         CanWriteField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfo, domainOrigin}

instance Show VkPipelineTessellationDomainOriginStateCreateInfo
         where
        showsPrec d x
          = showString "VkPipelineTessellationDomainOriginStateCreateInfo {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "domainOrigin = " .
                            showsPrec d (getField @"domainOrigin" x) . showChar '}'
