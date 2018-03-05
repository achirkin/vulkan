#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineTessellationDomainOriginStateCreateInfoKHR
       (VkPipelineTessellationDomainOriginStateCreateInfoKHR(..)) where
import           Foreign.Storable
                                                                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                     (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkTessellationDomainOriginKHR
                                                                                     (VkTessellationDomainOriginKHR)
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
                                                                                     (VkPipelineTessellationStateCreateInfo)
import           System.IO.Unsafe
                                                                                     (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineTessellationDomainOriginStateCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkTessellationDomainOriginKHR    domainOrigin;
--   > } VkPipelineTessellationDomainOriginStateCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineTessellationDomainOriginStateCreateInfoKHR.html VkPipelineTessellationDomainOriginStateCreateInfoKHR registry at www.khronos.org>
data VkPipelineTessellationDomainOriginStateCreateInfoKHR = VkPipelineTessellationDomainOriginStateCreateInfoKHR## Addr##
                                                                                                                  ByteArray##

instance Eq VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        (VkPipelineTessellationDomainOriginStateCreateInfoKHR## a _) ==
          x@(VkPipelineTessellationDomainOriginStateCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        (VkPipelineTessellationDomainOriginStateCreateInfoKHR## a _)
          `compare`
          x@(VkPipelineTessellationDomainOriginStateCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        sizeOf ~_
          = #{size VkPipelineTessellationDomainOriginStateCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineTessellationDomainOriginStateCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        unsafeAddr
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineTessellationDomainOriginStateCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineTessellationDomainOriginStateCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type StructFields
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = '["sType", "pNext", "domainOrigin"] -- ' closing tick for hsc2hs
        type CUnionType
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = '[VkPipelineTessellationStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type FieldType "sType"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}
        type FieldIsArray "sType"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type FieldType "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}
        type FieldIsArray "pNext"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        type FieldType "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = VkTessellationDomainOriginKHR
        type FieldOptional "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             =
             #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}
        type FieldIsArray "domainOrigin"
               VkPipelineTessellationDomainOriginStateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}

instance {-# OVERLAPPING #-}
         CanReadField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}

instance {-# OVERLAPPING #-}
         CanWriteField "domainOrigin"
           VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineTessellationDomainOriginStateCreateInfoKHR, domainOrigin}

instance Show VkPipelineTessellationDomainOriginStateCreateInfoKHR
         where
        showsPrec d x
          = showString
              "VkPipelineTessellationDomainOriginStateCreateInfoKHR {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "domainOrigin = " .
                            showsPrec d (getField @"domainOrigin" x) . showChar '}'
