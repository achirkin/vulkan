#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.Shader
       (VkShaderModuleCreateInfo(..),
        VkShaderModuleValidationCacheCreateInfoEXT(..),
        VkShaderResourceUsageAMD(..), VkShaderStatisticsInfoAMD(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           Proxy##,
                                                           byteArrayContents##,
                                                           plusAddr##, proxy##)
import           GHC.TypeLits                             (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkShaderModuleCreateFlags)
import           Graphics.Vulkan.Types.Enum.Shader        (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkValidationCacheEXT)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkShaderModuleCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkShaderModuleCreateFlags flags;
--   >     size_t                 codeSize;
--   >     const uint32_t*            pCode;
--   > } VkShaderModuleCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderModuleCreateInfo VkShaderModuleCreateInfo registry at www.khronos.org>
data VkShaderModuleCreateInfo = VkShaderModuleCreateInfo## Addr##
                                                          ByteArray##

instance Eq VkShaderModuleCreateInfo where
        (VkShaderModuleCreateInfo## a _) ==
          x@(VkShaderModuleCreateInfo## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderModuleCreateInfo where
        (VkShaderModuleCreateInfo## a _) `compare`
          x@(VkShaderModuleCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderModuleCreateInfo where
        sizeOf ~_ = #{size VkShaderModuleCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderModuleCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkShaderModuleCreateInfo where
        unsafeAddr (VkShaderModuleCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderModuleCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderModuleCreateInfo## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderModuleCreateInfo where
        type StructFields VkShaderModuleCreateInfo =
             '["sType", "pNext", "flags", "codeSize", "pCode"] -- ' closing tick for hsc2hs
        type CUnionType VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkShaderModuleCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkShaderModuleCreateInfo where
        type FieldType "sType" VkShaderModuleCreateInfo = VkStructureType
        type FieldOptional "sType" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, sType}
        type FieldIsArray "sType" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkShaderModuleCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkShaderModuleCreateInfo where
        type FieldType "pNext" VkShaderModuleCreateInfo = Ptr Void
        type FieldOptional "pNext" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, pNext}
        type FieldIsArray "pNext" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkShaderModuleCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkShaderModuleCreateInfo where
        type FieldType "flags" VkShaderModuleCreateInfo =
             VkShaderModuleCreateFlags
        type FieldOptional "flags" VkShaderModuleCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, flags}
        type FieldIsArray "flags" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkShaderModuleCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "codeSize" VkShaderModuleCreateInfo where
        type FieldType "codeSize" VkShaderModuleCreateInfo = CSize
        type FieldOptional "codeSize" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "codeSize" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, codeSize}
        type FieldIsArray "codeSize" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleCreateInfo, codeSize}

instance {-# OVERLAPPING #-}
         CanReadField "codeSize" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, codeSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, codeSize}

instance {-# OVERLAPPING #-}
         CanWriteField "codeSize" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, codeSize}

instance {-# OVERLAPPING #-}
         HasField "pCode" VkShaderModuleCreateInfo where
        type FieldType "pCode" VkShaderModuleCreateInfo = Ptr Word32
        type FieldOptional "pCode" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCode" VkShaderModuleCreateInfo =
             #{offset VkShaderModuleCreateInfo, pCode}
        type FieldIsArray "pCode" VkShaderModuleCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkShaderModuleCreateInfo, pCode}

instance {-# OVERLAPPING #-}
         CanReadField "pCode" VkShaderModuleCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleCreateInfo, pCode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleCreateInfo, pCode}

instance {-# OVERLAPPING #-}
         CanWriteField "pCode" VkShaderModuleCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleCreateInfo, pCode}

instance Show VkShaderModuleCreateInfo where
        showsPrec d x
          = showString "VkShaderModuleCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "codeSize = " .
                                  showsPrec d (getField @"codeSize" x) .
                                    showString ", " .
                                      showString "pCode = " .
                                        showsPrec d (getField @"pCode" x) . showChar '}'

-- | > typedef struct VkShaderModuleValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheEXT    validationCache;
--   > } VkShaderModuleValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderModuleValidationCacheCreateInfoEXT VkShaderModuleValidationCacheCreateInfoEXT registry at www.khronos.org>
data VkShaderModuleValidationCacheCreateInfoEXT = VkShaderModuleValidationCacheCreateInfoEXT## Addr##
                                                                                              ByteArray##

instance Eq VkShaderModuleValidationCacheCreateInfoEXT where
        (VkShaderModuleValidationCacheCreateInfoEXT## a _) ==
          x@(VkShaderModuleValidationCacheCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderModuleValidationCacheCreateInfoEXT where
        (VkShaderModuleValidationCacheCreateInfoEXT## a _) `compare`
          x@(VkShaderModuleValidationCacheCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderModuleValidationCacheCreateInfoEXT where
        sizeOf ~_
          = #{size VkShaderModuleValidationCacheCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkShaderModuleValidationCacheCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        unsafeAddr (VkShaderModuleValidationCacheCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderModuleValidationCacheCreateInfoEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderModuleValidationCacheCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderModuleValidationCacheCreateInfoEXT
         where
        type StructFields VkShaderModuleValidationCacheCreateInfoEXT =
             '["sType", "pNext", "validationCache"] -- ' closing tick for hsc2hs
        type CUnionType VkShaderModuleValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderModuleValidationCacheCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkShaderModuleValidationCacheCreateInfoEXT =
             '[VkShaderModuleCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkShaderModuleValidationCacheCreateInfoEXT where
        type FieldType "sType" VkShaderModuleValidationCacheCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkShaderModuleValidationCacheCreateInfoEXT
             =
             #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkShaderModuleValidationCacheCreateInfoEXT where
        type FieldType "pNext" VkShaderModuleValidationCacheCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkShaderModuleValidationCacheCreateInfoEXT
             =
             #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        type FieldType "validationCache"
               VkShaderModuleValidationCacheCreateInfoEXT
             = VkValidationCacheEXT
        type FieldOptional "validationCache"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "validationCache"
               VkShaderModuleValidationCacheCreateInfoEXT
             =
             #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}
        type FieldIsArray "validationCache"
               VkShaderModuleValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

instance {-# OVERLAPPING #-}
         CanReadField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

instance {-# OVERLAPPING #-}
         CanWriteField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

instance Show VkShaderModuleValidationCacheCreateInfoEXT where
        showsPrec d x
          = showString "VkShaderModuleValidationCacheCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "validationCache = " .
                            showsPrec d (getField @"validationCache" x) . showChar '}'

-- | > typedef struct VkShaderResourceUsageAMD {
--   >     uint32_t numUsedVgprs;
--   >     uint32_t numUsedSgprs;
--   >     uint32_t ldsSizePerLocalWorkGroup;
--   >     size_t ldsUsageSizeInBytes;
--   >     size_t scratchMemUsageInBytes;
--   > } VkShaderResourceUsageAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderResourceUsageAMD VkShaderResourceUsageAMD registry at www.khronos.org>
data VkShaderResourceUsageAMD = VkShaderResourceUsageAMD## Addr##
                                                          ByteArray##

instance Eq VkShaderResourceUsageAMD where
        (VkShaderResourceUsageAMD## a _) ==
          x@(VkShaderResourceUsageAMD## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderResourceUsageAMD where
        (VkShaderResourceUsageAMD## a _) `compare`
          x@(VkShaderResourceUsageAMD## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderResourceUsageAMD where
        sizeOf ~_ = #{size VkShaderResourceUsageAMD}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderResourceUsageAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkShaderResourceUsageAMD where
        unsafeAddr (VkShaderResourceUsageAMD## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderResourceUsageAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderResourceUsageAMD## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderResourceUsageAMD where
        type StructFields VkShaderResourceUsageAMD =
             '["numUsedVgprs", "numUsedSgprs", "ldsSizePerLocalWorkGroup", -- ' closing tick for hsc2hs
               "ldsUsageSizeInBytes", "scratchMemUsageInBytes"]
        type CUnionType VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderResourceUsageAMD = 'True -- ' closing tick for hsc2hs
        type StructExtends VkShaderResourceUsageAMD = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "numUsedVgprs" VkShaderResourceUsageAMD where
        type FieldType "numUsedVgprs" VkShaderResourceUsageAMD = Word32
        type FieldOptional "numUsedVgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type FieldOffset "numUsedVgprs" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, numUsedVgprs}
        type FieldIsArray "numUsedVgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance {-# OVERLAPPING #-}
         CanReadField "numUsedVgprs" VkShaderResourceUsageAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedVgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numUsedVgprs" VkShaderResourceUsageAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance {-# OVERLAPPING #-}
         HasField "numUsedSgprs" VkShaderResourceUsageAMD where
        type FieldType "numUsedSgprs" VkShaderResourceUsageAMD = Word32
        type FieldOptional "numUsedSgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type FieldOffset "numUsedSgprs" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, numUsedSgprs}
        type FieldIsArray "numUsedSgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance {-# OVERLAPPING #-}
         CanReadField "numUsedSgprs" VkShaderResourceUsageAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedSgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numUsedSgprs" VkShaderResourceUsageAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance {-# OVERLAPPING #-}
         HasField "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD where
        type FieldType "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD
             = Word32
        type FieldOptional "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             =
             #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}
        type FieldIsArray "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance {-# OVERLAPPING #-}
         CanReadField "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance {-# OVERLAPPING #-}
         CanWriteField "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance {-# OVERLAPPING #-}
         HasField "ldsUsageSizeInBytes" VkShaderResourceUsageAMD where
        type FieldType "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             CSize
        type FieldOptional "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}
        type FieldIsArray "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance {-# OVERLAPPING #-}
         CanReadField "ldsUsageSizeInBytes" VkShaderResourceUsageAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance {-# OVERLAPPING #-}
         CanWriteField "ldsUsageSizeInBytes" VkShaderResourceUsageAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance {-# OVERLAPPING #-}
         HasField "scratchMemUsageInBytes" VkShaderResourceUsageAMD where
        type FieldType "scratchMemUsageInBytes" VkShaderResourceUsageAMD =
             CSize
        type FieldOptional "scratchMemUsageInBytes"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "scratchMemUsageInBytes" VkShaderResourceUsageAMD
             =
             #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}
        type FieldIsArray "scratchMemUsageInBytes" VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance {-# OVERLAPPING #-}
         CanReadField "scratchMemUsageInBytes" VkShaderResourceUsageAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance {-# OVERLAPPING #-}
         CanWriteField "scratchMemUsageInBytes" VkShaderResourceUsageAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance Show VkShaderResourceUsageAMD where
        showsPrec d x
          = showString "VkShaderResourceUsageAMD {" .
              showString "numUsedVgprs = " .
                showsPrec d (getField @"numUsedVgprs" x) .
                  showString ", " .
                    showString "numUsedSgprs = " .
                      showsPrec d (getField @"numUsedSgprs" x) .
                        showString ", " .
                          showString "ldsSizePerLocalWorkGroup = " .
                            showsPrec d (getField @"ldsSizePerLocalWorkGroup" x) .
                              showString ", " .
                                showString "ldsUsageSizeInBytes = " .
                                  showsPrec d (getField @"ldsUsageSizeInBytes" x) .
                                    showString ", " .
                                      showString "scratchMemUsageInBytes = " .
                                        showsPrec d (getField @"scratchMemUsageInBytes" x) .
                                          showChar '}'

-- | > typedef struct VkShaderStatisticsInfoAMD {
--   >     VkShaderStageFlags shaderStageMask;
--   >     VkShaderResourceUsageAMD resourceUsage;
--   >     uint32_t numPhysicalVgprs;
--   >     uint32_t numPhysicalSgprs;
--   >     uint32_t numAvailableVgprs;
--   >     uint32_t numAvailableSgprs;
--   >     uint32_t computeWorkGroupSize[3];
--   > } VkShaderStatisticsInfoAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkShaderStatisticsInfoAMD VkShaderStatisticsInfoAMD registry at www.khronos.org>
data VkShaderStatisticsInfoAMD = VkShaderStatisticsInfoAMD## Addr##
                                                            ByteArray##

instance Eq VkShaderStatisticsInfoAMD where
        (VkShaderStatisticsInfoAMD## a _) ==
          x@(VkShaderStatisticsInfoAMD## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderStatisticsInfoAMD where
        (VkShaderStatisticsInfoAMD## a _) `compare`
          x@(VkShaderStatisticsInfoAMD## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderStatisticsInfoAMD where
        sizeOf ~_ = #{size VkShaderStatisticsInfoAMD}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderStatisticsInfoAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkShaderStatisticsInfoAMD where
        unsafeAddr (VkShaderStatisticsInfoAMD## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderStatisticsInfoAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderStatisticsInfoAMD## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderStatisticsInfoAMD where
        type StructFields VkShaderStatisticsInfoAMD =
             '["shaderStageMask", "resourceUsage", "numPhysicalVgprs", -- ' closing tick for hsc2hs
               "numPhysicalSgprs", "numAvailableVgprs", "numAvailableSgprs",
               "computeWorkGroupSize"]
        type CUnionType VkShaderStatisticsInfoAMD = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderStatisticsInfoAMD = 'True -- ' closing tick for hsc2hs
        type StructExtends VkShaderStatisticsInfoAMD = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "shaderStageMask" VkShaderStatisticsInfoAMD where
        type FieldType "shaderStageMask" VkShaderStatisticsInfoAMD =
             VkShaderStageFlags
        type FieldOptional "shaderStageMask" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStageMask" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, shaderStageMask}
        type FieldIsArray "shaderStageMask" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStageMask" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, shaderStageMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStageMask" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

instance {-# OVERLAPPING #-}
         HasField "resourceUsage" VkShaderStatisticsInfoAMD where
        type FieldType "resourceUsage" VkShaderStatisticsInfoAMD =
             VkShaderResourceUsageAMD
        type FieldOptional "resourceUsage" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceUsage" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, resourceUsage}
        type FieldIsArray "resourceUsage" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, resourceUsage}

instance {-# OVERLAPPING #-}
         CanReadField "resourceUsage" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, resourceUsage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, resourceUsage}

instance {-# OVERLAPPING #-}
         CanWriteField "resourceUsage" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, resourceUsage}

instance {-# OVERLAPPING #-}
         HasField "numPhysicalVgprs" VkShaderStatisticsInfoAMD where
        type FieldType "numPhysicalVgprs" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "numPhysicalVgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "numPhysicalVgprs" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}
        type FieldIsArray "numPhysicalVgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

instance {-# OVERLAPPING #-}
         CanReadField "numPhysicalVgprs" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numPhysicalVgprs" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

instance {-# OVERLAPPING #-}
         HasField "numPhysicalSgprs" VkShaderStatisticsInfoAMD where
        type FieldType "numPhysicalSgprs" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "numPhysicalSgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "numPhysicalSgprs" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}
        type FieldIsArray "numPhysicalSgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

instance {-# OVERLAPPING #-}
         CanReadField "numPhysicalSgprs" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numPhysicalSgprs" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

instance {-# OVERLAPPING #-}
         HasField "numAvailableVgprs" VkShaderStatisticsInfoAMD where
        type FieldType "numAvailableVgprs" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "numAvailableVgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "numAvailableVgprs" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}
        type FieldIsArray "numAvailableVgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

instance {-# OVERLAPPING #-}
         CanReadField "numAvailableVgprs" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numAvailableVgprs" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

instance {-# OVERLAPPING #-}
         HasField "numAvailableSgprs" VkShaderStatisticsInfoAMD where
        type FieldType "numAvailableSgprs" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "numAvailableSgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "numAvailableSgprs" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}
        type FieldIsArray "numAvailableSgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

instance {-# OVERLAPPING #-}
         CanReadField "numAvailableSgprs" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numAvailableSgprs" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

instance {-# OVERLAPPING #-}
         HasField "computeWorkGroupSize" VkShaderStatisticsInfoAMD where
        type FieldType "computeWorkGroupSize" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "computeWorkGroupSize" VkShaderStatisticsInfoAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "computeWorkGroupSize" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
        type FieldIsArray "computeWorkGroupSize" VkShaderStatisticsInfoAMD
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "computeWorkGroupSize" idx
            VkShaderStatisticsInfoAMD) =>
         CanReadFieldArray "computeWorkGroupSize" idx
           VkShaderStatisticsInfoAMD
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "computeWorkGroupSize" 0
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "computeWorkGroupSize" 1
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "computeWorkGroupSize" 2
                         VkShaderStatisticsInfoAMD
                       #-}
        type FieldArrayLength "computeWorkGroupSize"
               VkShaderStatisticsInfoAMD
             = 3

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 3

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
                      +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "computeWorkGroupSize" idx
            VkShaderStatisticsInfoAMD) =>
         CanWriteFieldArray "computeWorkGroupSize" idx
           VkShaderStatisticsInfoAMD
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "computeWorkGroupSize" 0
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "computeWorkGroupSize" 1
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "computeWorkGroupSize" 2
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkShaderStatisticsInfoAMD where
        showsPrec d x
          = showString "VkShaderStatisticsInfoAMD {" .
              showString "shaderStageMask = " .
                showsPrec d (getField @"shaderStageMask" x) .
                  showString ", " .
                    showString "resourceUsage = " .
                      showsPrec d (getField @"resourceUsage" x) .
                        showString ", " .
                          showString "numPhysicalVgprs = " .
                            showsPrec d (getField @"numPhysicalVgprs" x) .
                              showString ", " .
                                showString "numPhysicalSgprs = " .
                                  showsPrec d (getField @"numPhysicalSgprs" x) .
                                    showString ", " .
                                      showString "numAvailableVgprs = " .
                                        showsPrec d (getField @"numAvailableVgprs" x) .
                                          showString ", " .
                                            showString "numAvailableSgprs = " .
                                              showsPrec d (getField @"numAvailableSgprs" x) .
                                                showString ", " .
                                                  (showString "computeWorkGroupSize = [" .
                                                     showsPrec d
                                                       (let s = sizeOf
                                                                  (undefined ::
                                                                     FieldType
                                                                       "computeWorkGroupSize"
                                                                       VkShaderStatisticsInfoAMD)
                                                            o = fieldOffset @"computeWorkGroupSize"
                                                                  @VkShaderStatisticsInfoAMD
                                                            f i
                                                              = peekByteOff (unsafePtr x) i ::
                                                                  IO
                                                                    (FieldType
                                                                       "computeWorkGroupSize"
                                                                       VkShaderStatisticsInfoAMD)
                                                          in
                                                          unsafeDupablePerformIO . mapM f $
                                                            map (\ i -> o + i * s) [0 .. 3 - 1])
                                                       . showChar ']')
                                                    . showChar '}'
