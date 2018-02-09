#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_validation_cache
       (-- * Vulkan extension: @VK_EXT_validation_cache@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Cort Stratton @cdwfs@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @161@
        VkValidationCacheCreateInfoEXT(..),
        VkShaderModuleValidationCacheCreateInfoEXT(..),
        vkCreateValidationCacheEXT, vkDestroyValidationCacheEXT,
        vkMergeValidationCachesEXT, vkGetValidationCacheDataEXT,
        VK_EXT_VALIDATION_CACHE_SPEC_VERSION,
        pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION,
        VK_EXT_VALIDATION_CACHE_EXTENSION_NAME,
        pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT,
        pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheCreateFlagsEXT    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkValidationCacheCreateInfoEXT.html VkValidationCacheCreateInfoEXT registry at www.khronos.org>
data VkValidationCacheCreateInfoEXT = VkValidationCacheCreateInfoEXT## ByteArray##

instance Eq VkValidationCacheCreateInfoEXT where
        (VkValidationCacheCreateInfoEXT## a) ==
          (VkValidationCacheCreateInfoEXT## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkValidationCacheCreateInfoEXT where
        (VkValidationCacheCreateInfoEXT## a) `compare`
          (VkValidationCacheCreateInfoEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkValidationCacheCreateInfoEXT where
        sizeOf ~_ = #{size VkValidationCacheCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkValidationCacheCreateInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkValidationCacheCreateInfoEXT),
            I## a <- alignment (undefined :: VkValidationCacheCreateInfoEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkValidationCacheCreateInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkValidationCacheCreateInfoEXT## ba)
          | I## n <- sizeOf (undefined :: VkValidationCacheCreateInfoEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkValidationCacheCreateInfoEXT where
        type StructFields VkValidationCacheCreateInfoEXT =
             '["sType", "pNext", "flags", "initialDataSize", "pInitialData"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkValidationCacheCreateInfoEXT),
            I## a <- alignment (undefined :: VkValidationCacheCreateInfoEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkValidationCacheCreateInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkValidationCacheCreateInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkValidationCacheCreateInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkValidationCacheCreateInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkValidationCacheCreateInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkValidationCacheCreateInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkValidationCacheCreateInfoEXT where
        type VkSTypeMType VkValidationCacheCreateInfoEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkValidationCacheCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkValidationCacheCreateInfoEXT where
        type FieldType "sType" VkValidationCacheCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, sType}

instance CanReadField "sType" VkValidationCacheCreateInfoEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkValidationCacheCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkValidationCacheCreateInfoEXT where
        type VkPNextMType VkValidationCacheCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkValidationCacheCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkValidationCacheCreateInfoEXT where
        type FieldType "pNext" VkValidationCacheCreateInfoEXT = Ptr Void
        type FieldOptional "pNext" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, pNext}

instance CanReadField "pNext" VkValidationCacheCreateInfoEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkValidationCacheCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkValidationCacheCreateInfoEXT where
        type VkFlagsMType VkValidationCacheCreateInfoEXT =
             VkValidationCacheCreateFlagsEXT

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkValidationCacheCreateInfoEXT, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkValidationCacheCreateInfoEXT where
        type FieldType "flags" VkValidationCacheCreateInfoEXT =
             VkValidationCacheCreateFlagsEXT
        type FieldOptional "flags" VkValidationCacheCreateInfoEXT = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, flags}

instance CanReadField "flags" VkValidationCacheCreateInfoEXT where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkValidationCacheCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkInitialDataSize VkValidationCacheCreateInfoEXT where
        type VkInitialDataSizeMType VkValidationCacheCreateInfoEXT =
             #{type size_t}

        {-# NOINLINE vkInitialDataSize #-}
        vkInitialDataSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, initialDataSize})

        {-# INLINE vkInitialDataSizeByteOffset #-}
        vkInitialDataSizeByteOffset ~_
          = #{offset VkValidationCacheCreateInfoEXT, initialDataSize}

        {-# INLINE readVkInitialDataSize #-}
        readVkInitialDataSize p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, initialDataSize}

        {-# INLINE writeVkInitialDataSize #-}
        writeVkInitialDataSize p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, initialDataSize}

instance {-# OVERLAPPING #-}
         HasField "initialDataSize" VkValidationCacheCreateInfoEXT where
        type FieldType "initialDataSize" VkValidationCacheCreateInfoEXT =
             #{type size_t}
        type FieldOptional "initialDataSize" VkValidationCacheCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "initialDataSize" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, initialDataSize}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, initialDataSize}

instance CanReadField "initialDataSize"
           VkValidationCacheCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkInitialDataSize

        {-# INLINE readField #-}
        readField = readVkInitialDataSize

instance CanWriteField "initialDataSize"
           VkValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkInitialDataSize

instance {-# OVERLAPPING #-}
         HasVkPInitialData VkValidationCacheCreateInfoEXT where
        type VkPInitialDataMType VkValidationCacheCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPInitialData #-}
        vkPInitialData x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationCacheCreateInfoEXT, pInitialData})

        {-# INLINE vkPInitialDataByteOffset #-}
        vkPInitialDataByteOffset ~_
          = #{offset VkValidationCacheCreateInfoEXT, pInitialData}

        {-# INLINE readVkPInitialData #-}
        readVkPInitialData p
          = peekByteOff p #{offset VkValidationCacheCreateInfoEXT, pInitialData}

        {-# INLINE writeVkPInitialData #-}
        writeVkPInitialData p
          = pokeByteOff p #{offset VkValidationCacheCreateInfoEXT, pInitialData}

instance {-# OVERLAPPING #-}
         HasField "pInitialData" VkValidationCacheCreateInfoEXT where
        type FieldType "pInitialData" VkValidationCacheCreateInfoEXT =
             Ptr Void
        type FieldOptional "pInitialData" VkValidationCacheCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pInitialData" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, pInitialData}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationCacheCreateInfoEXT, pInitialData}

instance CanReadField "pInitialData" VkValidationCacheCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPInitialData

        {-# INLINE readField #-}
        readField = readVkPInitialData

instance CanWriteField "pInitialData"
           VkValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPInitialData

instance Show VkValidationCacheCreateInfoEXT where
        showsPrec d x
          = showString "VkValidationCacheCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkInitialDataSize = " .
                                  showsPrec d (vkInitialDataSize x) .
                                    showString ", " .
                                      showString "vkPInitialData = " .
                                        showsPrec d (vkPInitialData x) . showChar '}'

-- | > typedef struct VkShaderModuleValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheEXT    validationCache;
--   > } VkShaderModuleValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkShaderModuleValidationCacheCreateInfoEXT.html VkShaderModuleValidationCacheCreateInfoEXT registry at www.khronos.org>
data VkShaderModuleValidationCacheCreateInfoEXT = VkShaderModuleValidationCacheCreateInfoEXT## ByteArray##

instance Eq VkShaderModuleValidationCacheCreateInfoEXT where
        (VkShaderModuleValidationCacheCreateInfoEXT## a) ==
          (VkShaderModuleValidationCacheCreateInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkShaderModuleValidationCacheCreateInfoEXT where
        (VkShaderModuleValidationCacheCreateInfoEXT## a) `compare`
          (VkShaderModuleValidationCacheCreateInfoEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkShaderModuleValidationCacheCreateInfoEXT where
        sizeOf ~_
          = #{size VkShaderModuleValidationCacheCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkShaderModuleValidationCacheCreateInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkShaderModuleValidationCacheCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkShaderModuleValidationCacheCreateInfoEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkShaderModuleValidationCacheCreateInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkShaderModuleValidationCacheCreateInfoEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkShaderModuleValidationCacheCreateInfoEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkShaderModuleValidationCacheCreateInfoEXT
         where
        type StructFields VkShaderModuleValidationCacheCreateInfoEXT =
             '["sType", "pNext", "validationCache"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkShaderModuleValidationCacheCreateInfoEXT),
            I## a <- alignment
                      (undefined :: VkShaderModuleValidationCacheCreateInfoEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkShaderModuleValidationCacheCreateInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkShaderModuleValidationCacheCreateInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkShaderModuleValidationCacheCreateInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkShaderModuleValidationCacheCreateInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkShaderModuleValidationCacheCreateInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkShaderModuleValidationCacheCreateInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkShaderModuleValidationCacheCreateInfoEXT where
        type VkSTypeMType VkShaderModuleValidationCacheCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

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

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, sType}

instance CanReadField "sType"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkShaderModuleValidationCacheCreateInfoEXT where
        type VkPNextMType VkShaderModuleValidationCacheCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

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

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkValidationCache VkShaderModuleValidationCacheCreateInfoEXT
         where
        type VkValidationCacheMType
               VkShaderModuleValidationCacheCreateInfoEXT
             = VkValidationCacheEXT

        {-# NOINLINE vkValidationCache #-}
        vkValidationCache x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache})

        {-# INLINE vkValidationCacheByteOffset #-}
        vkValidationCacheByteOffset ~_
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

        {-# INLINE readVkValidationCache #-}
        readVkValidationCache p
          = peekByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

        {-# INLINE writeVkValidationCache #-}
        writeVkValidationCache p
          = pokeByteOff p #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

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

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderModuleValidationCacheCreateInfoEXT, validationCache}

instance CanReadField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkValidationCache

        {-# INLINE readField #-}
        readField = readVkValidationCache

instance CanWriteField "validationCache"
           VkShaderModuleValidationCacheCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkValidationCache

instance Show VkShaderModuleValidationCacheCreateInfoEXT where
        showsPrec d x
          = showString "VkShaderModuleValidationCacheCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkValidationCache = " .
                            showsPrec d (vkValidationCache x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateValidationCacheEXT
--   >     ( VkDevice device
--   >     , const VkValidationCacheCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkValidationCacheEXT* pValidationCache
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateValidationCacheEXT.html vkCreateValidationCacheEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCreateValidationCacheEXT"
               vkCreateValidationCacheEXT ::
               VkDevice -- ^ device
                        ->
                 Ptr VkValidationCacheCreateInfoEXT -- ^ pCreateInfo
                                                    ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkValidationCacheEXT -- ^ pValidationCache
                                              -> IO VkResult

-- | > void vkDestroyValidationCacheEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT validationCache
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyValidationCacheEXT.html vkDestroyValidationCacheEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyValidationCacheEXT"
               vkDestroyValidationCacheEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ validationCache
                                      -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                   -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkMergeValidationCachesEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT dstCache
--   >     , uint32_t srcCacheCount
--   >     , const VkValidationCacheEXT* pSrcCaches
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkMergeValidationCachesEXT.html vkMergeValidationCachesEXT registry at www.khronos.org>
foreign import ccall unsafe "vkMergeValidationCachesEXT"
               vkMergeValidationCachesEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ dstCache
                                      ->
                   Word32 -- ^ srcCacheCount
                          -> Ptr VkValidationCacheEXT -- ^ pSrcCaches
                                                      -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetValidationCacheDataEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT validationCache
--   >     , size_t* pDataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetValidationCacheDataEXT.html vkGetValidationCacheDataEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetValidationCacheDataEXT"
               vkGetValidationCacheDataEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ validationCache
                                      ->
                   Ptr #{type size_t} -- ^ pDataSize
                                                  -> Ptr Void -- ^ pData
                                                              -> IO VkResult

pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION = 1

type VK_EXT_VALIDATION_CACHE_SPEC_VERSION = 1

pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: CString

pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME <-
        (is_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME -> True)
  where VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
          = _VK_EXT_VALIDATION_CACHE_EXTENSION_NAME

_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_VALIDATION_CACHE_EXTENSION_NAME #-}
_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  = Ptr "VK_EXT_validation_cache\NUL"##

is_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME #-}
is_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  = (_VK_EXT_VALIDATION_CACHE_EXTENSION_NAME ==)

type VK_EXT_VALIDATION_CACHE_EXTENSION_NAME =
     "VK_EXT_validation_cache"

pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT =
        VkStructureType 1000160000

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
        = VkStructureType 1000160001

-- | VkValidationCacheEXT
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT :: VkObjectType

pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT =
        VkObjectType 1000160000
