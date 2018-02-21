#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkValidationCacheCreateInfoEXT
       (VkValidationCacheCreateInfoEXT(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkValidationCacheCreateFlagsEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkValidationCacheCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkValidationCacheCreateFlagsEXT    flags;
--   >     size_t                 initialDataSize;
--   >     const void*            pInitialData;
--   > } VkValidationCacheCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkValidationCacheCreateInfoEXT.html VkValidationCacheCreateInfoEXT registry at www.khronos.org>
data VkValidationCacheCreateInfoEXT = VkValidationCacheCreateInfoEXT## Addr##
                                                                      ByteArray##

instance Eq VkValidationCacheCreateInfoEXT where
        (VkValidationCacheCreateInfoEXT## a _) ==
          x@(VkValidationCacheCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkValidationCacheCreateInfoEXT where
        (VkValidationCacheCreateInfoEXT## a _) `compare`
          x@(VkValidationCacheCreateInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkValidationCacheCreateInfoEXT where
        sizeOf ~_ = #{size VkValidationCacheCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkValidationCacheCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkValidationCacheCreateInfoEXT where
        unsafeAddr (VkValidationCacheCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkValidationCacheCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkValidationCacheCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkValidationCacheCreateInfoEXT where
        type StructFields VkValidationCacheCreateInfoEXT =
             '["sType", "pNext", "flags", "initialDataSize", "pInitialData"] -- ' closing tick for hsc2hs
        type CUnionType VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkValidationCacheCreateInfoEXT = '[] -- ' closing tick for hsc2hs

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
        type FieldIsArray "sType" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "flags" VkValidationCacheCreateInfoEXT = 'False -- ' closing tick for hsc2hs

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
        type VkInitialDataSizeMType VkValidationCacheCreateInfoEXT = CSize

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
             CSize
        type FieldOptional "initialDataSize" VkValidationCacheCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "initialDataSize" VkValidationCacheCreateInfoEXT =
             #{offset VkValidationCacheCreateInfoEXT, initialDataSize}
        type FieldIsArray "initialDataSize" VkValidationCacheCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pInitialData" VkValidationCacheCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

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
