#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkValidationFlagsEXT
       (VkValidationFlagsEXT(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Base                                          (Addr##,
                                                                    ByteArray##,
                                                                    byteArrayContents##,
                                                                    plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkValidationCheckEXT   (VkValidationCheckEXT)
import           Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo (VkInstanceCreateInfo)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkValidationFlagsEXT {
--   >     VkStructureType                  sType;
--   >     const void*                      pNext;
--   >     uint32_t                         disabledValidationCheckCount;
--   >     VkValidationCheckEXT* pDisabledValidationChecks;
--   > } VkValidationFlagsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkValidationFlagsEXT VkValidationFlagsEXT registry at www.khronos.org>
data VkValidationFlagsEXT = VkValidationFlagsEXT## Addr## ByteArray##

instance Eq VkValidationFlagsEXT where
        (VkValidationFlagsEXT## a _) == x@(VkValidationFlagsEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkValidationFlagsEXT where
        (VkValidationFlagsEXT## a _) `compare` x@(VkValidationFlagsEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkValidationFlagsEXT where
        sizeOf ~_ = #{size VkValidationFlagsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkValidationFlagsEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkValidationFlagsEXT where
        unsafeAddr (VkValidationFlagsEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkValidationFlagsEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkValidationFlagsEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkValidationFlagsEXT where
        type StructFields VkValidationFlagsEXT =
             '["sType", "pNext", "disabledValidationCheckCount", -- ' closing tick for hsc2hs
               "pDisabledValidationChecks"]
        type CUnionType VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkValidationFlagsEXT = '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkValidationFlagsEXT
         where
        type FieldType "sType" VkValidationFlagsEXT = VkStructureType
        type FieldOptional "sType" VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkValidationFlagsEXT =
             #{offset VkValidationFlagsEXT, sType}
        type FieldIsArray "sType" VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkValidationFlagsEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkValidationFlagsEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationFlagsEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkValidationFlagsEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationFlagsEXT, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkValidationFlagsEXT
         where
        type FieldType "pNext" VkValidationFlagsEXT = Ptr Void
        type FieldOptional "pNext" VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkValidationFlagsEXT =
             #{offset VkValidationFlagsEXT, pNext}
        type FieldIsArray "pNext" VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkValidationFlagsEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkValidationFlagsEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationFlagsEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkValidationFlagsEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationFlagsEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "disabledValidationCheckCount" VkValidationFlagsEXT where
        type FieldType "disabledValidationCheckCount" VkValidationFlagsEXT
             = Word32
        type FieldOptional "disabledValidationCheckCount"
               VkValidationFlagsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "disabledValidationCheckCount"
               VkValidationFlagsEXT
             =
             #{offset VkValidationFlagsEXT, disabledValidationCheckCount}
        type FieldIsArray "disabledValidationCheckCount"
               VkValidationFlagsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

instance {-# OVERLAPPING #-}
         CanReadField "disabledValidationCheckCount" VkValidationFlagsEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, disabledValidationCheckCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

instance {-# OVERLAPPING #-}
         CanWriteField "disabledValidationCheckCount" VkValidationFlagsEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

instance {-# OVERLAPPING #-}
         HasField "pDisabledValidationChecks" VkValidationFlagsEXT where
        type FieldType "pDisabledValidationChecks" VkValidationFlagsEXT =
             Ptr VkValidationCheckEXT
        type FieldOptional "pDisabledValidationChecks" VkValidationFlagsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDisabledValidationChecks" VkValidationFlagsEXT =
             #{offset VkValidationFlagsEXT, pDisabledValidationChecks}
        type FieldIsArray "pDisabledValidationChecks" VkValidationFlagsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

instance {-# OVERLAPPING #-}
         CanReadField "pDisabledValidationChecks" VkValidationFlagsEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, pDisabledValidationChecks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

instance {-# OVERLAPPING #-}
         CanWriteField "pDisabledValidationChecks" VkValidationFlagsEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

instance Show VkValidationFlagsEXT where
        showsPrec d x
          = showString "VkValidationFlagsEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "disabledValidationCheckCount = " .
                            showsPrec d (getField @"disabledValidationCheckCount" x) .
                              showString ", " .
                                showString "pDisabledValidationChecks = " .
                                  showsPrec d (getField @"pDisabledValidationChecks" x) .
                                    showChar '}'
