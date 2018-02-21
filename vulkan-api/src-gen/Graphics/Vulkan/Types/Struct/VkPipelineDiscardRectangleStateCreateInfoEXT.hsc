#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineDiscardRectangleStateCreateInfoEXT
       (VkPipelineDiscardRectangleStateCreateInfoEXT(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                            (VkPipelineDiscardRectangleStateCreateFlagsEXT)
import           Graphics.Vulkan.Types.Enum.VkDiscardRectangleModeEXT      (VkDiscardRectangleModeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkGraphicsPipelineCreateInfo (VkGraphicsPipelineCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkRect2D                     (VkRect2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineDiscardRectangleStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineDiscardRectangleStateCreateFlagsEXT                    flags;
--   >     VkDiscardRectangleModeEXT                                                        discardRectangleMode;
--   >     uint32_t                                                         discardRectangleCount;
--   >     const VkRect2D* pDiscardRectangles;
--   > } VkPipelineDiscardRectangleStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineDiscardRectangleStateCreateInfoEXT.html VkPipelineDiscardRectangleStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineDiscardRectangleStateCreateInfoEXT = VkPipelineDiscardRectangleStateCreateInfoEXT## Addr##
                                                                                                  ByteArray##

instance Eq VkPipelineDiscardRectangleStateCreateInfoEXT where
        (VkPipelineDiscardRectangleStateCreateInfoEXT## a _) ==
          x@(VkPipelineDiscardRectangleStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDiscardRectangleStateCreateInfoEXT where
        (VkPipelineDiscardRectangleStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineDiscardRectangleStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineDiscardRectangleStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDiscardRectangleStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineDiscardRectangleStateCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineDiscardRectangleStateCreateInfoEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineDiscardRectangleStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type StructFields VkPipelineDiscardRectangleStateCreateInfoEXT =
             '["sType", "pNext", "flags", "discardRectangleMode", -- ' closing tick for hsc2hs
               "discardRectangleCount", "pDiscardRectangles"]
        type CUnionType VkPipelineDiscardRectangleStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineDiscardRectangleStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineDiscardRectangleStateCreateInfoEXT =
             '[VkGraphicsPipelineCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineDiscardRectangleStateCreateInfoEXT where
        type VkSTypeMType VkPipelineDiscardRectangleStateCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "sType" VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

instance CanReadField "sType"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineDiscardRectangleStateCreateInfoEXT where
        type VkPNextMType VkPipelineDiscardRectangleStateCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineDiscardRectangleStateCreateInfoEXT where
        type VkFlagsMType VkPipelineDiscardRectangleStateCreateInfoEXT =
             VkPipelineDiscardRectangleStateCreateFlagsEXT

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDiscardRectangleStateCreateInfoEXT where
        type FieldType "flags" VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkPipelineDiscardRectangleStateCreateFlagsEXT
        type FieldOptional "flags"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}
        type FieldIsArray "flags"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

instance CanReadField "flags"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDiscardRectangleMode
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type VkDiscardRectangleModeMType
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkDiscardRectangleModeEXT

        {-# NOINLINE vkDiscardRectangleMode #-}
        vkDiscardRectangleMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode})

        {-# INLINE vkDiscardRectangleModeByteOffset #-}
        vkDiscardRectangleModeByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

        {-# INLINE readVkDiscardRectangleMode #-}
        readVkDiscardRectangleMode p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

        {-# INLINE writeVkDiscardRectangleMode #-}
        writeVkDiscardRectangleMode p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

instance {-# OVERLAPPING #-}
         HasField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = VkDiscardRectangleModeEXT
        type FieldOptional "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}
        type FieldIsArray "discardRectangleMode"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

instance CanReadField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkDiscardRectangleMode

        {-# INLINE readField #-}
        readField = readVkDiscardRectangleMode

instance CanWriteField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDiscardRectangleMode

instance {-# OVERLAPPING #-}
         HasVkDiscardRectangleCount
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type VkDiscardRectangleCountMType
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Word32

        {-# NOINLINE vkDiscardRectangleCount #-}
        vkDiscardRectangleCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount})

        {-# INLINE vkDiscardRectangleCountByteOffset #-}
        vkDiscardRectangleCountByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

        {-# INLINE readVkDiscardRectangleCount #-}
        readVkDiscardRectangleCount p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

        {-# INLINE writeVkDiscardRectangleCount #-}
        writeVkDiscardRectangleCount p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

instance {-# OVERLAPPING #-}
         HasField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Word32
        type FieldOptional "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}
        type FieldIsArray "discardRectangleCount"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

instance CanReadField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkDiscardRectangleCount

        {-# INLINE readField #-}
        readField = readVkDiscardRectangleCount

instance CanWriteField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDiscardRectangleCount

instance {-# OVERLAPPING #-}
         HasVkPDiscardRectangles
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type VkPDiscardRectanglesMType
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Ptr VkRect2D

        {-# NOINLINE vkPDiscardRectangles #-}
        vkPDiscardRectangles x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles})

        {-# INLINE vkPDiscardRectanglesByteOffset #-}
        vkPDiscardRectanglesByteOffset ~_
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

        {-# INLINE readVkPDiscardRectangles #-}
        readVkPDiscardRectangles p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

        {-# INLINE writeVkPDiscardRectangles #-}
        writeVkPDiscardRectangles p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance {-# OVERLAPPING #-}
         HasField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        type FieldType "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = Ptr VkRect2D
        type FieldOptional "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             =
             #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}
        type FieldIsArray "pDiscardRectangles"
               VkPipelineDiscardRectangleStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance CanReadField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPDiscardRectangles

        {-# INLINE readField #-}
        readField = readVkPDiscardRectangles

instance CanWriteField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDiscardRectangles

instance Show VkPipelineDiscardRectangleStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineDiscardRectangleStateCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDiscardRectangleMode = " .
                                  showsPrec d (vkDiscardRectangleMode x) .
                                    showString ", " .
                                      showString "vkDiscardRectangleCount = " .
                                        showsPrec d (vkDiscardRectangleCount x) .
                                          showString ", " .
                                            showString "vkPDiscardRectangles = " .
                                              showsPrec d (vkPDiscardRectangles x) . showChar '}'
