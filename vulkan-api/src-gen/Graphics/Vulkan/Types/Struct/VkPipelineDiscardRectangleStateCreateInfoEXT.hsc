#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineDiscardRectangleStateCreateInfoEXT
       (VkPipelineDiscardRectangleStateCreateInfoEXT(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Base                                                  (Addr##,
                                                                            ByteArray##,
                                                                            byteArrayContents##,
                                                                            plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                            (VkPipelineDiscardRectangleStateCreateFlagsEXT)
import           Graphics.Vulkan.Types.Enum.VkDiscardRectangleModeEXT      (VkDiscardRectangleModeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkGraphicsPipelineCreateInfo (VkGraphicsPipelineCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkRect2D                     (VkRect2D)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineDiscardRectangleStateCreateInfoEXT VkPipelineDiscardRectangleStateCreateInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

instance {-# OVERLAPPING #-}
         CanWriteField "discardRectangleMode"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleMode}

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

instance {-# OVERLAPPING #-}
         CanReadField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

instance {-# OVERLAPPING #-}
         CanWriteField "discardRectangleCount"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, discardRectangleCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance {-# OVERLAPPING #-}
         CanWriteField "pDiscardRectangles"
           VkPipelineDiscardRectangleStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDiscardRectangleStateCreateInfoEXT, pDiscardRectangles}

instance Show VkPipelineDiscardRectangleStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineDiscardRectangleStateCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "discardRectangleMode = " .
                                  showsPrec d (getField @"discardRectangleMode" x) .
                                    showString ", " .
                                      showString "discardRectangleCount = " .
                                        showsPrec d (getField @"discardRectangleCount" x) .
                                          showString ", " .
                                            showString "pDiscardRectangles = " .
                                              showsPrec d (getField @"pDiscardRectangles" x) .
                                                showChar '}'
