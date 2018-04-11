#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryDedicatedRequirements
       (VkMemoryDedicatedRequirements(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Base                                           (Addr##,
                                                                     ByteArray##,
                                                                     byteArrayContents##,
                                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                    (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements2 (VkMemoryRequirements2)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryDedicatedRequirements {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         prefersDedicatedAllocation;
--   >     VkBool32                         requiresDedicatedAllocation;
--   > } VkMemoryDedicatedRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryDedicatedRequirements VkMemoryDedicatedRequirements registry at www.khronos.org>
data VkMemoryDedicatedRequirements = VkMemoryDedicatedRequirements## Addr##
                                                                    ByteArray##

instance Eq VkMemoryDedicatedRequirements where
        (VkMemoryDedicatedRequirements## a _) ==
          x@(VkMemoryDedicatedRequirements## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryDedicatedRequirements where
        (VkMemoryDedicatedRequirements## a _) `compare`
          x@(VkMemoryDedicatedRequirements## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryDedicatedRequirements where
        sizeOf ~_ = #{size VkMemoryDedicatedRequirements}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryDedicatedRequirements}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryDedicatedRequirements where
        unsafeAddr (VkMemoryDedicatedRequirements## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryDedicatedRequirements## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryDedicatedRequirements##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryDedicatedRequirements where
        type StructFields VkMemoryDedicatedRequirements =
             '["sType", "pNext", "prefersDedicatedAllocation", -- ' closing tick for hsc2hs
               "requiresDedicatedAllocation"]
        type CUnionType VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryDedicatedRequirements = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryDedicatedRequirements =
             '[VkMemoryRequirements2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryDedicatedRequirements where
        type FieldType "sType" VkMemoryDedicatedRequirements =
             VkStructureType
        type FieldOptional "sType" VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryDedicatedRequirements =
             #{offset VkMemoryDedicatedRequirements, sType}
        type FieldIsArray "sType" VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirements, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryDedicatedRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirements, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedRequirements, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryDedicatedRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirements, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryDedicatedRequirements where
        type FieldType "pNext" VkMemoryDedicatedRequirements = Ptr Void
        type FieldOptional "pNext" VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryDedicatedRequirements =
             #{offset VkMemoryDedicatedRequirements, pNext}
        type FieldIsArray "pNext" VkMemoryDedicatedRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirements, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryDedicatedRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirements, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedRequirements, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryDedicatedRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirements, pNext}

instance {-# OVERLAPPING #-}
         HasField "prefersDedicatedAllocation" VkMemoryDedicatedRequirements
         where
        type FieldType "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = VkBool32
        type FieldOptional "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirements
             =
             #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}
        type FieldIsArray "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        type FieldType "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = VkBool32
        type FieldOptional "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirements
             =
             #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}
        type FieldIsArray "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}

instance Show VkMemoryDedicatedRequirements where
        showsPrec d x
          = showString "VkMemoryDedicatedRequirements {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "prefersDedicatedAllocation = " .
                            showsPrec d (getField @"prefersDedicatedAllocation" x) .
                              showString ", " .
                                showString "requiresDedicatedAllocation = " .
                                  showsPrec d (getField @"requiresDedicatedAllocation" x) .
                                    showChar '}'
