#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSubpassDependency
       (VkSubpassDependency(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkAccessFlags        (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.VkDependencyFlags    (VkDependencyFlags)
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags (VkPipelineStageFlags)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkSubpassDependency {
--   >     uint32_t               srcSubpass;
--   >     uint32_t               dstSubpass;
--   >     VkPipelineStageFlags   srcStageMask;
--   >     VkPipelineStageFlags   dstStageMask;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     VkDependencyFlags      dependencyFlags;
--   > } VkSubpassDependency;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkSubpassDependencyVkSubpassDependency registry at www.khronos.org>
data VkSubpassDependency = VkSubpassDependency## Addr## ByteArray##

instance Eq VkSubpassDependency where
        (VkSubpassDependency## a _) == x@(VkSubpassDependency## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSubpassDependency where
        (VkSubpassDependency## a _) `compare` x@(VkSubpassDependency## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSubpassDependency where
        sizeOf ~_ = #{size VkSubpassDependency}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubpassDependency}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSubpassDependency where
        unsafeAddr (VkSubpassDependency## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSubpassDependency## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSubpassDependency## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSubpassDependency where
        type StructFields VkSubpassDependency =
             '["srcSubpass", "dstSubpass", "srcStageMask", "dstStageMask", -- ' closing tick for hsc2hs
               "srcAccessMask", "dstAccessMask", "dependencyFlags"]
        type CUnionType VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSubpassDependency = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "srcSubpass" VkSubpassDependency where
        type FieldType "srcSubpass" VkSubpassDependency = Word32
        type FieldOptional "srcSubpass" VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSubpass" VkSubpassDependency =
             #{offset VkSubpassDependency, srcSubpass}
        type FieldIsArray "srcSubpass" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubpassDependency, srcSubpass}

instance {-# OVERLAPPING #-}
         CanReadField "srcSubpass" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcSubpass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, srcSubpass}

instance {-# OVERLAPPING #-}
         CanWriteField "srcSubpass" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, srcSubpass}

instance {-# OVERLAPPING #-}
         HasField "dstSubpass" VkSubpassDependency where
        type FieldType "dstSubpass" VkSubpassDependency = Word32
        type FieldOptional "dstSubpass" VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSubpass" VkSubpassDependency =
             #{offset VkSubpassDependency, dstSubpass}
        type FieldIsArray "dstSubpass" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubpassDependency, dstSubpass}

instance {-# OVERLAPPING #-}
         CanReadField "dstSubpass" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstSubpass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, dstSubpass}

instance {-# OVERLAPPING #-}
         CanWriteField "dstSubpass" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, dstSubpass}

instance {-# OVERLAPPING #-}
         HasField "srcStageMask" VkSubpassDependency where
        type FieldType "srcStageMask" VkSubpassDependency =
             VkPipelineStageFlags
        type FieldOptional "srcStageMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcStageMask" VkSubpassDependency =
             #{offset VkSubpassDependency, srcStageMask}
        type FieldIsArray "srcStageMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, srcStageMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcStageMask" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcStageMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, srcStageMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcStageMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, srcStageMask}

instance {-# OVERLAPPING #-}
         HasField "dstStageMask" VkSubpassDependency where
        type FieldType "dstStageMask" VkSubpassDependency =
             VkPipelineStageFlags
        type FieldOptional "dstStageMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstStageMask" VkSubpassDependency =
             #{offset VkSubpassDependency, dstStageMask}
        type FieldIsArray "dstStageMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, dstStageMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstStageMask" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstStageMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, dstStageMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstStageMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, dstStageMask}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkSubpassDependency where
        type FieldType "srcAccessMask" VkSubpassDependency = VkAccessFlags
        type FieldOptional "srcAccessMask" VkSubpassDependency = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkSubpassDependency =
             #{offset VkSubpassDependency, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcAccessMask" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAccessMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkSubpassDependency where
        type FieldType "dstAccessMask" VkSubpassDependency = VkAccessFlags
        type FieldOptional "dstAccessMask" VkSubpassDependency = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkSubpassDependency =
             #{offset VkSubpassDependency, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstAccessMask" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAccessMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, dstAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dependencyFlags" VkSubpassDependency where
        type FieldType "dependencyFlags" VkSubpassDependency =
             VkDependencyFlags
        type FieldOptional "dependencyFlags" VkSubpassDependency = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dependencyFlags" VkSubpassDependency =
             #{offset VkSubpassDependency, dependencyFlags}
        type FieldIsArray "dependencyFlags" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, dependencyFlags}

instance {-# OVERLAPPING #-}
         CanReadField "dependencyFlags" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dependencyFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, dependencyFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "dependencyFlags" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, dependencyFlags}

instance Show VkSubpassDependency where
        showsPrec d x
          = showString "VkSubpassDependency {" .
              showString "srcSubpass = " .
                showsPrec d (getField @"srcSubpass" x) .
                  showString ", " .
                    showString "dstSubpass = " .
                      showsPrec d (getField @"dstSubpass" x) .
                        showString ", " .
                          showString "srcStageMask = " .
                            showsPrec d (getField @"srcStageMask" x) .
                              showString ", " .
                                showString "dstStageMask = " .
                                  showsPrec d (getField @"dstStageMask" x) .
                                    showString ", " .
                                      showString "srcAccessMask = " .
                                        showsPrec d (getField @"srcAccessMask" x) .
                                          showString ", " .
                                            showString "dstAccessMask = " .
                                              showsPrec d (getField @"dstAccessMask" x) .
                                                showString ", " .
                                                  showString "dependencyFlags = " .
                                                    showsPrec d (getField @"dependencyFlags" x) .
                                                      showChar '}'
