#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMirSurfaceCreateInfoKHR
       (VkMirSurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkMirSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include              (MirConnection,
                                                             MirSurface)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMirSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMirSurfaceCreateFlagsKHR   flags;
--   >     MirConnection*                   connection;
--   >     MirSurface*                      mirSurface;
--   > } VkMirSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMirSurfaceCreateInfoKHR.html VkMirSurfaceCreateInfoKHR registry at www.khronos.org>
data VkMirSurfaceCreateInfoKHR = VkMirSurfaceCreateInfoKHR## Addr##
                                                            ByteArray##

instance Eq VkMirSurfaceCreateInfoKHR where
        (VkMirSurfaceCreateInfoKHR## a _) ==
          x@(VkMirSurfaceCreateInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMirSurfaceCreateInfoKHR where
        (VkMirSurfaceCreateInfoKHR## a _) `compare`
          x@(VkMirSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMirSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkMirSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMirSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMirSurfaceCreateInfoKHR where
        unsafeAddr (VkMirSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMirSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMirSurfaceCreateInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMirSurfaceCreateInfoKHR where
        type StructFields VkMirSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "connection", "mirSurface"] -- ' closing tick for hsc2hs
        type CUnionType VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMirSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMirSurfaceCreateInfoKHR
         where
        type VkSTypeMType VkMirSurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMirSurfaceCreateInfoKHR where
        type FieldType "sType" VkMirSurfaceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, sType}

instance CanReadField "sType" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMirSurfaceCreateInfoKHR
         where
        type VkPNextMType VkMirSurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMirSurfaceCreateInfoKHR where
        type FieldType "pNext" VkMirSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, pNext}

instance CanReadField "pNext" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkMirSurfaceCreateInfoKHR
         where
        type VkFlagsMType VkMirSurfaceCreateInfoKHR =
             VkMirSurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMirSurfaceCreateInfoKHR where
        type FieldType "flags" VkMirSurfaceCreateInfoKHR =
             VkMirSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkMirSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, flags}

instance CanReadField "flags" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkConnection VkMirSurfaceCreateInfoKHR where
        type VkConnectionMType VkMirSurfaceCreateInfoKHR =
             Ptr MirConnection

        {-# NOINLINE vkConnection #-}
        vkConnection x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, connection})

        {-# INLINE vkConnectionByteOffset #-}
        vkConnectionByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, connection}

        {-# INLINE readVkConnection #-}
        readVkConnection p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, connection}

        {-# INLINE writeVkConnection #-}
        writeVkConnection p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         HasField "connection" VkMirSurfaceCreateInfoKHR where
        type FieldType "connection" VkMirSurfaceCreateInfoKHR =
             Ptr MirConnection
        type FieldOptional "connection" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "connection" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, connection}
        type FieldIsArray "connection" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, connection}

instance CanReadField "connection" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkConnection

        {-# INLINE readField #-}
        readField = readVkConnection

instance CanWriteField "connection" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkConnection

instance {-# OVERLAPPING #-}
         HasVkMirSurface VkMirSurfaceCreateInfoKHR where
        type VkMirSurfaceMType VkMirSurfaceCreateInfoKHR = Ptr MirSurface

        {-# NOINLINE vkMirSurface #-}
        vkMirSurface x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, mirSurface})

        {-# INLINE vkMirSurfaceByteOffset #-}
        vkMirSurfaceByteOffset ~_
          = #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

        {-# INLINE readVkMirSurface #-}
        readVkMirSurface p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

        {-# INLINE writeVkMirSurface #-}
        writeVkMirSurface p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

instance {-# OVERLAPPING #-}
         HasField "mirSurface" VkMirSurfaceCreateInfoKHR where
        type FieldType "mirSurface" VkMirSurfaceCreateInfoKHR =
             Ptr MirSurface
        type FieldOptional "mirSurface" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mirSurface" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, mirSurface}
        type FieldIsArray "mirSurface" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

instance CanReadField "mirSurface" VkMirSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkMirSurface

        {-# INLINE readField #-}
        readField = readVkMirSurface

instance CanWriteField "mirSurface" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkMirSurface

instance Show VkMirSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkMirSurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkConnection = " .
                                  showsPrec d (vkConnection x) .
                                    showString ", " .
                                      showString "vkMirSurface = " .
                                        showsPrec d (vkMirSurface x) . showChar '}'
