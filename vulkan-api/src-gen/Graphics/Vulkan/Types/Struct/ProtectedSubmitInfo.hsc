#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ProtectedSubmitInfo
       (VkProtectedSubmitInfo(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.SubmitInfo  (VkSubmitInfo)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkProtectedSubmitInfo {
--   >     VkStructureType sType;
--   >     const void*                     pNext;
--   >     VkBool32                        protectedSubmit;
--   > } VkProtectedSubmitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkProtectedSubmitInfo VkProtectedSubmitInfo registry at www.khronos.org>
data VkProtectedSubmitInfo = VkProtectedSubmitInfo## Addr##
                                                    ByteArray##

instance Eq VkProtectedSubmitInfo where
        (VkProtectedSubmitInfo## a _) == x@(VkProtectedSubmitInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkProtectedSubmitInfo where
        (VkProtectedSubmitInfo## a _) `compare`
          x@(VkProtectedSubmitInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkProtectedSubmitInfo where
        sizeOf ~_ = #{size VkProtectedSubmitInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkProtectedSubmitInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkProtectedSubmitInfo where
        unsafeAddr (VkProtectedSubmitInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkProtectedSubmitInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkProtectedSubmitInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkProtectedSubmitInfo where
        type StructFields VkProtectedSubmitInfo =
             '["sType", "pNext", "protectedSubmit"] -- ' closing tick for hsc2hs
        type CUnionType VkProtectedSubmitInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkProtectedSubmitInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkProtectedSubmitInfo = '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkProtectedSubmitInfo
         where
        type FieldType "sType" VkProtectedSubmitInfo = VkStructureType
        type FieldOptional "sType" VkProtectedSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkProtectedSubmitInfo =
             #{offset VkProtectedSubmitInfo, sType}
        type FieldIsArray "sType" VkProtectedSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkProtectedSubmitInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkProtectedSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkProtectedSubmitInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkProtectedSubmitInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkProtectedSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkProtectedSubmitInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkProtectedSubmitInfo
         where
        type FieldType "pNext" VkProtectedSubmitInfo = Ptr Void
        type FieldOptional "pNext" VkProtectedSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkProtectedSubmitInfo =
             #{offset VkProtectedSubmitInfo, pNext}
        type FieldIsArray "pNext" VkProtectedSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkProtectedSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkProtectedSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkProtectedSubmitInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkProtectedSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkProtectedSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkProtectedSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "protectedSubmit" VkProtectedSubmitInfo where
        type FieldType "protectedSubmit" VkProtectedSubmitInfo = VkBool32
        type FieldOptional "protectedSubmit" VkProtectedSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "protectedSubmit" VkProtectedSubmitInfo =
             #{offset VkProtectedSubmitInfo, protectedSubmit}
        type FieldIsArray "protectedSubmit" VkProtectedSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkProtectedSubmitInfo, protectedSubmit}

instance {-# OVERLAPPING #-}
         CanReadField "protectedSubmit" VkProtectedSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkProtectedSubmitInfo, protectedSubmit})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkProtectedSubmitInfo, protectedSubmit}

instance {-# OVERLAPPING #-}
         CanWriteField "protectedSubmit" VkProtectedSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkProtectedSubmitInfo, protectedSubmit}

instance Show VkProtectedSubmitInfo where
        showsPrec d x
          = showString "VkProtectedSubmitInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "protectedSubmit = " .
                            showsPrec d (getField @"protectedSubmit" x) . showChar '}'
