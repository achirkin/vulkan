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
module Graphics.Vulkan.Types.Struct.Debug
       (VkDebugMarkerMarkerInfoEXT(..),
        VkDebugMarkerObjectNameInfoEXT(..),
        VkDebugMarkerObjectTagInfoEXT(..),
        VkDebugReportCallbackCreateInfoEXT(..),
        VkDebugUtilsObjectTagInfoEXT(..))
       where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  Proxy##,
                                                                  byteArrayContents##,
                                                                  plusAddr##,
                                                                  proxy##)
import           GHC.TypeLits                                    (KnownNat,
                                                                  natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Debug                (VkDebugReportFlagsEXT,
                                                                  VkDebugReportObjectTypeEXT)
import           Graphics.Vulkan.Types.Enum.Object               (VkObjectType)
import           Graphics.Vulkan.Types.Enum.StructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Funcpointers              (PFN_vkDebugReportCallbackEXT)
import           Graphics.Vulkan.Types.Struct.InstanceCreateInfo (VkInstanceCreateInfo)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkDebugMarkerMarkerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const char* pMarkerName;
--   >     float            color[4];
--   > } VkDebugMarkerMarkerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugMarkerMarkerInfoEXT VkDebugMarkerMarkerInfoEXT registry at www.khronos.org>
data VkDebugMarkerMarkerInfoEXT = VkDebugMarkerMarkerInfoEXT## Addr##
                                                              ByteArray##

instance Eq VkDebugMarkerMarkerInfoEXT where
        (VkDebugMarkerMarkerInfoEXT## a _) ==
          x@(VkDebugMarkerMarkerInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerMarkerInfoEXT where
        (VkDebugMarkerMarkerInfoEXT## a _) `compare`
          x@(VkDebugMarkerMarkerInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerMarkerInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerMarkerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDebugMarkerMarkerInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugMarkerMarkerInfoEXT where
        unsafeAddr (VkDebugMarkerMarkerInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugMarkerMarkerInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugMarkerMarkerInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugMarkerMarkerInfoEXT where
        type StructFields VkDebugMarkerMarkerInfoEXT =
             '["sType", "pNext", "pMarkerName", "color"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugMarkerMarkerInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugMarkerMarkerInfoEXT where
        type FieldType "sType" VkDebugMarkerMarkerInfoEXT = VkStructureType
        type FieldOptional "sType" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugMarkerMarkerInfoEXT =
             #{offset VkDebugMarkerMarkerInfoEXT, sType}
        type FieldIsArray "sType" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerMarkerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugMarkerMarkerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugMarkerMarkerInfoEXT where
        type FieldType "pNext" VkDebugMarkerMarkerInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugMarkerMarkerInfoEXT =
             #{offset VkDebugMarkerMarkerInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerMarkerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugMarkerMarkerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pMarkerName" VkDebugMarkerMarkerInfoEXT where
        type FieldType "pMarkerName" VkDebugMarkerMarkerInfoEXT = CString
        type FieldOptional "pMarkerName" VkDebugMarkerMarkerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pMarkerName" VkDebugMarkerMarkerInfoEXT =
             #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}
        type FieldIsArray "pMarkerName" VkDebugMarkerMarkerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

instance {-# OVERLAPPING #-}
         CanReadField "pMarkerName" VkDebugMarkerMarkerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

instance {-# OVERLAPPING #-}
         CanWriteField "pMarkerName" VkDebugMarkerMarkerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}

instance {-# OVERLAPPING #-}
         HasField "color" VkDebugMarkerMarkerInfoEXT where
        type FieldType "color" VkDebugMarkerMarkerInfoEXT =
             #{type float}
        type FieldOptional "color" VkDebugMarkerMarkerInfoEXT = 'True -- ' closing tick for hsc2hs
        type FieldOffset "color" VkDebugMarkerMarkerInfoEXT =
             #{offset VkDebugMarkerMarkerInfoEXT, color}
        type FieldIsArray "color" VkDebugMarkerMarkerInfoEXT = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerMarkerInfoEXT, color}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "color" idx VkDebugMarkerMarkerInfoEXT) =>
         CanReadFieldArray "color" idx VkDebugMarkerMarkerInfoEXT
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "color" 0 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 1 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 2 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "color" 3 VkDebugMarkerMarkerInfoEXT #-}
        type FieldArrayLength "color" VkDebugMarkerMarkerInfoEXT = 4

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 4

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkDebugMarkerMarkerInfoEXT, color} +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkDebugMarkerMarkerInfoEXT, color} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "color" idx VkDebugMarkerMarkerInfoEXT) =>
         CanWriteFieldArray "color" idx VkDebugMarkerMarkerInfoEXT
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 0 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 1 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 2 VkDebugMarkerMarkerInfoEXT #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "color" 3 VkDebugMarkerMarkerInfoEXT #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkDebugMarkerMarkerInfoEXT, color} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkDebugMarkerMarkerInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerMarkerInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pMarkerName = " .
                            showsPrec d (getField @"pMarkerName" x) .
                              showString ", " .
                                (showString "color = [" .
                                   showsPrec d
                                     (let s = sizeOf
                                                (undefined ::
                                                   FieldType "color" VkDebugMarkerMarkerInfoEXT)
                                          o = fieldOffset @"color" @VkDebugMarkerMarkerInfoEXT
                                          f i
                                            = peekByteOff (unsafePtr x) i ::
                                                IO (FieldType "color" VkDebugMarkerMarkerInfoEXT)
                                        in
                                        unsafeDupablePerformIO . mapM f $
                                          map (\ i -> o + i * s) [0 .. 4 - 1])
                                     . showChar ']')
                                  . showChar '}'

-- | > typedef struct VkDebugMarkerObjectNameInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportObjectTypeEXT       objectType;
--   >     uint64_t                         object;
--   >     const char* pObjectName;
--   > } VkDebugMarkerObjectNameInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugMarkerObjectNameInfoEXT VkDebugMarkerObjectNameInfoEXT registry at www.khronos.org>
data VkDebugMarkerObjectNameInfoEXT = VkDebugMarkerObjectNameInfoEXT## Addr##
                                                                      ByteArray##

instance Eq VkDebugMarkerObjectNameInfoEXT where
        (VkDebugMarkerObjectNameInfoEXT## a _) ==
          x@(VkDebugMarkerObjectNameInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerObjectNameInfoEXT where
        (VkDebugMarkerObjectNameInfoEXT## a _) `compare`
          x@(VkDebugMarkerObjectNameInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerObjectNameInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerObjectNameInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugMarkerObjectNameInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugMarkerObjectNameInfoEXT where
        unsafeAddr (VkDebugMarkerObjectNameInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugMarkerObjectNameInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugMarkerObjectNameInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugMarkerObjectNameInfoEXT where
        type StructFields VkDebugMarkerObjectNameInfoEXT =
             '["sType", "pNext", "objectType", "object", "pObjectName"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugMarkerObjectNameInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "sType" VkDebugMarkerObjectNameInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, sType}
        type FieldIsArray "sType" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "pNext" VkDebugMarkerObjectNameInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "objectType" VkDebugMarkerObjectNameInfoEXT =
             VkDebugReportObjectTypeEXT
        type FieldOptional "objectType" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, objectType}
        type FieldIsArray "objectType" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanReadField "objectType" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, objectType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanWriteField "objectType" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "object" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "object" VkDebugMarkerObjectNameInfoEXT = Word64
        type FieldOptional "object" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "object" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, object}
        type FieldIsArray "object" VkDebugMarkerObjectNameInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance {-# OVERLAPPING #-}
         CanReadField "object" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, object})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance {-# OVERLAPPING #-}
         CanWriteField "object" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, object}

instance {-# OVERLAPPING #-}
         HasField "pObjectName" VkDebugMarkerObjectNameInfoEXT where
        type FieldType "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             CString
        type FieldOptional "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}
        type FieldIsArray "pObjectName" VkDebugMarkerObjectNameInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance {-# OVERLAPPING #-}
         CanReadField "pObjectName" VkDebugMarkerObjectNameInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance {-# OVERLAPPING #-}
         CanWriteField "pObjectName" VkDebugMarkerObjectNameInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}

instance Show VkDebugMarkerObjectNameInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerObjectNameInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectType = " .
                            showsPrec d (getField @"objectType" x) .
                              showString ", " .
                                showString "object = " .
                                  showsPrec d (getField @"object" x) .
                                    showString ", " .
                                      showString "pObjectName = " .
                                        showsPrec d (getField @"pObjectName" x) . showChar '}'

-- | > typedef struct VkDebugMarkerObjectTagInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportObjectTypeEXT       objectType;
--   >     uint64_t                         object;
--   >     uint64_t                         tagName;
--   >     size_t                           tagSize;
--   >     const void*        pTag;
--   > } VkDebugMarkerObjectTagInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugMarkerObjectTagInfoEXT VkDebugMarkerObjectTagInfoEXT registry at www.khronos.org>
data VkDebugMarkerObjectTagInfoEXT = VkDebugMarkerObjectTagInfoEXT## Addr##
                                                                    ByteArray##

instance Eq VkDebugMarkerObjectTagInfoEXT where
        (VkDebugMarkerObjectTagInfoEXT## a _) ==
          x@(VkDebugMarkerObjectTagInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugMarkerObjectTagInfoEXT where
        (VkDebugMarkerObjectTagInfoEXT## a _) `compare`
          x@(VkDebugMarkerObjectTagInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugMarkerObjectTagInfoEXT where
        sizeOf ~_ = #{size VkDebugMarkerObjectTagInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugMarkerObjectTagInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugMarkerObjectTagInfoEXT where
        unsafeAddr (VkDebugMarkerObjectTagInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugMarkerObjectTagInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugMarkerObjectTagInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugMarkerObjectTagInfoEXT where
        type StructFields VkDebugMarkerObjectTagInfoEXT =
             '["sType", "pNext", "objectType", "object", "tagName", "tagSize", -- ' closing tick for hsc2hs
               "pTag"]
        type CUnionType VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugMarkerObjectTagInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "sType" VkDebugMarkerObjectTagInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, sType}
        type FieldIsArray "sType" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "pNext" VkDebugMarkerObjectTagInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "objectType" VkDebugMarkerObjectTagInfoEXT =
             VkDebugReportObjectTypeEXT
        type FieldOptional "objectType" VkDebugMarkerObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, objectType}
        type FieldIsArray "objectType" VkDebugMarkerObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanReadField "objectType" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, objectType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanWriteField "objectType" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "object" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "object" VkDebugMarkerObjectTagInfoEXT = Word64
        type FieldOptional "object" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "object" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, object}
        type FieldIsArray "object" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance {-# OVERLAPPING #-}
         CanReadField "object" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, object})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance {-# OVERLAPPING #-}
         CanWriteField "object" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, object}

instance {-# OVERLAPPING #-}
         HasField "tagName" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "tagName" VkDebugMarkerObjectTagInfoEXT = Word64
        type FieldOptional "tagName" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagName" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, tagName}
        type FieldIsArray "tagName" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         CanReadField "tagName" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, tagName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         CanWriteField "tagName" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         HasField "tagSize" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "tagSize" VkDebugMarkerObjectTagInfoEXT = CSize
        type FieldOptional "tagSize" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagSize" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}
        type FieldIsArray "tagSize" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         CanReadField "tagSize" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, tagSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         CanWriteField "tagSize" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         HasField "pTag" VkDebugMarkerObjectTagInfoEXT where
        type FieldType "pTag" VkDebugMarkerObjectTagInfoEXT = Ptr Void
        type FieldOptional "pTag" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pTag" VkDebugMarkerObjectTagInfoEXT =
             #{offset VkDebugMarkerObjectTagInfoEXT, pTag}
        type FieldIsArray "pTag" VkDebugMarkerObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         CanReadField "pTag" VkDebugMarkerObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugMarkerObjectTagInfoEXT, pTag})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         CanWriteField "pTag" VkDebugMarkerObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugMarkerObjectTagInfoEXT, pTag}

instance Show VkDebugMarkerObjectTagInfoEXT where
        showsPrec d x
          = showString "VkDebugMarkerObjectTagInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectType = " .
                            showsPrec d (getField @"objectType" x) .
                              showString ", " .
                                showString "object = " .
                                  showsPrec d (getField @"object" x) .
                                    showString ", " .
                                      showString "tagName = " .
                                        showsPrec d (getField @"tagName" x) .
                                          showString ", " .
                                            showString "tagSize = " .
                                              showsPrec d (getField @"tagSize" x) .
                                                showString ", " .
                                                  showString "pTag = " .
                                                    showsPrec d (getField @"pTag" x) . showChar '}'

-- | > typedef struct VkDebugReportCallbackCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportFlagsEXT            flags;
--   >     PFN_vkDebugReportCallbackEXT     pfnCallback;
--   >     void*            pUserData;
--   > } VkDebugReportCallbackCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXT registry at www.khronos.org>
data VkDebugReportCallbackCreateInfoEXT = VkDebugReportCallbackCreateInfoEXT## Addr##
                                                                              ByteArray##

instance Eq VkDebugReportCallbackCreateInfoEXT where
        (VkDebugReportCallbackCreateInfoEXT## a _) ==
          x@(VkDebugReportCallbackCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugReportCallbackCreateInfoEXT where
        (VkDebugReportCallbackCreateInfoEXT## a _) `compare`
          x@(VkDebugReportCallbackCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugReportCallbackCreateInfoEXT where
        sizeOf ~_ = #{size VkDebugReportCallbackCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugReportCallbackCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugReportCallbackCreateInfoEXT where
        unsafeAddr (VkDebugReportCallbackCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugReportCallbackCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugReportCallbackCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugReportCallbackCreateInfoEXT where
        type StructFields VkDebugReportCallbackCreateInfoEXT =
             '["sType", "pNext", "flags", "pfnCallback", "pUserData"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugReportCallbackCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugReportCallbackCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugReportCallbackCreateInfoEXT =
             '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "sType" VkDebugReportCallbackCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, sType}
        type FieldIsArray "sType" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pNext" VkDebugReportCallbackCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "flags" VkDebugReportCallbackCreateInfoEXT =
             VkDebugReportFlagsEXT
        type FieldOptional "flags" VkDebugReportCallbackCreateInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, flags}
        type FieldIsArray "flags" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "pfnCallback" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pfnCallback" VkDebugReportCallbackCreateInfoEXT =
             PFN_vkDebugReportCallbackEXT
        type FieldOptional "pfnCallback" VkDebugReportCallbackCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pfnCallback" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}
        type FieldIsArray "pfnCallback" VkDebugReportCallbackCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

instance {-# OVERLAPPING #-}
         CanReadField "pfnCallback" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

instance {-# OVERLAPPING #-}
         CanWriteField "pfnCallback" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

instance {-# OVERLAPPING #-}
         HasField "pUserData" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pUserData" VkDebugReportCallbackCreateInfoEXT =
             Ptr Void
        type FieldOptional "pUserData" VkDebugReportCallbackCreateInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pUserData" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}
        type FieldIsArray "pUserData" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance {-# OVERLAPPING #-}
         CanReadField "pUserData" VkDebugReportCallbackCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pUserData})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance {-# OVERLAPPING #-}
         CanWriteField "pUserData" VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance Show VkDebugReportCallbackCreateInfoEXT where
        showsPrec d x
          = showString "VkDebugReportCallbackCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "pfnCallback = " .
                                  showsPrec d (getField @"pfnCallback" x) .
                                    showString ", " .
                                      showString "pUserData = " .
                                        showsPrec d (getField @"pUserData" x) . showChar '}'

-- | > typedef struct VkDebugUtilsObjectTagInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkObjectType                           objectType;
--   >     uint64_t                               objectHandle;
--   >     uint64_t                               tagName;
--   >     size_t                                 tagSize;
--   >     const void*              pTag;
--   > } VkDebugUtilsObjectTagInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXT registry at www.khronos.org>
data VkDebugUtilsObjectTagInfoEXT = VkDebugUtilsObjectTagInfoEXT## Addr##
                                                                  ByteArray##

instance Eq VkDebugUtilsObjectTagInfoEXT where
        (VkDebugUtilsObjectTagInfoEXT## a _) ==
          x@(VkDebugUtilsObjectTagInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugUtilsObjectTagInfoEXT where
        (VkDebugUtilsObjectTagInfoEXT## a _) `compare`
          x@(VkDebugUtilsObjectTagInfoEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugUtilsObjectTagInfoEXT where
        sizeOf ~_ = #{size VkDebugUtilsObjectTagInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugUtilsObjectTagInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugUtilsObjectTagInfoEXT where
        unsafeAddr (VkDebugUtilsObjectTagInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugUtilsObjectTagInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugUtilsObjectTagInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugUtilsObjectTagInfoEXT where
        type StructFields VkDebugUtilsObjectTagInfoEXT =
             '["sType", "pNext", "objectType", "objectHandle", "tagName", -- ' closing tick for hsc2hs
               "tagSize", "pTag"]
        type CUnionType VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugUtilsObjectTagInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "sType" VkDebugUtilsObjectTagInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, sType}
        type FieldIsArray "sType" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "pNext" VkDebugUtilsObjectTagInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectType" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "objectType" VkDebugUtilsObjectTagInfoEXT =
             VkObjectType
        type FieldOptional "objectType" VkDebugUtilsObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectType" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, objectType}
        type FieldIsArray "objectType" VkDebugUtilsObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanReadField "objectType" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, objectType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         CanWriteField "objectType" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, objectType}

instance {-# OVERLAPPING #-}
         HasField "objectHandle" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "objectHandle" VkDebugUtilsObjectTagInfoEXT = Word64
        type FieldOptional "objectHandle" VkDebugUtilsObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectHandle" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}
        type FieldIsArray "objectHandle" VkDebugUtilsObjectTagInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         CanReadField "objectHandle" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         CanWriteField "objectHandle" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}

instance {-# OVERLAPPING #-}
         HasField "tagName" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "tagName" VkDebugUtilsObjectTagInfoEXT = Word64
        type FieldOptional "tagName" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagName" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, tagName}
        type FieldIsArray "tagName" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         CanReadField "tagName" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, tagName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         CanWriteField "tagName" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, tagName}

instance {-# OVERLAPPING #-}
         HasField "tagSize" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "tagSize" VkDebugUtilsObjectTagInfoEXT = CSize
        type FieldOptional "tagSize" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tagSize" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}
        type FieldIsArray "tagSize" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         CanReadField "tagSize" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, tagSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         CanWriteField "tagSize" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}

instance {-# OVERLAPPING #-}
         HasField "pTag" VkDebugUtilsObjectTagInfoEXT where
        type FieldType "pTag" VkDebugUtilsObjectTagInfoEXT = Ptr Void
        type FieldOptional "pTag" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pTag" VkDebugUtilsObjectTagInfoEXT =
             #{offset VkDebugUtilsObjectTagInfoEXT, pTag}
        type FieldIsArray "pTag" VkDebugUtilsObjectTagInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugUtilsObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         CanReadField "pTag" VkDebugUtilsObjectTagInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugUtilsObjectTagInfoEXT, pTag})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, pTag}

instance {-# OVERLAPPING #-}
         CanWriteField "pTag" VkDebugUtilsObjectTagInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDebugUtilsObjectTagInfoEXT, pTag}

instance Show VkDebugUtilsObjectTagInfoEXT where
        showsPrec d x
          = showString "VkDebugUtilsObjectTagInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectType = " .
                            showsPrec d (getField @"objectType" x) .
                              showString ", " .
                                showString "objectHandle = " .
                                  showsPrec d (getField @"objectHandle" x) .
                                    showString ", " .
                                      showString "tagName = " .
                                        showsPrec d (getField @"tagName" x) .
                                          showString ", " .
                                            showString "tagSize = " .
                                              showsPrec d (getField @"tagSize" x) .
                                                showString ", " .
                                                  showString "pTag = " .
                                                    showsPrec d (getField @"pTag" x) . showChar '}'
