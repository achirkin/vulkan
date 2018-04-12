#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.AllocationCallbacks
       (VkAllocationCallbacks(..)) where
import           Foreign.Storable                   (Storable (..))
import           GHC.Base                           (Addr##, ByteArray##,
                                                     byteArrayContents##,
                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Funcpointers (PFN_vkAllocationFunction,
                                                     PFN_vkFreeFunction,
                                                     PFN_vkInternalAllocationNotification,
                                                     PFN_vkInternalFreeNotification,
                                                     PFN_vkReallocationFunction)
import           System.IO.Unsafe                   (unsafeDupablePerformIO)

-- | > typedef struct VkAllocationCallbacks {
--   >     void*           pUserData;
--   >     PFN_vkAllocationFunction   pfnAllocation;
--   >     PFN_vkReallocationFunction pfnReallocation;
--   >     PFN_vkFreeFunction    pfnFree;
--   >     PFN_vkInternalAllocationNotification pfnInternalAllocation;
--   >     PFN_vkInternalFreeNotification pfnInternalFree;
--   > } VkAllocationCallbacks;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAllocationCallbacks VkAllocationCallbacks registry at www.khronos.org>
data VkAllocationCallbacks = VkAllocationCallbacks## Addr##
                                                    ByteArray##

instance Eq VkAllocationCallbacks where
        (VkAllocationCallbacks## a _) == x@(VkAllocationCallbacks## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAllocationCallbacks where
        (VkAllocationCallbacks## a _) `compare`
          x@(VkAllocationCallbacks## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAllocationCallbacks where
        sizeOf ~_ = #{size VkAllocationCallbacks}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkAllocationCallbacks}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAllocationCallbacks where
        unsafeAddr (VkAllocationCallbacks## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAllocationCallbacks## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAllocationCallbacks## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAllocationCallbacks where
        type StructFields VkAllocationCallbacks =
             '["pUserData", "pfnAllocation", "pfnReallocation", "pfnFree", -- ' closing tick for hsc2hs
               "pfnInternalAllocation", "pfnInternalFree"]
        type CUnionType VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAllocationCallbacks = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "pUserData" VkAllocationCallbacks where
        type FieldType "pUserData" VkAllocationCallbacks = Ptr Void
        type FieldOptional "pUserData" VkAllocationCallbacks = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pUserData" VkAllocationCallbacks =
             #{offset VkAllocationCallbacks, pUserData}
        type FieldIsArray "pUserData" VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAllocationCallbacks, pUserData}

instance {-# OVERLAPPING #-}
         CanReadField "pUserData" VkAllocationCallbacks where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pUserData})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAllocationCallbacks, pUserData}

instance {-# OVERLAPPING #-}
         CanWriteField "pUserData" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAllocationCallbacks, pUserData}

instance {-# OVERLAPPING #-}
         HasField "pfnAllocation" VkAllocationCallbacks where
        type FieldType "pfnAllocation" VkAllocationCallbacks =
             PFN_vkAllocationFunction
        type FieldOptional "pfnAllocation" VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pfnAllocation" VkAllocationCallbacks =
             #{offset VkAllocationCallbacks, pfnAllocation}
        type FieldIsArray "pfnAllocation" VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAllocationCallbacks, pfnAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "pfnAllocation" VkAllocationCallbacks where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "pfnAllocation" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnAllocation}

instance {-# OVERLAPPING #-}
         HasField "pfnReallocation" VkAllocationCallbacks where
        type FieldType "pfnReallocation" VkAllocationCallbacks =
             PFN_vkReallocationFunction
        type FieldOptional "pfnReallocation" VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pfnReallocation" VkAllocationCallbacks =
             #{offset VkAllocationCallbacks, pfnReallocation}
        type FieldIsArray "pfnReallocation" VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAllocationCallbacks, pfnReallocation}

instance {-# OVERLAPPING #-}
         CanReadField "pfnReallocation" VkAllocationCallbacks where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnReallocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnReallocation}

instance {-# OVERLAPPING #-}
         CanWriteField "pfnReallocation" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnReallocation}

instance {-# OVERLAPPING #-}
         HasField "pfnFree" VkAllocationCallbacks where
        type FieldType "pfnFree" VkAllocationCallbacks = PFN_vkFreeFunction
        type FieldOptional "pfnFree" VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pfnFree" VkAllocationCallbacks =
             #{offset VkAllocationCallbacks, pfnFree}
        type FieldIsArray "pfnFree" VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkAllocationCallbacks, pfnFree}

instance {-# OVERLAPPING #-}
         CanReadField "pfnFree" VkAllocationCallbacks where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnFree})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnFree}

instance {-# OVERLAPPING #-}
         CanWriteField "pfnFree" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnFree}

instance {-# OVERLAPPING #-}
         HasField "pfnInternalAllocation" VkAllocationCallbacks where
        type FieldType "pfnInternalAllocation" VkAllocationCallbacks =
             PFN_vkInternalAllocationNotification
        type FieldOptional "pfnInternalAllocation" VkAllocationCallbacks =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pfnInternalAllocation" VkAllocationCallbacks =
             #{offset VkAllocationCallbacks, pfnInternalAllocation}
        type FieldIsArray "pfnInternalAllocation" VkAllocationCallbacks =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAllocationCallbacks, pfnInternalAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "pfnInternalAllocation" VkAllocationCallbacks where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnInternalAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnInternalAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "pfnInternalAllocation" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnInternalAllocation}

instance {-# OVERLAPPING #-}
         HasField "pfnInternalFree" VkAllocationCallbacks where
        type FieldType "pfnInternalFree" VkAllocationCallbacks =
             PFN_vkInternalFreeNotification
        type FieldOptional "pfnInternalFree" VkAllocationCallbacks = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pfnInternalFree" VkAllocationCallbacks =
             #{offset VkAllocationCallbacks, pfnInternalFree}
        type FieldIsArray "pfnInternalFree" VkAllocationCallbacks = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAllocationCallbacks, pfnInternalFree}

instance {-# OVERLAPPING #-}
         CanReadField "pfnInternalFree" VkAllocationCallbacks where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnInternalFree})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnInternalFree}

instance {-# OVERLAPPING #-}
         CanWriteField "pfnInternalFree" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnInternalFree}

instance Show VkAllocationCallbacks where
        showsPrec d x
          = showString "VkAllocationCallbacks {" .
              showString "pUserData = " .
                showsPrec d (getField @"pUserData" x) .
                  showString ", " .
                    showString "pfnAllocation = " .
                      showsPrec d (getField @"pfnAllocation" x) .
                        showString ", " .
                          showString "pfnReallocation = " .
                            showsPrec d (getField @"pfnReallocation" x) .
                              showString ", " .
                                showString "pfnFree = " .
                                  showsPrec d (getField @"pfnFree" x) .
                                    showString ", " .
                                      showString "pfnInternalAllocation = " .
                                        showsPrec d (getField @"pfnInternalAllocation" x) .
                                          showString ", " .
                                            showString "pfnInternalFree = " .
                                              showsPrec d (getField @"pfnInternalFree" x) .
                                                showChar '}'
