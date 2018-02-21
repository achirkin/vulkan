#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
       (VkAllocationCallbacks(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Funcpointers  (PFN_vkAllocationFunction,
                                                      PFN_vkFreeFunction,
                                                      PFN_vkInternalAllocationNotification,
                                                      PFN_vkInternalFreeNotification,
                                                      PFN_vkReallocationFunction)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkAllocationCallbacks {
--   >     void*           pUserData;
--   >     PFN_vkAllocationFunction   pfnAllocation;
--   >     PFN_vkReallocationFunction pfnReallocation;
--   >     PFN_vkFreeFunction    pfnFree;
--   >     PFN_vkInternalAllocationNotification pfnInternalAllocation;
--   >     PFN_vkInternalFreeNotification pfnInternalFree;
--   > } VkAllocationCallbacks;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkAllocationCallbacks.html VkAllocationCallbacks registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkPUserData VkAllocationCallbacks
         where
        type VkPUserDataMType VkAllocationCallbacks = Ptr Void

        {-# NOINLINE vkPUserData #-}
        vkPUserData x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pUserData})

        {-# INLINE vkPUserDataByteOffset #-}
        vkPUserDataByteOffset ~_
          = #{offset VkAllocationCallbacks, pUserData}

        {-# INLINE readVkPUserData #-}
        readVkPUserData p
          = peekByteOff p #{offset VkAllocationCallbacks, pUserData}

        {-# INLINE writeVkPUserData #-}
        writeVkPUserData p
          = pokeByteOff p #{offset VkAllocationCallbacks, pUserData}

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

instance CanReadField "pUserData" VkAllocationCallbacks where
        {-# INLINE getField #-}
        getField = vkPUserData

        {-# INLINE readField #-}
        readField = readVkPUserData

instance CanWriteField "pUserData" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField = writeVkPUserData

instance {-# OVERLAPPING #-}
         HasVkPfnAllocation VkAllocationCallbacks where
        type VkPfnAllocationMType VkAllocationCallbacks =
             PFN_vkAllocationFunction

        {-# NOINLINE vkPfnAllocation #-}
        vkPfnAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnAllocation})

        {-# INLINE vkPfnAllocationByteOffset #-}
        vkPfnAllocationByteOffset ~_
          = #{offset VkAllocationCallbacks, pfnAllocation}

        {-# INLINE readVkPfnAllocation #-}
        readVkPfnAllocation p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnAllocation}

        {-# INLINE writeVkPfnAllocation #-}
        writeVkPfnAllocation p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnAllocation}

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

instance CanReadField "pfnAllocation" VkAllocationCallbacks where
        {-# INLINE getField #-}
        getField = vkPfnAllocation

        {-# INLINE readField #-}
        readField = readVkPfnAllocation

instance CanWriteField "pfnAllocation" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField = writeVkPfnAllocation

instance {-# OVERLAPPING #-}
         HasVkPfnReallocation VkAllocationCallbacks where
        type VkPfnReallocationMType VkAllocationCallbacks =
             PFN_vkReallocationFunction

        {-# NOINLINE vkPfnReallocation #-}
        vkPfnReallocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnReallocation})

        {-# INLINE vkPfnReallocationByteOffset #-}
        vkPfnReallocationByteOffset ~_
          = #{offset VkAllocationCallbacks, pfnReallocation}

        {-# INLINE readVkPfnReallocation #-}
        readVkPfnReallocation p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnReallocation}

        {-# INLINE writeVkPfnReallocation #-}
        writeVkPfnReallocation p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnReallocation}

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

instance CanReadField "pfnReallocation" VkAllocationCallbacks where
        {-# INLINE getField #-}
        getField = vkPfnReallocation

        {-# INLINE readField #-}
        readField = readVkPfnReallocation

instance CanWriteField "pfnReallocation" VkAllocationCallbacks
         where
        {-# INLINE writeField #-}
        writeField = writeVkPfnReallocation

instance {-# OVERLAPPING #-} HasVkPfnFree VkAllocationCallbacks
         where
        type VkPfnFreeMType VkAllocationCallbacks = PFN_vkFreeFunction

        {-# NOINLINE vkPfnFree #-}
        vkPfnFree x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnFree})

        {-# INLINE vkPfnFreeByteOffset #-}
        vkPfnFreeByteOffset ~_
          = #{offset VkAllocationCallbacks, pfnFree}

        {-# INLINE readVkPfnFree #-}
        readVkPfnFree p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnFree}

        {-# INLINE writeVkPfnFree #-}
        writeVkPfnFree p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnFree}

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

instance CanReadField "pfnFree" VkAllocationCallbacks where
        {-# INLINE getField #-}
        getField = vkPfnFree

        {-# INLINE readField #-}
        readField = readVkPfnFree

instance CanWriteField "pfnFree" VkAllocationCallbacks where
        {-# INLINE writeField #-}
        writeField = writeVkPfnFree

instance {-# OVERLAPPING #-}
         HasVkPfnInternalAllocation VkAllocationCallbacks where
        type VkPfnInternalAllocationMType VkAllocationCallbacks =
             PFN_vkInternalAllocationNotification

        {-# NOINLINE vkPfnInternalAllocation #-}
        vkPfnInternalAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnInternalAllocation})

        {-# INLINE vkPfnInternalAllocationByteOffset #-}
        vkPfnInternalAllocationByteOffset ~_
          = #{offset VkAllocationCallbacks, pfnInternalAllocation}

        {-# INLINE readVkPfnInternalAllocation #-}
        readVkPfnInternalAllocation p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnInternalAllocation}

        {-# INLINE writeVkPfnInternalAllocation #-}
        writeVkPfnInternalAllocation p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnInternalAllocation}

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

instance CanReadField "pfnInternalAllocation" VkAllocationCallbacks
         where
        {-# INLINE getField #-}
        getField = vkPfnInternalAllocation

        {-# INLINE readField #-}
        readField = readVkPfnInternalAllocation

instance CanWriteField "pfnInternalAllocation"
           VkAllocationCallbacks
         where
        {-# INLINE writeField #-}
        writeField = writeVkPfnInternalAllocation

instance {-# OVERLAPPING #-}
         HasVkPfnInternalFree VkAllocationCallbacks where
        type VkPfnInternalFreeMType VkAllocationCallbacks =
             PFN_vkInternalFreeNotification

        {-# NOINLINE vkPfnInternalFree #-}
        vkPfnInternalFree x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAllocationCallbacks, pfnInternalFree})

        {-# INLINE vkPfnInternalFreeByteOffset #-}
        vkPfnInternalFreeByteOffset ~_
          = #{offset VkAllocationCallbacks, pfnInternalFree}

        {-# INLINE readVkPfnInternalFree #-}
        readVkPfnInternalFree p
          = peekByteOff p #{offset VkAllocationCallbacks, pfnInternalFree}

        {-# INLINE writeVkPfnInternalFree #-}
        writeVkPfnInternalFree p
          = pokeByteOff p #{offset VkAllocationCallbacks, pfnInternalFree}

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

instance CanReadField "pfnInternalFree" VkAllocationCallbacks where
        {-# INLINE getField #-}
        getField = vkPfnInternalFree

        {-# INLINE readField #-}
        readField = readVkPfnInternalFree

instance CanWriteField "pfnInternalFree" VkAllocationCallbacks
         where
        {-# INLINE writeField #-}
        writeField = writeVkPfnInternalFree

instance Show VkAllocationCallbacks where
        showsPrec d x
          = showString "VkAllocationCallbacks {" .
              showString "vkPUserData = " .
                showsPrec d (vkPUserData x) .
                  showString ", " .
                    showString "vkPfnAllocation = " .
                      showsPrec d (vkPfnAllocation x) .
                        showString ", " .
                          showString "vkPfnReallocation = " .
                            showsPrec d (vkPfnReallocation x) .
                              showString ", " .
                                showString "vkPfnFree = " .
                                  showsPrec d (vkPfnFree x) .
                                    showString ", " .
                                      showString "vkPfnInternalAllocation = " .
                                        showsPrec d (vkPfnInternalAllocation x) .
                                          showString ", " .
                                            showString "vkPfnInternalFree = " .
                                              showsPrec d (vkPfnInternalFree x) . showChar '}'
