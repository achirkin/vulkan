{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_external_memory_dma_buf
       (-- * Vulkan extension: @VK_EXT_external_memory_dma_buf@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Chad Versace @chadversary@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @126@
        --
        -- Required extensions: 'VK_KHR_external_memory_fd'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory_fd'.
        VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION,
        pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION,
        VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME,
        pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT)
       where
import           GHC.Ptr                             (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.External (VkExternalMemoryHandleTypeBitmask (..))

pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = 1

type VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = 1

pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME :: CString

pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME <-
        (is_VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME -> True)
  where
    VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
      = _VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME

{-# INLINE _VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME #-}

_VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME :: CString
_VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
  = Ptr "VK_EXT_external_memory_dma_buf\NUL"#

{-# INLINE is_VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME #-}

is_VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME

type VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME =
     "VK_EXT_external_memory_dma_buf"

-- | bitpos = @9@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT =
        VkExternalMemoryHandleTypeBitmask 512
