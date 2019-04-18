{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Lib.Vulkan.Image
  ( createTextureImage
  , createTextureImageView
  , createTextureSampler
  , textureImageInfo
  , createImageView
  , createImage
  , copyBufferToImage
  , findSupportedFormat
  , findDepthFormat
  , hasStencilComponent
  , createDepthImgView
  ) where

import           Codec.Picture
import           Control.Monad
import           Data.Bits
import qualified Data.Vector.Storable           as Vec
import           Foreign.Marshal.Array          (copyArray)
import           Foreign.Ptr                    (castPtr)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Buffer
import           Lib.Vulkan.Command



createTextureImageView :: VkDevice -> VkImage -> Program r VkImageView
createTextureImageView dev img = createImageView dev img VK_FORMAT_R8G8B8A8_UNORM VK_IMAGE_ASPECT_COLOR_BIT

createTextureImage :: VkPhysicalDevice
                   -> VkDevice
                   -> VkCommandPool
                   -> VkQueue
                   -> FilePath
                   -> Program r VkImage
createTextureImage pdev dev cmdPool cmdQueue path = do
  Image { imageWidth, imageHeight, imageData }
    <- (liftIO $ readImage path) >>= \case
      Left err -> throwVkMsg err
      Right dynImg -> pure $ convertRGBA8 dynImg
  let (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
      bufSize :: VkDeviceSize = fromIntegral imageDataLen

  -- we don't need to access the VkDeviceMemory of the image, copyBufferToImage works with the VkImage
  (_, image) <- createImage pdev dev (fromIntegral imageWidth) (fromIntegral imageHeight)
    VK_FORMAT_R8G8B8A8_UNORM VK_IMAGE_TILING_OPTIMAL
    (VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT)
    VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  transitionImageLayout dev cmdPool cmdQueue image VK_FORMAT_R8G8B8A8_UNORM Undef_TransDst

  -- Use "locally" to destroy temporary staging buffer after data copy is complete
  locally $ do
    (stagingMem, stagingBuf) <-
      createBuffer pdev dev bufSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
        ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

    -- copy data
    stagingDataPtr <- allocaPeek $
      runVk . vkMapMemory dev stagingMem 0 bufSize 0
    liftIO $ withForeignPtr imageDataForeignPtr $ \imageDataPtr ->
      copyArray (castPtr stagingDataPtr) imageDataPtr imageDataLen
    liftIO $ vkUnmapMemory dev stagingMem

    copyBufferToImage dev cmdPool cmdQueue stagingBuf image
      (fromIntegral imageWidth) (fromIntegral imageHeight)

  transitionImageLayout dev cmdPool cmdQueue image VK_FORMAT_R8G8B8A8_UNORM TransDst_ShaderRO

  return image


createTextureSampler :: VkDevice
                     -> Program r VkSampler
createTextureSampler dev = do
  let samplerCreateInfo = createVk @VkSamplerCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
        &* set @"pNext" VK_NULL_HANDLE
        &* set @"magFilter" VK_FILTER_LINEAR
        &* set @"minFilter" VK_FILTER_LINEAR
        &* set @"addressModeU" VK_SAMPLER_ADDRESS_MODE_REPEAT
        &* set @"addressModeV" VK_SAMPLER_ADDRESS_MODE_REPEAT
        &* set @"addressModeW" VK_SAMPLER_ADDRESS_MODE_REPEAT
        &* set @"anisotropyEnable" VK_TRUE
        &* set @"maxAnisotropy" 16
        &* set @"borderColor" VK_BORDER_COLOR_INT_OPAQUE_BLACK
        &* set @"unnormalizedCoordinates" VK_FALSE
        &* set @"compareEnable" VK_FALSE
        &* set @"compareOp" VK_COMPARE_OP_ALWAYS
        &* set @"mipmapMode" VK_SAMPLER_MIPMAP_MODE_LINEAR
        &* set @"mipLodBias" 0
        &* set @"minLod" 0
        &* set @"maxLod" 0

  allocResource (liftIO . (flip (vkDestroySampler dev) VK_NULL)) $
    withVkPtr samplerCreateInfo $ \sciPtr ->
      allocaPeek $ runVk . vkCreateSampler dev sciPtr VK_NULL


textureImageInfo :: VkImageView -> VkSampler -> Program r VkDescriptorImageInfo
textureImageInfo view sampler = return $ createVk @VkDescriptorImageInfo
        $  set @"imageLayout" VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        &* set @"imageView" view
        &* set @"sampler" sampler


createImageView :: VkDevice
                -> VkImage
                -> VkFormat
                -> VkImageAspectFlags
                -> Program r VkImageView
createImageView dev image format aspectFlags = do
    let cmapping = createVk
          $  set @"r" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"g" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"b" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"a" VK_COMPONENT_SWIZZLE_IDENTITY
        srrange = createVk
          $  set @"aspectMask" aspectFlags
          &* set @"baseMipLevel" 0
          &* set @"levelCount" 1
          &* set @"baseArrayLayer" 0
          &* set @"layerCount" 1
        imgvCreateInfo = createVk @VkImageViewCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
          &* set @"pNext" VK_NULL_HANDLE
          &* set @"flags" 0
          &* set @"image" image
          &* set @"viewType" VK_IMAGE_VIEW_TYPE_2D
          &* set @"format" format
          &* set @"components" cmapping
          &* set @"subresourceRange" srrange

    allocResource (liftIO . (flip (vkDestroyImageView dev) VK_NULL)) $
      withVkPtr imgvCreateInfo $ \imgvciPtr ->
         allocaPeek $ runVk . vkCreateImageView dev imgvciPtr VK_NULL


data ImageLayoutTransition = Undef_TransDst | TransDst_ShaderRO | Undef_DepthStencil

data TransitionDependent = TransitionDependent
  { oldLayout     :: VkImageLayout
  , newLayout     :: VkImageLayout
  , srcAccessMask :: VkAccessFlags
  , dstAccessMask :: VkAccessFlags
  , srcStageMask  :: VkPipelineStageFlags
  , dstStageMask  :: VkPipelineStageFlags
  }

dependents :: ImageLayoutTransition -> TransitionDependent
dependents Undef_TransDst =
  TransitionDependent
  { oldLayout       = VK_IMAGE_LAYOUT_UNDEFINED
  , newLayout       = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , srcAccessMask   = 0
  , dstAccessMask   = VK_ACCESS_TRANSFER_WRITE_BIT
  , srcStageMask    = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , dstStageMask    = VK_PIPELINE_STAGE_TRANSFER_BIT
  }
dependents TransDst_ShaderRO =
  TransitionDependent
  { oldLayout       = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , newLayout       = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , srcAccessMask   = VK_ACCESS_TRANSFER_WRITE_BIT
  , dstAccessMask   = VK_ACCESS_SHADER_READ_BIT
  , srcStageMask    = VK_PIPELINE_STAGE_TRANSFER_BIT
  , dstStageMask    = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
  }
dependents Undef_DepthStencil =
  TransitionDependent
  { oldLayout       = VK_IMAGE_LAYOUT_UNDEFINED
  , newLayout       = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , srcAccessMask   = 0
  , dstAccessMask   = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT .|. VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
  , srcStageMask    = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , dstStageMask    = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
  }

transitionImageLayout :: VkDevice
                      -> VkCommandPool
                      -> VkQueue
                      -> VkImage
                      -> VkFormat
                      -> ImageLayoutTransition
                      -> Program r ()
transitionImageLayout dev cmdPool cmdQueue image format transition =
  runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf -> do
    let TransitionDependent {..} = dependents transition
    let aspectMask = case newLayout of
          VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
            | hasStencilComponent format ->
              VK_IMAGE_ASPECT_DEPTH_BIT .|. VK_IMAGE_ASPECT_STENCIL_BIT
            | otherwise ->
              VK_IMAGE_ASPECT_DEPTH_BIT
          _ -> VK_IMAGE_ASPECT_COLOR_BIT
    let barrier = createVk @VkImageMemoryBarrier
          $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
          &* set @"pNext" VK_NULL
          &* set @"oldLayout" oldLayout
          &* set @"newLayout" newLayout
          &* set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
          &* set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
          &* set @"image" image
          &* setVk @"subresourceRange"
              (  set @"aspectMask" aspectMask
              &* set @"baseMipLevel" 0
              &* set @"levelCount" 1
              &* set @"baseArrayLayer" 0
              &* set @"layerCount" 1
              )
          &* set @"srcAccessMask" srcAccessMask
          &* set @"dstAccessMask" dstAccessMask
    withVkPtr barrier $ \barrPtr -> liftIO $
      vkCmdPipelineBarrier cmdBuf
      srcStageMask dstStageMask
      0
      0 VK_NULL
      0 VK_NULL
      1 barrPtr


createImage :: VkPhysicalDevice
            -> VkDevice
            -> Word32
            -> Word32
            -> VkFormat
            -> VkImageTiling
            -> VkImageUsageFlags
            -> VkMemoryPropertyFlags
            -> Program r (VkDeviceMemory, VkImage)
createImage pdev dev width height format tiling usage propFlags = do
  let ici = createVk @VkImageCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
        &* set @"imageType" VK_IMAGE_TYPE_2D
        &* setVk @"extent"
            (  set @"width" width
            &* set @"height" height
            &* set @"depth" 1
            )
        &* set @"mipLevels" 1
        &* set @"arrayLayers" 1
        &* set @"format" format
        &* set @"tiling" tiling
        &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
        &* set @"usage" usage
        &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
        &* set @"samples" VK_SAMPLE_COUNT_1_BIT
        &* set @"queueFamilyIndexCount" 0
        &* set @"pQueueFamilyIndices" VK_NULL
  (image, freeImageLater) <- allocResource'
      (\img -> liftIO (vkDestroyImage dev img VK_NULL)) $
      withVkPtr ici $ \iciPtr -> allocaPeek $ \imgPtr ->
        runVk $ vkCreateImage dev iciPtr VK_NULL imgPtr

  memRequirements <- allocaPeek $ \reqsPtr ->
    liftIO $ vkGetImageMemoryRequirements dev image reqsPtr

  memType <- findMemoryType pdev
    (getField @"memoryTypeBits" memRequirements) propFlags

  -- allocate memory
  let allocInfo = createVk @VkMemoryAllocateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"allocationSize" (getField @"size" memRequirements)
        &* set @"memoryTypeIndex" memType

  imageMemory <- allocResource
    (\iMem -> liftIO $ vkFreeMemory dev iMem VK_NULL) $
    withVkPtr allocInfo $ \aiPtr -> allocaPeek $
      runVk . vkAllocateMemory dev aiPtr VK_NULL

  -- release the image before releasing the memory that is bound to it
  freeImageLater

  runVk $ vkBindImageMemory dev image imageMemory 0

  return (imageMemory, image)


copyBufferToImage :: VkDevice
                  -> VkCommandPool
                  -> VkQueue
                  -> VkBuffer
                  -> VkImage
                  -> Word32
                  -> Word32
                  -> Program r ()
copyBufferToImage dev cmdPool cmdQueue buffer image width height =
  runCommandsOnce dev cmdPool cmdQueue $ \cmdBuf -> do
    let region = createVk @VkBufferImageCopy
          $  set @"bufferOffset" 0
          &* set @"bufferRowLength" 0
          &* set @"bufferImageHeight" 0
          &* setVk @"imageSubresource"
              (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
              &* set @"mipLevel" 0
              &* set @"baseArrayLayer" 0
              &* set @"layerCount" 1
              )
          &* setVk @"imageOffset"
              (  set @"x" 0
              &* set @"y" 0
              &* set @"z" 0
              )
          &* setVk @"imageExtent"
              (  set @"width" width
              &* set @"height" height
              &* set @"depth" 1
              )
    withVkPtr region $ \regPtr -> liftIO $
      vkCmdCopyBufferToImage cmdBuf buffer image
        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 regPtr


findSupportedFormat :: VkPhysicalDevice
                    -> [VkFormat]
                    -> VkImageTiling
                    -> VkFormatFeatureFlags
                    -> Program r VkFormat
findSupportedFormat pdev candidates tiling features = do
  goodCands <- flip filterM candidates $ \format -> do
    props <- allocaPeek $ \propsPtr ->
      liftIO $ vkGetPhysicalDeviceFormatProperties pdev format propsPtr
    return $ case tiling of
      VK_IMAGE_TILING_LINEAR ->
        getField @"linearTilingFeatures" props .&. features == features
      VK_IMAGE_TILING_OPTIMAL ->
        getField @"optimalTilingFeatures" props .&. features == features
      _ -> False
  case goodCands of
    x:_ -> return x
    []  -> throwVkMsg "failed to find supported format"


findDepthFormat :: VkPhysicalDevice
                -> Program r VkFormat
findDepthFormat pdev =
  findSupportedFormat pdev
    [VK_FORMAT_D32_SFLOAT, VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT]
    VK_IMAGE_TILING_OPTIMAL
    VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT


hasStencilComponent :: VkFormat
                    -> Bool
hasStencilComponent format = format `elem`
  [VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT]


createDepthImgView :: VkPhysicalDevice
                   -> VkDevice
                   -> VkCommandPool
                   -> VkQueue
                   -> VkExtent2D
                   -> Program r VkImageView
createDepthImgView pdev dev cmdPool cmdQueue extent = do
  depthFormat <- findDepthFormat pdev

  (_, depthImage) <- createImage pdev dev
    (getField @"width" extent) (getField @"height" extent) depthFormat
    VK_IMAGE_TILING_OPTIMAL VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  depthImageView <- createImageView dev depthImage depthFormat VK_IMAGE_ASPECT_DEPTH_BIT
  transitionImageLayout dev cmdPool cmdQueue depthImage depthFormat Undef_DepthStencil
  return depthImageView