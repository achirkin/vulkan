{-# OPTIONS_GHC -fno-warn-missing-methods#-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphics.Vulkan.StructMembers
       (HasVkA(..), HasVkAcquireCount(..), HasVkActualPresentTime(..),
        HasVkAddressModeU(..), HasVkAddressModeV(..),
        HasVkAddressModeW(..), HasVkAdvancedBlendAllOperations(..),
        HasVkAdvancedBlendCoherentOperations(..),
        HasVkAdvancedBlendCorrelatedOverlap(..),
        HasVkAdvancedBlendIndependentBlend(..),
        HasVkAdvancedBlendMaxColorAttachments(..),
        HasVkAdvancedBlendNonPremultipliedDstColor(..),
        HasVkAdvancedBlendNonPremultipliedSrcColor(..), HasVkAlignment(..),
        HasVkAllocationSize(..), HasVkAlphaBlendOp(..), HasVkAlphaMode(..),
        HasVkAlphaToCoverageEnable(..), HasVkAlphaToOne(..),
        HasVkAlphaToOneEnable(..), HasVkAnisotropyEnable(..),
        HasVkApiVersion(..), HasVkApplicationVersion(..),
        HasVkArrayLayer(..), HasVkArrayLayers(..), HasVkArrayPitch(..),
        HasVkAspectMask(..), HasVkAspectReferenceCount(..),
        HasVkAttachment(..), HasVkAttachmentCount(..),
        HasVkAttachmentIndex(..),
        HasVkAttachmentInitialSampleLocationsCount(..), HasVkB(..),
        HasVkBack(..), HasVkBaseArrayLayer(..), HasVkBaseMipLevel(..),
        HasVkBasePipelineHandle(..), HasVkBasePipelineIndex(..),
        HasVkBindCount(..), HasVkBinding(..), HasVkBindingCount(..),
        HasVkBindingUnit(..), HasVkBlendConstantsArray(..),
        HasVkBlendEnable(..), HasVkBlendOverlap(..), HasVkBorderColor(..),
        HasVkBuffer(..), HasVkBufferBindCount(..), HasVkBufferFeatures(..),
        HasVkBufferImageGranularity(..), HasVkBufferImageHeight(..),
        HasVkBufferOffset(..), HasVkBufferRowLength(..),
        HasVkChromaFilter(..), HasVkClearValue(..),
        HasVkClearValueCount(..), HasVkClipped(..), HasVkCodeSize(..),
        HasVkColor(..), HasVkColorArray(..), HasVkColorAttachment(..),
        HasVkColorAttachmentCount(..), HasVkColorBlendOp(..),
        HasVkColorSpace(..), HasVkColorWriteMask(..),
        HasVkCombinedImageSamplerDescriptorCount(..),
        HasVkCommandBufferCount(..), HasVkCommandPool(..),
        HasVkCompareEnable(..), HasVkCompareMask(..), HasVkCompareOp(..),
        HasVkCompatibleHandleTypes(..), HasVkComponents(..),
        HasVkCompositeAlpha(..), HasVkComputeBindingPointSupport(..),
        HasVkComputeWorkGroupSizeArray(..), HasVkConnection(..),
        HasVkConservativePointAndLineRasterization(..),
        HasVkConservativeRasterizationMode(..),
        HasVkConservativeRasterizationPostDepthCoverage(..),
        HasVkConstantID(..), HasVkConversion(..),
        HasVkCorrelationMaskCount(..), HasVkCoverageModulationMode(..),
        HasVkCoverageModulationTableCount(..),
        HasVkCoverageModulationTableEnable(..),
        HasVkCoverageToColorEnable(..), HasVkCoverageToColorLocation(..),
        HasVkCullMode(..), HasVkCurrentDisplay(..), HasVkCurrentExtent(..),
        HasVkCurrentStackIndex(..), HasVkCurrentTransform(..),
        HasVkDataSize(..), HasVkDedicatedAllocation(..),
        HasVkDegenerateLinesRasterized(..),
        HasVkDegenerateTrianglesRasterized(..), HasVkDependencyCount(..),
        HasVkDependencyFlags(..), HasVkDepth(..), HasVkDepthBiasClamp(..),
        HasVkDepthBiasConstantFactor(..), HasVkDepthBiasEnable(..),
        HasVkDepthBiasSlopeFactor(..), HasVkDepthBounds(..),
        HasVkDepthBoundsTestEnable(..), HasVkDepthClamp(..),
        HasVkDepthClampEnable(..), HasVkDepthCompareOp(..),
        HasVkDepthFailOp(..), HasVkDepthPitch(..), HasVkDepthStencil(..),
        HasVkDepthTestEnable(..), HasVkDepthWriteEnable(..),
        HasVkDescriptionArray(..), HasVkDescriptorCount(..),
        HasVkDescriptorPool(..), HasVkDescriptorSet(..),
        HasVkDescriptorSetCount(..), HasVkDescriptorSetLayout(..),
        HasVkDescriptorType(..), HasVkDescriptorUpdateEntryCount(..),
        HasVkDesiredPresentTime(..), HasVkDeviceEvent(..),
        HasVkDeviceID(..), HasVkDeviceIndexCount(..),
        HasVkDeviceLUIDArray(..), HasVkDeviceLUIDValid(..),
        HasVkDeviceMask(..), HasVkDeviceNameArray(..),
        HasVkDeviceNodeMask(..), HasVkDeviceRenderAreaCount(..),
        HasVkDeviceType(..), HasVkDeviceUUIDArray(..),
        HasVkDisabledValidationCheckCount(..),
        HasVkDiscardRectangleCount(..), HasVkDiscardRectangleMode(..),
        HasVkDiscreteQueuePriorities(..), HasVkDisplay(..),
        HasVkDisplayEvent(..), HasVkDisplayMode(..), HasVkDisplayName(..),
        HasVkDisplayPrimaryBlue(..), HasVkDisplayPrimaryGreen(..),
        HasVkDisplayPrimaryRed(..), HasVkDivisor(..),
        HasVkDomainOrigin(..), HasVkDpy(..),
        HasVkDrawIndirectFirstInstance(..), HasVkDriverUUIDArray(..),
        HasVkDriverVersion(..), HasVkDstAccessMask(..),
        HasVkDstAlphaBlendFactor(..), HasVkDstArrayElement(..),
        HasVkDstBinding(..), HasVkDstColorBlendFactor(..),
        HasVkDstOffset(..), HasVkDstOffsetsArray(..),
        HasVkDstPremultiplied(..), HasVkDstQueueFamilyIndex(..),
        HasVkDstRect(..), HasVkDstSet(..), HasVkDstStageMask(..),
        HasVkDstSubpass(..), HasVkDstSubresource(..),
        HasVkDualSrcBlend(..), HasVkDwAccess(..), HasVkDynamicCount(..),
        HasVkDynamicStateCount(..), HasVkEarliestPresentTime(..),
        HasVkEnabledExtensionCount(..), HasVkEnabledLayerCount(..),
        HasVkEngineVersion(..), HasVkExportFromImportedHandleTypes(..),
        HasVkExtensionNameArray(..), HasVkExtent(..),
        HasVkExternalFenceFeatures(..), HasVkExternalMemoryFeatures(..),
        HasVkExternalMemoryProperties(..),
        HasVkExternalSemaphoreFeatures(..),
        HasVkExtraPrimitiveOverestimationSize(..),
        HasVkExtraPrimitiveOverestimationSizeGranularity(..),
        HasVkFailOp(..), HasVkFd(..), HasVkFeatures(..), HasVkFence(..),
        HasVkFillModeNonSolid(..),
        HasVkFilterMinmaxImageComponentMapping(..),
        HasVkFilterMinmaxSingleComponentFormats(..), HasVkFinalLayout(..),
        HasVkFirstIndex(..), HasVkFirstInstance(..), HasVkFirstVertex(..),
        HasVkFlags(..), HasVkFloat32Array(..),
        HasVkForceExplicitReconstruction(..), HasVkFormat(..),
        HasVkFormatProperties(..), HasVkFragmentStoresAndAtomics(..),
        HasVkFramebuffer(..), HasVkFramebufferColorSampleCounts(..),
        HasVkFramebufferDepthSampleCounts(..),
        HasVkFramebufferNoAttachmentsSampleCounts(..),
        HasVkFramebufferStencilSampleCounts(..), HasVkFront(..),
        HasVkFrontFace(..), HasVkFullDrawIndexUint32(..),
        HasVkFullyCoveredFragmentShaderInputVariable(..), HasVkG(..),
        HasVkGeometryShader(..), HasVkGlobalAlpha(..),
        HasVkGlobalPriority(..), HasVkHandle(..), HasVkHandleType(..),
        HasVkHandleTypes(..), HasVkHeapIndex(..), HasVkHeight(..),
        HasVkHinstance(..), HasVkHwnd(..), HasVkImage(..),
        HasVkImageArrayLayers(..), HasVkImageBindCount(..),
        HasVkImageColorSpace(..), HasVkImageCubeArray(..),
        HasVkImageExtent(..), HasVkImageFormat(..),
        HasVkImageFormatProperties(..), HasVkImageGranularity(..),
        HasVkImageIndex(..), HasVkImageLayout(..),
        HasVkImageMipTailFirstLod(..), HasVkImageMipTailOffset(..),
        HasVkImageMipTailSize(..), HasVkImageMipTailStride(..),
        HasVkImageOffset(..), HasVkImageOpaqueBindCount(..),
        HasVkImageSharingMode(..), HasVkImageSubresource(..),
        HasVkImageType(..), HasVkImageUsage(..), HasVkImageView(..),
        HasVkImplementationVersion(..), HasVkIndependentBlend(..),
        HasVkIndexCount(..), HasVkIndexType(..),
        HasVkIndirectCommandsLayout(..),
        HasVkIndirectCommandsTokenCount(..), HasVkInheritedQueries(..),
        HasVkInitialDataSize(..), HasVkInitialLayout(..),
        HasVkInputAttachmentCount(..), HasVkInputAttachmentIndex(..),
        HasVkInputRate(..), HasVkInstanceCount(..), HasVkInt32Array(..),
        HasVkLargePoints(..), HasVkLayer(..), HasVkLayerCount(..),
        HasVkLayerNameArray(..), HasVkLayers(..), HasVkLayout(..),
        HasVkLdsSizePerLocalWorkGroup(..), HasVkLdsUsageSizeInBytes(..),
        HasVkLevel(..), HasVkLevelCount(..), HasVkLimits(..),
        HasVkLineWidth(..), HasVkLineWidthGranularity(..),
        HasVkLineWidthRangeArray(..), HasVkLinearTilingFeatures(..),
        HasVkLoadOp(..), HasVkLocation(..), HasVkLogicOp(..),
        HasVkLogicOpEnable(..), HasVkMagFilter(..), HasVkMapEntryCount(..),
        HasVkMaxAnisotropy(..), HasVkMaxArrayLayers(..),
        HasVkMaxBoundDescriptorSets(..), HasVkMaxClipDistances(..),
        HasVkMaxColorAttachments(..),
        HasVkMaxCombinedClipAndCullDistances(..),
        HasVkMaxComputeSharedMemorySize(..),
        HasVkMaxComputeWorkGroupCountArray(..),
        HasVkMaxComputeWorkGroupInvocations(..),
        HasVkMaxComputeWorkGroupSizeArray(..),
        HasVkMaxContentLightLevel(..), HasVkMaxCullDistances(..),
        HasVkMaxDepth(..), HasVkMaxDepthBounds(..),
        HasVkMaxDescriptorSetInputAttachments(..),
        HasVkMaxDescriptorSetSampledImages(..),
        HasVkMaxDescriptorSetSamplers(..),
        HasVkMaxDescriptorSetStorageBuffers(..),
        HasVkMaxDescriptorSetStorageBuffersDynamic(..),
        HasVkMaxDescriptorSetStorageImages(..),
        HasVkMaxDescriptorSetUniformBuffers(..),
        HasVkMaxDescriptorSetUniformBuffersDynamic(..),
        HasVkMaxDiscardRectangles(..), HasVkMaxDrawIndexedIndexValue(..),
        HasVkMaxDrawIndirectCount(..), HasVkMaxDstExtent(..),
        HasVkMaxDstPosition(..), HasVkMaxExtent(..),
        HasVkMaxExtraPrimitiveOverestimationSize(..),
        HasVkMaxFragmentCombinedOutputResources(..),
        HasVkMaxFragmentDualSrcAttachments(..),
        HasVkMaxFragmentInputComponents(..),
        HasVkMaxFragmentOutputAttachments(..),
        HasVkMaxFrameAverageLightLevel(..), HasVkMaxFramebufferHeight(..),
        HasVkMaxFramebufferLayers(..), HasVkMaxFramebufferWidth(..),
        HasVkMaxGeometryInputComponents(..),
        HasVkMaxGeometryOutputComponents(..),
        HasVkMaxGeometryOutputVertices(..),
        HasVkMaxGeometryShaderInvocations(..),
        HasVkMaxGeometryTotalOutputComponents(..),
        HasVkMaxImageArrayLayers(..), HasVkMaxImageCount(..),
        HasVkMaxImageDimension1D(..), HasVkMaxImageDimension2D(..),
        HasVkMaxImageDimension3D(..), HasVkMaxImageDimensionCube(..),
        HasVkMaxImageExtent(..),
        HasVkMaxIndirectCommandsLayoutTokenCount(..),
        HasVkMaxInterpolationOffset(..), HasVkMaxLod(..),
        HasVkMaxLuminance(..), HasVkMaxMemoryAllocationCount(..),
        HasVkMaxMipLevels(..), HasVkMaxMultiviewInstanceIndex(..),
        HasVkMaxMultiviewViewCount(..), HasVkMaxObjectEntryCounts(..),
        HasVkMaxPerStageDescriptorInputAttachments(..),
        HasVkMaxPerStageDescriptorSampledImages(..),
        HasVkMaxPerStageDescriptorSamplers(..),
        HasVkMaxPerStageDescriptorStorageBuffers(..),
        HasVkMaxPerStageDescriptorStorageImages(..),
        HasVkMaxPerStageDescriptorUniformBuffers(..),
        HasVkMaxPerStageResources(..), HasVkMaxPipelineLayouts(..),
        HasVkMaxPushConstantsSize(..), HasVkMaxPushDescriptors(..),
        HasVkMaxResourceSize(..), HasVkMaxSampleLocationGridSize(..),
        HasVkMaxSampleMaskWords(..),
        HasVkMaxSampledImagesPerDescriptor(..),
        HasVkMaxSamplerAllocationCount(..), HasVkMaxSamplerAnisotropy(..),
        HasVkMaxSamplerLodBias(..), HasVkMaxSequencesCount(..),
        HasVkMaxSets(..), HasVkMaxSrcExtent(..), HasVkMaxSrcPosition(..),
        HasVkMaxStorageBufferRange(..),
        HasVkMaxStorageBuffersPerDescriptor(..),
        HasVkMaxStorageImagesPerDescriptor(..),
        HasVkMaxTessellationControlPerPatchOutputComponents(..),
        HasVkMaxTessellationControlPerVertexInputComponents(..),
        HasVkMaxTessellationControlPerVertexOutputComponents(..),
        HasVkMaxTessellationControlTotalOutputComponents(..),
        HasVkMaxTessellationEvaluationInputComponents(..),
        HasVkMaxTessellationEvaluationOutputComponents(..),
        HasVkMaxTessellationGenerationLevel(..),
        HasVkMaxTessellationPatchSize(..), HasVkMaxTexelBufferElements(..),
        HasVkMaxTexelGatherOffset(..), HasVkMaxTexelOffset(..),
        HasVkMaxUniformBufferRange(..),
        HasVkMaxUniformBuffersPerDescriptor(..),
        HasVkMaxVertexInputAttributeOffset(..),
        HasVkMaxVertexInputAttributes(..),
        HasVkMaxVertexInputBindingStride(..),
        HasVkMaxVertexInputBindings(..),
        HasVkMaxVertexOutputComponents(..),
        HasVkMaxViewportDimensionsArray(..), HasVkMaxViewports(..),
        HasVkMemory(..), HasVkMemoryDeviceIndex(..),
        HasVkMemoryHeapCount(..), HasVkMemoryHeapsArray(..),
        HasVkMemoryOffset(..), HasVkMemoryProperties(..),
        HasVkMemoryRequirements(..), HasVkMemoryTypeBits(..),
        HasVkMemoryTypeCount(..), HasVkMemoryTypeIndex(..),
        HasVkMemoryTypesArray(..),
        HasVkMinCommandsTokenBufferOffsetAlignment(..), HasVkMinDepth(..),
        HasVkMinDepthBounds(..), HasVkMinDstExtent(..),
        HasVkMinDstPosition(..), HasVkMinFilter(..),
        HasVkMinImageCount(..), HasVkMinImageExtent(..),
        HasVkMinImageTransferGranularity(..),
        HasVkMinImportedHostPointerAlignment(..),
        HasVkMinInterpolationOffset(..), HasVkMinLod(..),
        HasVkMinLuminance(..), HasVkMinMemoryMapAlignment(..),
        HasVkMinSampleShading(..),
        HasVkMinSequenceCountBufferOffsetAlignment(..),
        HasVkMinSequenceIndexBufferOffsetAlignment(..),
        HasVkMinSrcExtent(..), HasVkMinSrcPosition(..),
        HasVkMinStorageBufferOffsetAlignment(..),
        HasVkMinTexelBufferOffsetAlignment(..),
        HasVkMinTexelGatherOffset(..), HasVkMinTexelOffset(..),
        HasVkMinUniformBufferOffsetAlignment(..), HasVkMipLevel(..),
        HasVkMipLevels(..), HasVkMipLodBias(..), HasVkMipmapMode(..),
        HasVkMipmapPrecisionBits(..), HasVkMirSurface(..), HasVkMode(..),
        HasVkModes(..), HasVkModule(..), HasVkMultiDrawIndirect(..),
        HasVkMultiViewport(..), HasVkMultiview(..),
        HasVkMultiviewGeometryShader(..),
        HasVkMultiviewTessellationShader(..), HasVkName(..),
        HasVkNewLayout(..), HasVkNonCoherentAtomSize(..),
        HasVkNumAvailableSgprs(..), HasVkNumAvailableVgprs(..),
        HasVkNumPhysicalSgprs(..), HasVkNumPhysicalVgprs(..),
        HasVkNumUsedSgprs(..), HasVkNumUsedVgprs(..), HasVkObject(..),
        HasVkObjectCount(..), HasVkObjectTable(..), HasVkObjectType(..),
        HasVkOcclusionQueryEnable(..), HasVkOcclusionQueryPrecise(..),
        HasVkOffset(..), HasVkOldLayout(..), HasVkOldSwapchain(..),
        HasVkOptimalBufferCopyOffsetAlignment(..),
        HasVkOptimalBufferCopyRowPitchAlignment(..),
        HasVkOptimalTilingFeatures(..), HasVkPAcquireKeys(..),
        HasVkPAcquireSyncs(..), HasVkPAcquireTimeoutMilliseconds(..),
        HasVkPAcquireTimeouts(..), HasVkPApplicationInfo(..),
        HasVkPApplicationName(..), HasVkPAspectReferences(..),
        HasVkPAttachmentInitialSampleLocations(..), HasVkPAttachments(..),
        HasVkPAttributes(..), HasVkPBindings(..), HasVkPBinds(..),
        HasVkPBufferBinds(..), HasVkPBufferInfo(..), HasVkPClearValues(..),
        HasVkPCode(..), HasVkPColorAttachments(..),
        HasVkPColorBlendState(..), HasVkPCommandBufferDeviceMasks(..),
        HasVkPCommandBuffers(..), HasVkPCorrelationMasks(..),
        HasVkPCoverageModulationTable(..), HasVkPData(..),
        HasVkPDependencies(..), HasVkPDepthStencilAttachment(..),
        HasVkPDepthStencilState(..), HasVkPDescriptorUpdateEntries(..),
        HasVkPDeviceIndices(..), HasVkPDeviceMasks(..),
        HasVkPDeviceRenderAreas(..), HasVkPDisabledValidationChecks(..),
        HasVkPDiscardRectangles(..), HasVkPDynamicState(..),
        HasVkPDynamicStates(..), HasVkPEnabledFeatures(..),
        HasVkPEngineName(..), HasVkPHostPointer(..), HasVkPImageBinds(..),
        HasVkPImageIndices(..), HasVkPImageInfo(..),
        HasVkPImageOpaqueBinds(..), HasVkPImmutableSamplers(..),
        HasVkPIndirectCommandsTokens(..), HasVkPInheritanceInfo(..),
        HasVkPInitialData(..), HasVkPInputAssemblyState(..),
        HasVkPInputAttachments(..), HasVkPMapEntries(..),
        HasVkPMarkerName(..), HasVkPMultisampleState(..), HasVkPName(..),
        HasVkPNext(..), HasVkPObjectEntryCounts(..),
        HasVkPObjectEntryTypes(..), HasVkPObjectEntryUsageFlags(..),
        HasVkPObjectName(..), HasVkPPhysicalDevices(..),
        HasVkPPoolSizes(..), HasVkPPostSubpassSampleLocations(..),
        HasVkPPreserveAttachments(..), HasVkPPushConstantRanges(..),
        HasVkPQueueCreateInfos(..), HasVkPQueueFamilyIndices(..),
        HasVkPQueuePriorities(..), HasVkPRasterizationState(..),
        HasVkPRectangles(..), HasVkPRegions(..), HasVkPReleaseKeys(..),
        HasVkPReleaseSyncs(..), HasVkPResolveAttachments(..),
        HasVkPResults(..), HasVkPSFRRects(..), HasVkPSampleLocations(..),
        HasVkPSampleMask(..), HasVkPScissors(..), HasVkPSetLayouts(..),
        HasVkPSignalSemaphoreDeviceIndices(..),
        HasVkPSignalSemaphoreValues(..), HasVkPSignalSemaphores(..),
        HasVkPSpecializationInfo(..), HasVkPStages(..),
        HasVkPSubpasses(..), HasVkPSwapchains(..), HasVkPTag(..),
        HasVkPTessellationState(..), HasVkPTexelBufferView(..),
        HasVkPTimes(..), HasVkPTokens(..), HasVkPUserData(..),
        HasVkPVertexAttributeDescriptions(..),
        HasVkPVertexBindingDescriptions(..), HasVkPVertexInputState(..),
        HasVkPView(..), HasVkPViewFormats(..), HasVkPViewMasks(..),
        HasVkPViewOffsets(..), HasVkPViewportState(..),
        HasVkPViewportSwizzles(..), HasVkPViewportWScalings(..),
        HasVkPViewports(..), HasVkPWaitDstStageMask(..),
        HasVkPWaitSemaphoreDeviceIndices(..),
        HasVkPWaitSemaphoreValues(..), HasVkPWaitSemaphores(..),
        HasVkParameters(..), HasVkPassOp(..), HasVkPatchControlPoints(..),
        HasVkPerViewPositionAllComponents(..), HasVkPersistent(..),
        HasVkPersistentContent(..), HasVkPfnAllocation(..),
        HasVkPfnCallback(..), HasVkPfnFree(..),
        HasVkPfnInternalAllocation(..), HasVkPfnInternalFree(..),
        HasVkPfnReallocation(..), HasVkPhysicalDeviceCount(..),
        HasVkPhysicalDevicesArray(..), HasVkPhysicalDimensions(..),
        HasVkPhysicalResolution(..), HasVkPipeline(..),
        HasVkPipelineBindPoint(..), HasVkPipelineCacheUUIDArray(..),
        HasVkPipelineLayout(..), HasVkPipelineStatistics(..),
        HasVkPipelineStatisticsQuery(..), HasVkPlaneAspect(..),
        HasVkPlaneIndex(..), HasVkPlaneReorderPossible(..),
        HasVkPlaneStackIndex(..), HasVkPointClippingBehavior(..),
        HasVkPointSizeGranularity(..), HasVkPointSizeRangeArray(..),
        HasVkPolygonMode(..), HasVkPoolSizeCount(..),
        HasVkPostSubpassSampleLocationsCount(..), HasVkPowerState(..),
        HasVkPpEnabledExtensionNames(..), HasVkPpEnabledLayerNames(..),
        HasVkPreTransform(..), HasVkPrefersDedicatedAllocation(..),
        HasVkPresentID(..), HasVkPresentMargin(..),
        HasVkPresentMaskArray(..), HasVkPresentMode(..),
        HasVkPreserveAttachmentCount(..),
        HasVkPrimitiveOverestimationSize(..),
        HasVkPrimitiveRestartEnable(..), HasVkPrimitiveUnderestimation(..),
        HasVkProperties(..), HasVkPropertyFlags(..),
        HasVkPushConstantRangeCount(..), HasVkQueryCount(..),
        HasVkQueryFlags(..), HasVkQueryType(..), HasVkQueueCount(..),
        HasVkQueueCreateInfoCount(..), HasVkQueueFamilyIndex(..),
        HasVkQueueFamilyIndexCount(..), HasVkQueueFamilyProperties(..),
        HasVkQueueFlags(..), HasVkR(..), HasVkRange(..),
        HasVkRasterizationOrder(..), HasVkRasterizationSamples(..),
        HasVkRasterizerDiscardEnable(..), HasVkRect(..),
        HasVkRectangleCount(..), HasVkReductionMode(..),
        HasVkReference(..), HasVkRefreshDuration(..), HasVkRefreshRate(..),
        HasVkReleaseCount(..), HasVkRenderArea(..), HasVkRenderPass(..),
        HasVkRequiresDedicatedAllocation(..),
        HasVkResidencyAlignedMipSize(..),
        HasVkResidencyNonResidentStrict(..),
        HasVkResidencyStandard2DBlockShape(..),
        HasVkResidencyStandard2DMultisampleBlockShape(..),
        HasVkResidencyStandard3DBlockShape(..),
        HasVkResourceDeviceIndex(..), HasVkResourceOffset(..),
        HasVkResourceUsage(..), HasVkRobustBufferAccess(..),
        HasVkRowPitch(..), HasVkSFRRectCount(..), HasVkSType(..),
        HasVkSampleCounts(..), HasVkSampleLocationCoordinateRangeArray(..),
        HasVkSampleLocationGridSize(..),
        HasVkSampleLocationSampleCounts(..),
        HasVkSampleLocationSubPixelBits(..), HasVkSampleLocationsCount(..),
        HasVkSampleLocationsEnable(..), HasVkSampleLocationsInfo(..),
        HasVkSampleLocationsPerPixel(..), HasVkSampleRateShading(..),
        HasVkSampleShadingEnable(..),
        HasVkSampledImageColorSampleCounts(..),
        HasVkSampledImageDepthSampleCounts(..),
        HasVkSampledImageIntegerSampleCounts(..),
        HasVkSampledImageStencilSampleCounts(..), HasVkSampler(..),
        HasVkSamplerAnisotropy(..), HasVkSamplerYcbcrConversion(..),
        HasVkSamples(..), HasVkScissorCount(..),
        HasVkScratchMemUsageInBytes(..), HasVkSemaphore(..),
        HasVkSequencesCountBuffer(..), HasVkSequencesCountOffset(..),
        HasVkSequencesIndexBuffer(..), HasVkSequencesIndexOffset(..),
        HasVkSet(..), HasVkSetLayoutCount(..), HasVkShaderClipDistance(..),
        HasVkShaderCullDistance(..), HasVkShaderFloat64(..),
        HasVkShaderImageGatherExtended(..), HasVkShaderInt16(..),
        HasVkShaderInt64(..), HasVkShaderResourceMinLod(..),
        HasVkShaderResourceResidency(..),
        HasVkShaderSampledImageArrayDynamicIndexing(..),
        HasVkShaderStageMask(..),
        HasVkShaderStorageBufferArrayDynamicIndexing(..),
        HasVkShaderStorageImageArrayDynamicIndexing(..),
        HasVkShaderStorageImageExtendedFormats(..),
        HasVkShaderStorageImageMultisample(..),
        HasVkShaderStorageImageReadWithoutFormat(..),
        HasVkShaderStorageImageWriteWithoutFormat(..),
        HasVkShaderTessellationAndGeometryPointSize(..),
        HasVkShaderUniformBufferArrayDynamicIndexing(..),
        HasVkSharedPresentSupportedUsageFlags(..), HasVkSharingMode(..),
        HasVkSignalSemaphoreCount(..), HasVkSignalSemaphoreValuesCount(..),
        HasVkSize(..), HasVkSparseAddressSpaceSize(..),
        HasVkSparseBinding(..), HasVkSparseProperties(..),
        HasVkSparseResidency16Samples(..),
        HasVkSparseResidency2Samples(..), HasVkSparseResidency4Samples(..),
        HasVkSparseResidency8Samples(..), HasVkSparseResidencyAliased(..),
        HasVkSparseResidencyBuffer(..), HasVkSparseResidencyImage2D(..),
        HasVkSparseResidencyImage3D(..), HasVkSpecVersion(..),
        HasVkSrcAccessMask(..), HasVkSrcAlphaBlendFactor(..),
        HasVkSrcArrayElement(..), HasVkSrcBinding(..),
        HasVkSrcColorBlendFactor(..), HasVkSrcOffset(..),
        HasVkSrcOffsetsArray(..), HasVkSrcPremultiplied(..),
        HasVkSrcQueueFamilyIndex(..), HasVkSrcRect(..), HasVkSrcSet(..),
        HasVkSrcStageMask(..), HasVkSrcSubpass(..),
        HasVkSrcSubresource(..), HasVkStage(..), HasVkStageCount(..),
        HasVkStageFlags(..), HasVkStandardSampleLocations(..),
        HasVkStencil(..), HasVkStencilLoadOp(..), HasVkStencilStoreOp(..),
        HasVkStencilTestEnable(..), HasVkStorageBuffer16BitAccess(..),
        HasVkStorageImageSampleCounts(..), HasVkStorageInputOutput16(..),
        HasVkStoragePushConstant16(..), HasVkStoreOp(..),
        HasVkStrictLines(..), HasVkStride(..),
        HasVkSubPixelInterpolationOffsetBits(..),
        HasVkSubPixelPrecisionBits(..), HasVkSubTexelPrecisionBits(..),
        HasVkSubpass(..), HasVkSubpassCount(..), HasVkSubpassIndex(..),
        HasVkSubresource(..), HasVkSubresourceRange(..),
        HasVkSubsetAllocation(..), HasVkSupportedAlpha(..),
        HasVkSupportedCompositeAlpha(..),
        HasVkSupportedSurfaceCounters(..), HasVkSupportedTransforms(..),
        HasVkSupportedUsageFlags(..),
        HasVkSupportsTextureGatherLODBiasAMD(..), HasVkSurface(..),
        HasVkSurfaceCapabilities(..), HasVkSurfaceCounters(..),
        HasVkSurfaceFormat(..), HasVkSwapchain(..),
        HasVkSwapchainCount(..), HasVkTagName(..), HasVkTagSize(..),
        HasVkTargetCommandBuffer(..), HasVkTemplateType(..),
        HasVkTessellationShader(..), HasVkTextureCompressionASTC_LDR(..),
        HasVkTextureCompressionBC(..), HasVkTextureCompressionETC2(..),
        HasVkTiling(..), HasVkTimeout(..),
        HasVkTimestampComputeAndGraphics(..), HasVkTimestampPeriod(..),
        HasVkTimestampValidBits(..), HasVkTokenCount(..),
        HasVkTokenType(..), HasVkTopology(..), HasVkTransform(..),
        HasVkType(..), HasVkUint32Array(..),
        HasVkUniformAndStorageBuffer16BitAccess(..),
        HasVkUnnormalizedCoordinates(..), HasVkUsage(..),
        HasVkValidationCache(..), HasVkVariableMultisampleRate(..),
        HasVkVariablePointers(..), HasVkVariablePointersStorageBuffer(..),
        HasVkVariableSampleLocations(..), HasVkVendorID(..),
        HasVkVertexAttributeDescriptionCount(..),
        HasVkVertexBindingDescriptionCount(..), HasVkVertexCount(..),
        HasVkVertexOffset(..), HasVkVertexPipelineStoresAndAtomics(..),
        HasVkViewFormatCount(..), HasVkViewType(..),
        HasVkViewportBoundsRangeArray(..), HasVkViewportCount(..),
        HasVkViewportSubPixelBits(..), HasVkViewportWScalingEnable(..),
        HasVkVisibleRegion(..), HasVkW(..), HasVkWaitSemaphoreCount(..),
        HasVkWaitSemaphoreValuesCount(..), HasVkWhitePoint(..),
        HasVkWideLines(..), HasVkWidth(..), HasVkWindow(..),
        HasVkWriteMask(..), HasVkX(..), HasVkXChromaOffset(..),
        HasVkXcoeff(..), HasVkY(..), HasVkYChromaOffset(..),
        HasVkYcbcrModel(..), HasVkYcbcrRange(..), HasVkYcoeff(..),
        HasVkZ(..))
       where
import           GHC.TypeLits            (ErrorMessage (..), TypeError)
import           Graphics.Vulkan.Marshal

class HasVkA a where
        type VkAMType a :: *

        vkA :: a -> VkAMType a

        vkAByteOffset :: a -> Int

        readVkA :: Ptr a -> IO (VkAMType a)

        writeVkA :: Ptr a -> VkAMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'a'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkA a

class HasVkAcquireCount a where
        type VkAcquireCountMType a :: *

        vkAcquireCount :: a -> VkAcquireCountMType a

        vkAcquireCountByteOffset :: a -> Int

        readVkAcquireCount :: Ptr a -> IO (VkAcquireCountMType a)

        writeVkAcquireCount :: Ptr a -> VkAcquireCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'acquireCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAcquireCount a

class HasVkActualPresentTime a where
        type VkActualPresentTimeMType a :: *

        vkActualPresentTime :: a -> VkActualPresentTimeMType a

        vkActualPresentTimeByteOffset :: a -> Int

        readVkActualPresentTime :: Ptr a -> IO (VkActualPresentTimeMType a)

        writeVkActualPresentTime ::
                                 Ptr a -> VkActualPresentTimeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'actualPresentTime'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkActualPresentTime a

class HasVkAddressModeU a where
        type VkAddressModeUMType a :: *

        vkAddressModeU :: a -> VkAddressModeUMType a

        vkAddressModeUByteOffset :: a -> Int

        readVkAddressModeU :: Ptr a -> IO (VkAddressModeUMType a)

        writeVkAddressModeU :: Ptr a -> VkAddressModeUMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'addressModeU'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAddressModeU a

class HasVkAddressModeV a where
        type VkAddressModeVMType a :: *

        vkAddressModeV :: a -> VkAddressModeVMType a

        vkAddressModeVByteOffset :: a -> Int

        readVkAddressModeV :: Ptr a -> IO (VkAddressModeVMType a)

        writeVkAddressModeV :: Ptr a -> VkAddressModeVMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'addressModeV'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAddressModeV a

class HasVkAddressModeW a where
        type VkAddressModeWMType a :: *

        vkAddressModeW :: a -> VkAddressModeWMType a

        vkAddressModeWByteOffset :: a -> Int

        readVkAddressModeW :: Ptr a -> IO (VkAddressModeWMType a)

        writeVkAddressModeW :: Ptr a -> VkAddressModeWMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'addressModeW'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAddressModeW a

class HasVkAdvancedBlendAllOperations a where
        type VkAdvancedBlendAllOperationsMType a :: *

        vkAdvancedBlendAllOperations ::
                                     a -> VkAdvancedBlendAllOperationsMType a

        vkAdvancedBlendAllOperationsByteOffset :: a -> Int

        readVkAdvancedBlendAllOperations ::
                                         Ptr a -> IO (VkAdvancedBlendAllOperationsMType a)

        writeVkAdvancedBlendAllOperations ::
                                          Ptr a -> VkAdvancedBlendAllOperationsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'advancedBlendAllOperations'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAdvancedBlendAllOperations a

class HasVkAdvancedBlendCoherentOperations a where
        type VkAdvancedBlendCoherentOperationsMType a :: *

        vkAdvancedBlendCoherentOperations ::
                                          a -> VkAdvancedBlendCoherentOperationsMType a

        vkAdvancedBlendCoherentOperationsByteOffset :: a -> Int

        readVkAdvancedBlendCoherentOperations ::
                                              Ptr a -> IO (VkAdvancedBlendCoherentOperationsMType a)

        writeVkAdvancedBlendCoherentOperations ::
                                               Ptr a ->
                                                 VkAdvancedBlendCoherentOperationsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'advancedBlendCoherentOperations'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAdvancedBlendCoherentOperations a

class HasVkAdvancedBlendCorrelatedOverlap a where
        type VkAdvancedBlendCorrelatedOverlapMType a :: *

        vkAdvancedBlendCorrelatedOverlap ::
                                         a -> VkAdvancedBlendCorrelatedOverlapMType a

        vkAdvancedBlendCorrelatedOverlapByteOffset :: a -> Int

        readVkAdvancedBlendCorrelatedOverlap ::
                                             Ptr a -> IO (VkAdvancedBlendCorrelatedOverlapMType a)

        writeVkAdvancedBlendCorrelatedOverlap ::
                                              Ptr a ->
                                                VkAdvancedBlendCorrelatedOverlapMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'advancedBlendCorrelatedOverlap'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAdvancedBlendCorrelatedOverlap a

class HasVkAdvancedBlendIndependentBlend a where
        type VkAdvancedBlendIndependentBlendMType a :: *

        vkAdvancedBlendIndependentBlend ::
                                        a -> VkAdvancedBlendIndependentBlendMType a

        vkAdvancedBlendIndependentBlendByteOffset :: a -> Int

        readVkAdvancedBlendIndependentBlend ::
                                            Ptr a -> IO (VkAdvancedBlendIndependentBlendMType a)

        writeVkAdvancedBlendIndependentBlend ::
                                             Ptr a ->
                                               VkAdvancedBlendIndependentBlendMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'advancedBlendIndependentBlend'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAdvancedBlendIndependentBlend a

class HasVkAdvancedBlendMaxColorAttachments a where
        type VkAdvancedBlendMaxColorAttachmentsMType a :: *

        vkAdvancedBlendMaxColorAttachments ::
                                           a -> VkAdvancedBlendMaxColorAttachmentsMType a

        vkAdvancedBlendMaxColorAttachmentsByteOffset :: a -> Int

        readVkAdvancedBlendMaxColorAttachments ::
                                               Ptr a ->
                                                 IO (VkAdvancedBlendMaxColorAttachmentsMType a)

        writeVkAdvancedBlendMaxColorAttachments ::
                                                Ptr a ->
                                                  VkAdvancedBlendMaxColorAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'advancedBlendMaxColorAttachments'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAdvancedBlendMaxColorAttachments a

class HasVkAdvancedBlendNonPremultipliedDstColor a where
        type VkAdvancedBlendNonPremultipliedDstColorMType a :: *

        vkAdvancedBlendNonPremultipliedDstColor ::
                                                a -> VkAdvancedBlendNonPremultipliedDstColorMType a

        vkAdvancedBlendNonPremultipliedDstColorByteOffset :: a -> Int

        readVkAdvancedBlendNonPremultipliedDstColor ::
                                                    Ptr a ->
                                                      IO
                                                        (VkAdvancedBlendNonPremultipliedDstColorMType
                                                           a)

        writeVkAdvancedBlendNonPremultipliedDstColor ::
                                                     Ptr a ->
                                                       VkAdvancedBlendNonPremultipliedDstColorMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'advancedBlendNonPremultipliedDstColor'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAdvancedBlendNonPremultipliedDstColor a

class HasVkAdvancedBlendNonPremultipliedSrcColor a where
        type VkAdvancedBlendNonPremultipliedSrcColorMType a :: *

        vkAdvancedBlendNonPremultipliedSrcColor ::
                                                a -> VkAdvancedBlendNonPremultipliedSrcColorMType a

        vkAdvancedBlendNonPremultipliedSrcColorByteOffset :: a -> Int

        readVkAdvancedBlendNonPremultipliedSrcColor ::
                                                    Ptr a ->
                                                      IO
                                                        (VkAdvancedBlendNonPremultipliedSrcColorMType
                                                           a)

        writeVkAdvancedBlendNonPremultipliedSrcColor ::
                                                     Ptr a ->
                                                       VkAdvancedBlendNonPremultipliedSrcColorMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'advancedBlendNonPremultipliedSrcColor'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAdvancedBlendNonPremultipliedSrcColor a

class HasVkAlignment a where
        type VkAlignmentMType a :: *

        vkAlignment :: a -> VkAlignmentMType a

        vkAlignmentByteOffset :: a -> Int

        readVkAlignment :: Ptr a -> IO (VkAlignmentMType a)

        writeVkAlignment :: Ptr a -> VkAlignmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'alignment'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAlignment a

class HasVkAllocationSize a where
        type VkAllocationSizeMType a :: *

        vkAllocationSize :: a -> VkAllocationSizeMType a

        vkAllocationSizeByteOffset :: a -> Int

        readVkAllocationSize :: Ptr a -> IO (VkAllocationSizeMType a)

        writeVkAllocationSize :: Ptr a -> VkAllocationSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'allocationSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAllocationSize a

class HasVkAlphaBlendOp a where
        type VkAlphaBlendOpMType a :: *

        vkAlphaBlendOp :: a -> VkAlphaBlendOpMType a

        vkAlphaBlendOpByteOffset :: a -> Int

        readVkAlphaBlendOp :: Ptr a -> IO (VkAlphaBlendOpMType a)

        writeVkAlphaBlendOp :: Ptr a -> VkAlphaBlendOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'alphaBlendOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAlphaBlendOp a

class HasVkAlphaMode a where
        type VkAlphaModeMType a :: *

        vkAlphaMode :: a -> VkAlphaModeMType a

        vkAlphaModeByteOffset :: a -> Int

        readVkAlphaMode :: Ptr a -> IO (VkAlphaModeMType a)

        writeVkAlphaMode :: Ptr a -> VkAlphaModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'alphaMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAlphaMode a

class HasVkAlphaToCoverageEnable a where
        type VkAlphaToCoverageEnableMType a :: *

        vkAlphaToCoverageEnable :: a -> VkAlphaToCoverageEnableMType a

        vkAlphaToCoverageEnableByteOffset :: a -> Int

        readVkAlphaToCoverageEnable ::
                                    Ptr a -> IO (VkAlphaToCoverageEnableMType a)

        writeVkAlphaToCoverageEnable ::
                                     Ptr a -> VkAlphaToCoverageEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'alphaToCoverageEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAlphaToCoverageEnable a

class HasVkAlphaToOne a where
        type VkAlphaToOneMType a :: *

        vkAlphaToOne :: a -> VkAlphaToOneMType a

        vkAlphaToOneByteOffset :: a -> Int

        readVkAlphaToOne :: Ptr a -> IO (VkAlphaToOneMType a)

        writeVkAlphaToOne :: Ptr a -> VkAlphaToOneMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'alphaToOne'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAlphaToOne a

class HasVkAlphaToOneEnable a where
        type VkAlphaToOneEnableMType a :: *

        vkAlphaToOneEnable :: a -> VkAlphaToOneEnableMType a

        vkAlphaToOneEnableByteOffset :: a -> Int

        readVkAlphaToOneEnable :: Ptr a -> IO (VkAlphaToOneEnableMType a)

        writeVkAlphaToOneEnable ::
                                Ptr a -> VkAlphaToOneEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'alphaToOneEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAlphaToOneEnable a

class HasVkAnisotropyEnable a where
        type VkAnisotropyEnableMType a :: *

        vkAnisotropyEnable :: a -> VkAnisotropyEnableMType a

        vkAnisotropyEnableByteOffset :: a -> Int

        readVkAnisotropyEnable :: Ptr a -> IO (VkAnisotropyEnableMType a)

        writeVkAnisotropyEnable ::
                                Ptr a -> VkAnisotropyEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'anisotropyEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAnisotropyEnable a

class HasVkApiVersion a where
        type VkApiVersionMType a :: *

        vkApiVersion :: a -> VkApiVersionMType a

        vkApiVersionByteOffset :: a -> Int

        readVkApiVersion :: Ptr a -> IO (VkApiVersionMType a)

        writeVkApiVersion :: Ptr a -> VkApiVersionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'apiVersion'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkApiVersion a

class HasVkApplicationVersion a where
        type VkApplicationVersionMType a :: *

        vkApplicationVersion :: a -> VkApplicationVersionMType a

        vkApplicationVersionByteOffset :: a -> Int

        readVkApplicationVersion ::
                                 Ptr a -> IO (VkApplicationVersionMType a)

        writeVkApplicationVersion ::
                                  Ptr a -> VkApplicationVersionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'applicationVersion'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkApplicationVersion a

class HasVkArrayLayer a where
        type VkArrayLayerMType a :: *

        vkArrayLayer :: a -> VkArrayLayerMType a

        vkArrayLayerByteOffset :: a -> Int

        readVkArrayLayer :: Ptr a -> IO (VkArrayLayerMType a)

        writeVkArrayLayer :: Ptr a -> VkArrayLayerMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'arrayLayer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkArrayLayer a

class HasVkArrayLayers a where
        type VkArrayLayersMType a :: *

        vkArrayLayers :: a -> VkArrayLayersMType a

        vkArrayLayersByteOffset :: a -> Int

        readVkArrayLayers :: Ptr a -> IO (VkArrayLayersMType a)

        writeVkArrayLayers :: Ptr a -> VkArrayLayersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'arrayLayers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkArrayLayers a

class HasVkArrayPitch a where
        type VkArrayPitchMType a :: *

        vkArrayPitch :: a -> VkArrayPitchMType a

        vkArrayPitchByteOffset :: a -> Int

        readVkArrayPitch :: Ptr a -> IO (VkArrayPitchMType a)

        writeVkArrayPitch :: Ptr a -> VkArrayPitchMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'arrayPitch'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkArrayPitch a

class HasVkAspectMask a where
        type VkAspectMaskMType a :: *

        vkAspectMask :: a -> VkAspectMaskMType a

        vkAspectMaskByteOffset :: a -> Int

        readVkAspectMask :: Ptr a -> IO (VkAspectMaskMType a)

        writeVkAspectMask :: Ptr a -> VkAspectMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'aspectMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAspectMask a

class HasVkAspectReferenceCount a where
        type VkAspectReferenceCountMType a :: *

        vkAspectReferenceCount :: a -> VkAspectReferenceCountMType a

        vkAspectReferenceCountByteOffset :: a -> Int

        readVkAspectReferenceCount ::
                                   Ptr a -> IO (VkAspectReferenceCountMType a)

        writeVkAspectReferenceCount ::
                                    Ptr a -> VkAspectReferenceCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'aspectReferenceCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAspectReferenceCount a

class HasVkAttachment a where
        type VkAttachmentMType a :: *

        vkAttachment :: a -> VkAttachmentMType a

        vkAttachmentByteOffset :: a -> Int

        readVkAttachment :: Ptr a -> IO (VkAttachmentMType a)

        writeVkAttachment :: Ptr a -> VkAttachmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'attachment'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAttachment a

class HasVkAttachmentCount a where
        type VkAttachmentCountMType a :: *

        vkAttachmentCount :: a -> VkAttachmentCountMType a

        vkAttachmentCountByteOffset :: a -> Int

        readVkAttachmentCount :: Ptr a -> IO (VkAttachmentCountMType a)

        writeVkAttachmentCount ::
                               Ptr a -> VkAttachmentCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'attachmentCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAttachmentCount a

class HasVkAttachmentIndex a where
        type VkAttachmentIndexMType a :: *

        vkAttachmentIndex :: a -> VkAttachmentIndexMType a

        vkAttachmentIndexByteOffset :: a -> Int

        readVkAttachmentIndex :: Ptr a -> IO (VkAttachmentIndexMType a)

        writeVkAttachmentIndex ::
                               Ptr a -> VkAttachmentIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'attachmentIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAttachmentIndex a

class HasVkAttachmentInitialSampleLocationsCount a where
        type VkAttachmentInitialSampleLocationsCountMType a :: *

        vkAttachmentInitialSampleLocationsCount ::
                                                a -> VkAttachmentInitialSampleLocationsCountMType a

        vkAttachmentInitialSampleLocationsCountByteOffset :: a -> Int

        readVkAttachmentInitialSampleLocationsCount ::
                                                    Ptr a ->
                                                      IO
                                                        (VkAttachmentInitialSampleLocationsCountMType
                                                           a)

        writeVkAttachmentInitialSampleLocationsCount ::
                                                     Ptr a ->
                                                       VkAttachmentInitialSampleLocationsCountMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'attachmentInitialSampleLocationsCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkAttachmentInitialSampleLocationsCount a

class HasVkB a where
        type VkBMType a :: *

        vkB :: a -> VkBMType a

        vkBByteOffset :: a -> Int

        readVkB :: Ptr a -> IO (VkBMType a)

        writeVkB :: Ptr a -> VkBMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'b'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkB a

class HasVkBack a where
        type VkBackMType a :: *

        vkBack :: a -> VkBackMType a

        vkBackByteOffset :: a -> Int

        readVkBack :: Ptr a -> IO (VkBackMType a)

        writeVkBack :: Ptr a -> VkBackMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'back'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBack a

class HasVkBaseArrayLayer a where
        type VkBaseArrayLayerMType a :: *

        vkBaseArrayLayer :: a -> VkBaseArrayLayerMType a

        vkBaseArrayLayerByteOffset :: a -> Int

        readVkBaseArrayLayer :: Ptr a -> IO (VkBaseArrayLayerMType a)

        writeVkBaseArrayLayer :: Ptr a -> VkBaseArrayLayerMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'baseArrayLayer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBaseArrayLayer a

class HasVkBaseMipLevel a where
        type VkBaseMipLevelMType a :: *

        vkBaseMipLevel :: a -> VkBaseMipLevelMType a

        vkBaseMipLevelByteOffset :: a -> Int

        readVkBaseMipLevel :: Ptr a -> IO (VkBaseMipLevelMType a)

        writeVkBaseMipLevel :: Ptr a -> VkBaseMipLevelMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'baseMipLevel'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBaseMipLevel a

class HasVkBasePipelineHandle a where
        type VkBasePipelineHandleMType a :: *

        vkBasePipelineHandle :: a -> VkBasePipelineHandleMType a

        vkBasePipelineHandleByteOffset :: a -> Int

        readVkBasePipelineHandle ::
                                 Ptr a -> IO (VkBasePipelineHandleMType a)

        writeVkBasePipelineHandle ::
                                  Ptr a -> VkBasePipelineHandleMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'basePipelineHandle'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBasePipelineHandle a

class HasVkBasePipelineIndex a where
        type VkBasePipelineIndexMType a :: *

        vkBasePipelineIndex :: a -> VkBasePipelineIndexMType a

        vkBasePipelineIndexByteOffset :: a -> Int

        readVkBasePipelineIndex :: Ptr a -> IO (VkBasePipelineIndexMType a)

        writeVkBasePipelineIndex ::
                                 Ptr a -> VkBasePipelineIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'basePipelineIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBasePipelineIndex a

class HasVkBindCount a where
        type VkBindCountMType a :: *

        vkBindCount :: a -> VkBindCountMType a

        vkBindCountByteOffset :: a -> Int

        readVkBindCount :: Ptr a -> IO (VkBindCountMType a)

        writeVkBindCount :: Ptr a -> VkBindCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bindCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBindCount a

class HasVkBinding a where
        type VkBindingMType a :: *

        vkBinding :: a -> VkBindingMType a

        vkBindingByteOffset :: a -> Int

        readVkBinding :: Ptr a -> IO (VkBindingMType a)

        writeVkBinding :: Ptr a -> VkBindingMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'binding'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBinding a

class HasVkBindingCount a where
        type VkBindingCountMType a :: *

        vkBindingCount :: a -> VkBindingCountMType a

        vkBindingCountByteOffset :: a -> Int

        readVkBindingCount :: Ptr a -> IO (VkBindingCountMType a)

        writeVkBindingCount :: Ptr a -> VkBindingCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bindingCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBindingCount a

class HasVkBindingUnit a where
        type VkBindingUnitMType a :: *

        vkBindingUnit :: a -> VkBindingUnitMType a

        vkBindingUnitByteOffset :: a -> Int

        readVkBindingUnit :: Ptr a -> IO (VkBindingUnitMType a)

        writeVkBindingUnit :: Ptr a -> VkBindingUnitMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bindingUnit'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBindingUnit a

class HasVkBlendConstantsArray a where
        type VkBlendConstantsArrayMType a :: *

        vkBlendConstantsArray :: a -> Int -> VkBlendConstantsArrayMType a

        vkBlendConstantsArrayByteOffset :: a -> Int

        readVkBlendConstantsArray ::
                                  Ptr a -> Int -> IO (VkBlendConstantsArrayMType a)

        writeVkBlendConstantsArray ::
                                   Ptr a -> Int -> VkBlendConstantsArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'blendConstants'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBlendConstantsArray a

class HasVkBlendEnable a where
        type VkBlendEnableMType a :: *

        vkBlendEnable :: a -> VkBlendEnableMType a

        vkBlendEnableByteOffset :: a -> Int

        readVkBlendEnable :: Ptr a -> IO (VkBlendEnableMType a)

        writeVkBlendEnable :: Ptr a -> VkBlendEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'blendEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBlendEnable a

class HasVkBlendOverlap a where
        type VkBlendOverlapMType a :: *

        vkBlendOverlap :: a -> VkBlendOverlapMType a

        vkBlendOverlapByteOffset :: a -> Int

        readVkBlendOverlap :: Ptr a -> IO (VkBlendOverlapMType a)

        writeVkBlendOverlap :: Ptr a -> VkBlendOverlapMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'blendOverlap'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBlendOverlap a

class HasVkBorderColor a where
        type VkBorderColorMType a :: *

        vkBorderColor :: a -> VkBorderColorMType a

        vkBorderColorByteOffset :: a -> Int

        readVkBorderColor :: Ptr a -> IO (VkBorderColorMType a)

        writeVkBorderColor :: Ptr a -> VkBorderColorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'borderColor'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBorderColor a

class HasVkBuffer a where
        type VkBufferMType a :: *

        vkBuffer :: a -> VkBufferMType a

        vkBufferByteOffset :: a -> Int

        readVkBuffer :: Ptr a -> IO (VkBufferMType a)

        writeVkBuffer :: Ptr a -> VkBufferMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'buffer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBuffer a

class HasVkBufferBindCount a where
        type VkBufferBindCountMType a :: *

        vkBufferBindCount :: a -> VkBufferBindCountMType a

        vkBufferBindCountByteOffset :: a -> Int

        readVkBufferBindCount :: Ptr a -> IO (VkBufferBindCountMType a)

        writeVkBufferBindCount ::
                               Ptr a -> VkBufferBindCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bufferBindCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBufferBindCount a

class HasVkBufferFeatures a where
        type VkBufferFeaturesMType a :: *

        vkBufferFeatures :: a -> VkBufferFeaturesMType a

        vkBufferFeaturesByteOffset :: a -> Int

        readVkBufferFeatures :: Ptr a -> IO (VkBufferFeaturesMType a)

        writeVkBufferFeatures :: Ptr a -> VkBufferFeaturesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bufferFeatures'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBufferFeatures a

class HasVkBufferImageGranularity a where
        type VkBufferImageGranularityMType a :: *

        vkBufferImageGranularity :: a -> VkBufferImageGranularityMType a

        vkBufferImageGranularityByteOffset :: a -> Int

        readVkBufferImageGranularity ::
                                     Ptr a -> IO (VkBufferImageGranularityMType a)

        writeVkBufferImageGranularity ::
                                      Ptr a -> VkBufferImageGranularityMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bufferImageGranularity'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBufferImageGranularity a

class HasVkBufferImageHeight a where
        type VkBufferImageHeightMType a :: *

        vkBufferImageHeight :: a -> VkBufferImageHeightMType a

        vkBufferImageHeightByteOffset :: a -> Int

        readVkBufferImageHeight :: Ptr a -> IO (VkBufferImageHeightMType a)

        writeVkBufferImageHeight ::
                                 Ptr a -> VkBufferImageHeightMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bufferImageHeight'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBufferImageHeight a

class HasVkBufferOffset a where
        type VkBufferOffsetMType a :: *

        vkBufferOffset :: a -> VkBufferOffsetMType a

        vkBufferOffsetByteOffset :: a -> Int

        readVkBufferOffset :: Ptr a -> IO (VkBufferOffsetMType a)

        writeVkBufferOffset :: Ptr a -> VkBufferOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bufferOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBufferOffset a

class HasVkBufferRowLength a where
        type VkBufferRowLengthMType a :: *

        vkBufferRowLength :: a -> VkBufferRowLengthMType a

        vkBufferRowLengthByteOffset :: a -> Int

        readVkBufferRowLength :: Ptr a -> IO (VkBufferRowLengthMType a)

        writeVkBufferRowLength ::
                               Ptr a -> VkBufferRowLengthMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'bufferRowLength'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkBufferRowLength a

class HasVkChromaFilter a where
        type VkChromaFilterMType a :: *

        vkChromaFilter :: a -> VkChromaFilterMType a

        vkChromaFilterByteOffset :: a -> Int

        readVkChromaFilter :: Ptr a -> IO (VkChromaFilterMType a)

        writeVkChromaFilter :: Ptr a -> VkChromaFilterMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'chromaFilter'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkChromaFilter a

class HasVkClearValue a where
        type VkClearValueMType a :: *

        vkClearValue :: a -> VkClearValueMType a

        vkClearValueByteOffset :: a -> Int

        readVkClearValue :: Ptr a -> IO (VkClearValueMType a)

        writeVkClearValue :: Ptr a -> VkClearValueMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'clearValue'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkClearValue a

class HasVkClearValueCount a where
        type VkClearValueCountMType a :: *

        vkClearValueCount :: a -> VkClearValueCountMType a

        vkClearValueCountByteOffset :: a -> Int

        readVkClearValueCount :: Ptr a -> IO (VkClearValueCountMType a)

        writeVkClearValueCount ::
                               Ptr a -> VkClearValueCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'clearValueCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkClearValueCount a

class HasVkClipped a where
        type VkClippedMType a :: *

        vkClipped :: a -> VkClippedMType a

        vkClippedByteOffset :: a -> Int

        readVkClipped :: Ptr a -> IO (VkClippedMType a)

        writeVkClipped :: Ptr a -> VkClippedMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'clipped'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkClipped a

class HasVkCodeSize a where
        type VkCodeSizeMType a :: *

        vkCodeSize :: a -> VkCodeSizeMType a

        vkCodeSizeByteOffset :: a -> Int

        readVkCodeSize :: Ptr a -> IO (VkCodeSizeMType a)

        writeVkCodeSize :: Ptr a -> VkCodeSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'codeSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCodeSize a

class HasVkColor a where
        type VkColorMType a :: *

        vkColor :: a -> VkColorMType a

        vkColorByteOffset :: a -> Int

        readVkColor :: Ptr a -> IO (VkColorMType a)

        writeVkColor :: Ptr a -> VkColorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'color'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkColor a

class HasVkColorArray a where
        type VkColorArrayMType a :: *

        vkColorArray :: a -> Int -> VkColorArrayMType a

        vkColorArrayByteOffset :: a -> Int

        readVkColorArray :: Ptr a -> Int -> IO (VkColorArrayMType a)

        writeVkColorArray :: Ptr a -> Int -> VkColorArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'color'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkColorArray a

class HasVkColorAttachment a where
        type VkColorAttachmentMType a :: *

        vkColorAttachment :: a -> VkColorAttachmentMType a

        vkColorAttachmentByteOffset :: a -> Int

        readVkColorAttachment :: Ptr a -> IO (VkColorAttachmentMType a)

        writeVkColorAttachment ::
                               Ptr a -> VkColorAttachmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'colorAttachment'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkColorAttachment a

class HasVkColorAttachmentCount a where
        type VkColorAttachmentCountMType a :: *

        vkColorAttachmentCount :: a -> VkColorAttachmentCountMType a

        vkColorAttachmentCountByteOffset :: a -> Int

        readVkColorAttachmentCount ::
                                   Ptr a -> IO (VkColorAttachmentCountMType a)

        writeVkColorAttachmentCount ::
                                    Ptr a -> VkColorAttachmentCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'colorAttachmentCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkColorAttachmentCount a

class HasVkColorBlendOp a where
        type VkColorBlendOpMType a :: *

        vkColorBlendOp :: a -> VkColorBlendOpMType a

        vkColorBlendOpByteOffset :: a -> Int

        readVkColorBlendOp :: Ptr a -> IO (VkColorBlendOpMType a)

        writeVkColorBlendOp :: Ptr a -> VkColorBlendOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'colorBlendOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkColorBlendOp a

class HasVkColorSpace a where
        type VkColorSpaceMType a :: *

        vkColorSpace :: a -> VkColorSpaceMType a

        vkColorSpaceByteOffset :: a -> Int

        readVkColorSpace :: Ptr a -> IO (VkColorSpaceMType a)

        writeVkColorSpace :: Ptr a -> VkColorSpaceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'colorSpace'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkColorSpace a

class HasVkColorWriteMask a where
        type VkColorWriteMaskMType a :: *

        vkColorWriteMask :: a -> VkColorWriteMaskMType a

        vkColorWriteMaskByteOffset :: a -> Int

        readVkColorWriteMask :: Ptr a -> IO (VkColorWriteMaskMType a)

        writeVkColorWriteMask :: Ptr a -> VkColorWriteMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'colorWriteMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkColorWriteMask a

class HasVkCombinedImageSamplerDescriptorCount a where
        type VkCombinedImageSamplerDescriptorCountMType a :: *

        vkCombinedImageSamplerDescriptorCount ::
                                              a -> VkCombinedImageSamplerDescriptorCountMType a

        vkCombinedImageSamplerDescriptorCountByteOffset :: a -> Int

        readVkCombinedImageSamplerDescriptorCount ::
                                                  Ptr a ->
                                                    IO
                                                      (VkCombinedImageSamplerDescriptorCountMType a)

        writeVkCombinedImageSamplerDescriptorCount ::
                                                   Ptr a ->
                                                     VkCombinedImageSamplerDescriptorCountMType a ->
                                                       IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'combinedImageSamplerDescriptorCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCombinedImageSamplerDescriptorCount a

class HasVkCommandBufferCount a where
        type VkCommandBufferCountMType a :: *

        vkCommandBufferCount :: a -> VkCommandBufferCountMType a

        vkCommandBufferCountByteOffset :: a -> Int

        readVkCommandBufferCount ::
                                 Ptr a -> IO (VkCommandBufferCountMType a)

        writeVkCommandBufferCount ::
                                  Ptr a -> VkCommandBufferCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'commandBufferCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCommandBufferCount a

class HasVkCommandPool a where
        type VkCommandPoolMType a :: *

        vkCommandPool :: a -> VkCommandPoolMType a

        vkCommandPoolByteOffset :: a -> Int

        readVkCommandPool :: Ptr a -> IO (VkCommandPoolMType a)

        writeVkCommandPool :: Ptr a -> VkCommandPoolMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'commandPool'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCommandPool a

class HasVkCompareEnable a where
        type VkCompareEnableMType a :: *

        vkCompareEnable :: a -> VkCompareEnableMType a

        vkCompareEnableByteOffset :: a -> Int

        readVkCompareEnable :: Ptr a -> IO (VkCompareEnableMType a)

        writeVkCompareEnable :: Ptr a -> VkCompareEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'compareEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCompareEnable a

class HasVkCompareMask a where
        type VkCompareMaskMType a :: *

        vkCompareMask :: a -> VkCompareMaskMType a

        vkCompareMaskByteOffset :: a -> Int

        readVkCompareMask :: Ptr a -> IO (VkCompareMaskMType a)

        writeVkCompareMask :: Ptr a -> VkCompareMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'compareMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCompareMask a

class HasVkCompareOp a where
        type VkCompareOpMType a :: *

        vkCompareOp :: a -> VkCompareOpMType a

        vkCompareOpByteOffset :: a -> Int

        readVkCompareOp :: Ptr a -> IO (VkCompareOpMType a)

        writeVkCompareOp :: Ptr a -> VkCompareOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'compareOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCompareOp a

class HasVkCompatibleHandleTypes a where
        type VkCompatibleHandleTypesMType a :: *

        vkCompatibleHandleTypes :: a -> VkCompatibleHandleTypesMType a

        vkCompatibleHandleTypesByteOffset :: a -> Int

        readVkCompatibleHandleTypes ::
                                    Ptr a -> IO (VkCompatibleHandleTypesMType a)

        writeVkCompatibleHandleTypes ::
                                     Ptr a -> VkCompatibleHandleTypesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'compatibleHandleTypes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCompatibleHandleTypes a

class HasVkComponents a where
        type VkComponentsMType a :: *

        vkComponents :: a -> VkComponentsMType a

        vkComponentsByteOffset :: a -> Int

        readVkComponents :: Ptr a -> IO (VkComponentsMType a)

        writeVkComponents :: Ptr a -> VkComponentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'components'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkComponents a

class HasVkCompositeAlpha a where
        type VkCompositeAlphaMType a :: *

        vkCompositeAlpha :: a -> VkCompositeAlphaMType a

        vkCompositeAlphaByteOffset :: a -> Int

        readVkCompositeAlpha :: Ptr a -> IO (VkCompositeAlphaMType a)

        writeVkCompositeAlpha :: Ptr a -> VkCompositeAlphaMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'compositeAlpha'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCompositeAlpha a

class HasVkComputeBindingPointSupport a where
        type VkComputeBindingPointSupportMType a :: *

        vkComputeBindingPointSupport ::
                                     a -> VkComputeBindingPointSupportMType a

        vkComputeBindingPointSupportByteOffset :: a -> Int

        readVkComputeBindingPointSupport ::
                                         Ptr a -> IO (VkComputeBindingPointSupportMType a)

        writeVkComputeBindingPointSupport ::
                                          Ptr a -> VkComputeBindingPointSupportMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'computeBindingPointSupport'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkComputeBindingPointSupport a

class HasVkComputeWorkGroupSizeArray a where
        type VkComputeWorkGroupSizeArrayMType a :: *

        vkComputeWorkGroupSizeArray ::
                                    a -> Int -> VkComputeWorkGroupSizeArrayMType a

        vkComputeWorkGroupSizeArrayByteOffset :: a -> Int

        readVkComputeWorkGroupSizeArray ::
                                        Ptr a -> Int -> IO (VkComputeWorkGroupSizeArrayMType a)

        writeVkComputeWorkGroupSizeArray ::
                                         Ptr a -> Int -> VkComputeWorkGroupSizeArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'computeWorkGroupSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkComputeWorkGroupSizeArray a

class HasVkConnection a where
        type VkConnectionMType a :: *

        vkConnection :: a -> VkConnectionMType a

        vkConnectionByteOffset :: a -> Int

        readVkConnection :: Ptr a -> IO (VkConnectionMType a)

        writeVkConnection :: Ptr a -> VkConnectionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'connection'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkConnection a

class HasVkConservativePointAndLineRasterization a where
        type VkConservativePointAndLineRasterizationMType a :: *

        vkConservativePointAndLineRasterization ::
                                                a -> VkConservativePointAndLineRasterizationMType a

        vkConservativePointAndLineRasterizationByteOffset :: a -> Int

        readVkConservativePointAndLineRasterization ::
                                                    Ptr a ->
                                                      IO
                                                        (VkConservativePointAndLineRasterizationMType
                                                           a)

        writeVkConservativePointAndLineRasterization ::
                                                     Ptr a ->
                                                       VkConservativePointAndLineRasterizationMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'conservativePointAndLineRasterization'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkConservativePointAndLineRasterization a

class HasVkConservativeRasterizationMode a where
        type VkConservativeRasterizationModeMType a :: *

        vkConservativeRasterizationMode ::
                                        a -> VkConservativeRasterizationModeMType a

        vkConservativeRasterizationModeByteOffset :: a -> Int

        readVkConservativeRasterizationMode ::
                                            Ptr a -> IO (VkConservativeRasterizationModeMType a)

        writeVkConservativeRasterizationMode ::
                                             Ptr a ->
                                               VkConservativeRasterizationModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'conservativeRasterizationMode'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkConservativeRasterizationMode a

class HasVkConservativeRasterizationPostDepthCoverage a where
        type VkConservativeRasterizationPostDepthCoverageMType a :: *

        vkConservativeRasterizationPostDepthCoverage ::
                                                     a ->
                                                       VkConservativeRasterizationPostDepthCoverageMType
                                                         a

        vkConservativeRasterizationPostDepthCoverageByteOffset :: a -> Int

        readVkConservativeRasterizationPostDepthCoverage ::
                                                         Ptr a ->
                                                           IO
                                                             (VkConservativeRasterizationPostDepthCoverageMType
                                                                a)

        writeVkConservativeRasterizationPostDepthCoverage ::
                                                          Ptr a ->
                                                            VkConservativeRasterizationPostDepthCoverageMType
                                                              a
                                                              -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'conservativeRasterizationPostDepthCoverage'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkConservativeRasterizationPostDepthCoverage a

class HasVkConstantID a where
        type VkConstantIDMType a :: *

        vkConstantID :: a -> VkConstantIDMType a

        vkConstantIDByteOffset :: a -> Int

        readVkConstantID :: Ptr a -> IO (VkConstantIDMType a)

        writeVkConstantID :: Ptr a -> VkConstantIDMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'constantID'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkConstantID a

class HasVkConversion a where
        type VkConversionMType a :: *

        vkConversion :: a -> VkConversionMType a

        vkConversionByteOffset :: a -> Int

        readVkConversion :: Ptr a -> IO (VkConversionMType a)

        writeVkConversion :: Ptr a -> VkConversionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'conversion'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkConversion a

class HasVkCorrelationMaskCount a where
        type VkCorrelationMaskCountMType a :: *

        vkCorrelationMaskCount :: a -> VkCorrelationMaskCountMType a

        vkCorrelationMaskCountByteOffset :: a -> Int

        readVkCorrelationMaskCount ::
                                   Ptr a -> IO (VkCorrelationMaskCountMType a)

        writeVkCorrelationMaskCount ::
                                    Ptr a -> VkCorrelationMaskCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'correlationMaskCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCorrelationMaskCount a

class HasVkCoverageModulationMode a where
        type VkCoverageModulationModeMType a :: *

        vkCoverageModulationMode :: a -> VkCoverageModulationModeMType a

        vkCoverageModulationModeByteOffset :: a -> Int

        readVkCoverageModulationMode ::
                                     Ptr a -> IO (VkCoverageModulationModeMType a)

        writeVkCoverageModulationMode ::
                                      Ptr a -> VkCoverageModulationModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'coverageModulationMode'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCoverageModulationMode a

class HasVkCoverageModulationTableCount a where
        type VkCoverageModulationTableCountMType a :: *

        vkCoverageModulationTableCount ::
                                       a -> VkCoverageModulationTableCountMType a

        vkCoverageModulationTableCountByteOffset :: a -> Int

        readVkCoverageModulationTableCount ::
                                           Ptr a -> IO (VkCoverageModulationTableCountMType a)

        writeVkCoverageModulationTableCount ::
                                            Ptr a -> VkCoverageModulationTableCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'coverageModulationTableCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCoverageModulationTableCount a

class HasVkCoverageModulationTableEnable a where
        type VkCoverageModulationTableEnableMType a :: *

        vkCoverageModulationTableEnable ::
                                        a -> VkCoverageModulationTableEnableMType a

        vkCoverageModulationTableEnableByteOffset :: a -> Int

        readVkCoverageModulationTableEnable ::
                                            Ptr a -> IO (VkCoverageModulationTableEnableMType a)

        writeVkCoverageModulationTableEnable ::
                                             Ptr a ->
                                               VkCoverageModulationTableEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'coverageModulationTableEnable'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCoverageModulationTableEnable a

class HasVkCoverageToColorEnable a where
        type VkCoverageToColorEnableMType a :: *

        vkCoverageToColorEnable :: a -> VkCoverageToColorEnableMType a

        vkCoverageToColorEnableByteOffset :: a -> Int

        readVkCoverageToColorEnable ::
                                    Ptr a -> IO (VkCoverageToColorEnableMType a)

        writeVkCoverageToColorEnable ::
                                     Ptr a -> VkCoverageToColorEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'coverageToColorEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCoverageToColorEnable a

class HasVkCoverageToColorLocation a where
        type VkCoverageToColorLocationMType a :: *

        vkCoverageToColorLocation :: a -> VkCoverageToColorLocationMType a

        vkCoverageToColorLocationByteOffset :: a -> Int

        readVkCoverageToColorLocation ::
                                      Ptr a -> IO (VkCoverageToColorLocationMType a)

        writeVkCoverageToColorLocation ::
                                       Ptr a -> VkCoverageToColorLocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'coverageToColorLocation'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCoverageToColorLocation a

class HasVkCullMode a where
        type VkCullModeMType a :: *

        vkCullMode :: a -> VkCullModeMType a

        vkCullModeByteOffset :: a -> Int

        readVkCullMode :: Ptr a -> IO (VkCullModeMType a)

        writeVkCullMode :: Ptr a -> VkCullModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'cullMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCullMode a

class HasVkCurrentDisplay a where
        type VkCurrentDisplayMType a :: *

        vkCurrentDisplay :: a -> VkCurrentDisplayMType a

        vkCurrentDisplayByteOffset :: a -> Int

        readVkCurrentDisplay :: Ptr a -> IO (VkCurrentDisplayMType a)

        writeVkCurrentDisplay :: Ptr a -> VkCurrentDisplayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'currentDisplay'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCurrentDisplay a

class HasVkCurrentExtent a where
        type VkCurrentExtentMType a :: *

        vkCurrentExtent :: a -> VkCurrentExtentMType a

        vkCurrentExtentByteOffset :: a -> Int

        readVkCurrentExtent :: Ptr a -> IO (VkCurrentExtentMType a)

        writeVkCurrentExtent :: Ptr a -> VkCurrentExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'currentExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCurrentExtent a

class HasVkCurrentStackIndex a where
        type VkCurrentStackIndexMType a :: *

        vkCurrentStackIndex :: a -> VkCurrentStackIndexMType a

        vkCurrentStackIndexByteOffset :: a -> Int

        readVkCurrentStackIndex :: Ptr a -> IO (VkCurrentStackIndexMType a)

        writeVkCurrentStackIndex ::
                                 Ptr a -> VkCurrentStackIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'currentStackIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCurrentStackIndex a

class HasVkCurrentTransform a where
        type VkCurrentTransformMType a :: *

        vkCurrentTransform :: a -> VkCurrentTransformMType a

        vkCurrentTransformByteOffset :: a -> Int

        readVkCurrentTransform :: Ptr a -> IO (VkCurrentTransformMType a)

        writeVkCurrentTransform ::
                                Ptr a -> VkCurrentTransformMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'currentTransform'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkCurrentTransform a

class HasVkDataSize a where
        type VkDataSizeMType a :: *

        vkDataSize :: a -> VkDataSizeMType a

        vkDataSizeByteOffset :: a -> Int

        readVkDataSize :: Ptr a -> IO (VkDataSizeMType a)

        writeVkDataSize :: Ptr a -> VkDataSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dataSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDataSize a

class HasVkDedicatedAllocation a where
        type VkDedicatedAllocationMType a :: *

        vkDedicatedAllocation :: a -> VkDedicatedAllocationMType a

        vkDedicatedAllocationByteOffset :: a -> Int

        readVkDedicatedAllocation ::
                                  Ptr a -> IO (VkDedicatedAllocationMType a)

        writeVkDedicatedAllocation ::
                                   Ptr a -> VkDedicatedAllocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dedicatedAllocation'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDedicatedAllocation a

class HasVkDegenerateLinesRasterized a where
        type VkDegenerateLinesRasterizedMType a :: *

        vkDegenerateLinesRasterized ::
                                    a -> VkDegenerateLinesRasterizedMType a

        vkDegenerateLinesRasterizedByteOffset :: a -> Int

        readVkDegenerateLinesRasterized ::
                                        Ptr a -> IO (VkDegenerateLinesRasterizedMType a)

        writeVkDegenerateLinesRasterized ::
                                         Ptr a -> VkDegenerateLinesRasterizedMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'degenerateLinesRasterized'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDegenerateLinesRasterized a

class HasVkDegenerateTrianglesRasterized a where
        type VkDegenerateTrianglesRasterizedMType a :: *

        vkDegenerateTrianglesRasterized ::
                                        a -> VkDegenerateTrianglesRasterizedMType a

        vkDegenerateTrianglesRasterizedByteOffset :: a -> Int

        readVkDegenerateTrianglesRasterized ::
                                            Ptr a -> IO (VkDegenerateTrianglesRasterizedMType a)

        writeVkDegenerateTrianglesRasterized ::
                                             Ptr a ->
                                               VkDegenerateTrianglesRasterizedMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'degenerateTrianglesRasterized'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDegenerateTrianglesRasterized a

class HasVkDependencyCount a where
        type VkDependencyCountMType a :: *

        vkDependencyCount :: a -> VkDependencyCountMType a

        vkDependencyCountByteOffset :: a -> Int

        readVkDependencyCount :: Ptr a -> IO (VkDependencyCountMType a)

        writeVkDependencyCount ::
                               Ptr a -> VkDependencyCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dependencyCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDependencyCount a

class HasVkDependencyFlags a where
        type VkDependencyFlagsMType a :: *

        vkDependencyFlags :: a -> VkDependencyFlagsMType a

        vkDependencyFlagsByteOffset :: a -> Int

        readVkDependencyFlags :: Ptr a -> IO (VkDependencyFlagsMType a)

        writeVkDependencyFlags ::
                               Ptr a -> VkDependencyFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dependencyFlags'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDependencyFlags a

class HasVkDepth a where
        type VkDepthMType a :: *

        vkDepth :: a -> VkDepthMType a

        vkDepthByteOffset :: a -> Int

        readVkDepth :: Ptr a -> IO (VkDepthMType a)

        writeVkDepth :: Ptr a -> VkDepthMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depth'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepth a

class HasVkDepthBiasClamp a where
        type VkDepthBiasClampMType a :: *

        vkDepthBiasClamp :: a -> VkDepthBiasClampMType a

        vkDepthBiasClampByteOffset :: a -> Int

        readVkDepthBiasClamp :: Ptr a -> IO (VkDepthBiasClampMType a)

        writeVkDepthBiasClamp :: Ptr a -> VkDepthBiasClampMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthBiasClamp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthBiasClamp a

class HasVkDepthBiasConstantFactor a where
        type VkDepthBiasConstantFactorMType a :: *

        vkDepthBiasConstantFactor :: a -> VkDepthBiasConstantFactorMType a

        vkDepthBiasConstantFactorByteOffset :: a -> Int

        readVkDepthBiasConstantFactor ::
                                      Ptr a -> IO (VkDepthBiasConstantFactorMType a)

        writeVkDepthBiasConstantFactor ::
                                       Ptr a -> VkDepthBiasConstantFactorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthBiasConstantFactor'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthBiasConstantFactor a

class HasVkDepthBiasEnable a where
        type VkDepthBiasEnableMType a :: *

        vkDepthBiasEnable :: a -> VkDepthBiasEnableMType a

        vkDepthBiasEnableByteOffset :: a -> Int

        readVkDepthBiasEnable :: Ptr a -> IO (VkDepthBiasEnableMType a)

        writeVkDepthBiasEnable ::
                               Ptr a -> VkDepthBiasEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthBiasEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthBiasEnable a

class HasVkDepthBiasSlopeFactor a where
        type VkDepthBiasSlopeFactorMType a :: *

        vkDepthBiasSlopeFactor :: a -> VkDepthBiasSlopeFactorMType a

        vkDepthBiasSlopeFactorByteOffset :: a -> Int

        readVkDepthBiasSlopeFactor ::
                                   Ptr a -> IO (VkDepthBiasSlopeFactorMType a)

        writeVkDepthBiasSlopeFactor ::
                                    Ptr a -> VkDepthBiasSlopeFactorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthBiasSlopeFactor'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthBiasSlopeFactor a

class HasVkDepthBounds a where
        type VkDepthBoundsMType a :: *

        vkDepthBounds :: a -> VkDepthBoundsMType a

        vkDepthBoundsByteOffset :: a -> Int

        readVkDepthBounds :: Ptr a -> IO (VkDepthBoundsMType a)

        writeVkDepthBounds :: Ptr a -> VkDepthBoundsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthBounds'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthBounds a

class HasVkDepthBoundsTestEnable a where
        type VkDepthBoundsTestEnableMType a :: *

        vkDepthBoundsTestEnable :: a -> VkDepthBoundsTestEnableMType a

        vkDepthBoundsTestEnableByteOffset :: a -> Int

        readVkDepthBoundsTestEnable ::
                                    Ptr a -> IO (VkDepthBoundsTestEnableMType a)

        writeVkDepthBoundsTestEnable ::
                                     Ptr a -> VkDepthBoundsTestEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthBoundsTestEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthBoundsTestEnable a

class HasVkDepthClamp a where
        type VkDepthClampMType a :: *

        vkDepthClamp :: a -> VkDepthClampMType a

        vkDepthClampByteOffset :: a -> Int

        readVkDepthClamp :: Ptr a -> IO (VkDepthClampMType a)

        writeVkDepthClamp :: Ptr a -> VkDepthClampMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthClamp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthClamp a

class HasVkDepthClampEnable a where
        type VkDepthClampEnableMType a :: *

        vkDepthClampEnable :: a -> VkDepthClampEnableMType a

        vkDepthClampEnableByteOffset :: a -> Int

        readVkDepthClampEnable :: Ptr a -> IO (VkDepthClampEnableMType a)

        writeVkDepthClampEnable ::
                                Ptr a -> VkDepthClampEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthClampEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthClampEnable a

class HasVkDepthCompareOp a where
        type VkDepthCompareOpMType a :: *

        vkDepthCompareOp :: a -> VkDepthCompareOpMType a

        vkDepthCompareOpByteOffset :: a -> Int

        readVkDepthCompareOp :: Ptr a -> IO (VkDepthCompareOpMType a)

        writeVkDepthCompareOp :: Ptr a -> VkDepthCompareOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthCompareOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthCompareOp a

class HasVkDepthFailOp a where
        type VkDepthFailOpMType a :: *

        vkDepthFailOp :: a -> VkDepthFailOpMType a

        vkDepthFailOpByteOffset :: a -> Int

        readVkDepthFailOp :: Ptr a -> IO (VkDepthFailOpMType a)

        writeVkDepthFailOp :: Ptr a -> VkDepthFailOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthFailOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthFailOp a

class HasVkDepthPitch a where
        type VkDepthPitchMType a :: *

        vkDepthPitch :: a -> VkDepthPitchMType a

        vkDepthPitchByteOffset :: a -> Int

        readVkDepthPitch :: Ptr a -> IO (VkDepthPitchMType a)

        writeVkDepthPitch :: Ptr a -> VkDepthPitchMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthPitch'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthPitch a

class HasVkDepthStencil a where
        type VkDepthStencilMType a :: *

        vkDepthStencil :: a -> VkDepthStencilMType a

        vkDepthStencilByteOffset :: a -> Int

        readVkDepthStencil :: Ptr a -> IO (VkDepthStencilMType a)

        writeVkDepthStencil :: Ptr a -> VkDepthStencilMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthStencil'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthStencil a

class HasVkDepthTestEnable a where
        type VkDepthTestEnableMType a :: *

        vkDepthTestEnable :: a -> VkDepthTestEnableMType a

        vkDepthTestEnableByteOffset :: a -> Int

        readVkDepthTestEnable :: Ptr a -> IO (VkDepthTestEnableMType a)

        writeVkDepthTestEnable ::
                               Ptr a -> VkDepthTestEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthTestEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthTestEnable a

class HasVkDepthWriteEnable a where
        type VkDepthWriteEnableMType a :: *

        vkDepthWriteEnable :: a -> VkDepthWriteEnableMType a

        vkDepthWriteEnableByteOffset :: a -> Int

        readVkDepthWriteEnable :: Ptr a -> IO (VkDepthWriteEnableMType a)

        writeVkDepthWriteEnable ::
                                Ptr a -> VkDepthWriteEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'depthWriteEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDepthWriteEnable a

class HasVkDescriptionArray a where
        type VkDescriptionArrayMType a :: *

        vkDescriptionArray :: a -> Int -> VkDescriptionArrayMType a

        vkDescriptionArrayByteOffset :: a -> Int

        readVkDescriptionArray ::
                               Ptr a -> Int -> IO (VkDescriptionArrayMType a)

        writeVkDescriptionArray ::
                                Ptr a -> Int -> VkDescriptionArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'description'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDescriptionArray a

class HasVkDescriptorCount a where
        type VkDescriptorCountMType a :: *

        vkDescriptorCount :: a -> VkDescriptorCountMType a

        vkDescriptorCountByteOffset :: a -> Int

        readVkDescriptorCount :: Ptr a -> IO (VkDescriptorCountMType a)

        writeVkDescriptorCount ::
                               Ptr a -> VkDescriptorCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'descriptorCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDescriptorCount a

class HasVkDescriptorPool a where
        type VkDescriptorPoolMType a :: *

        vkDescriptorPool :: a -> VkDescriptorPoolMType a

        vkDescriptorPoolByteOffset :: a -> Int

        readVkDescriptorPool :: Ptr a -> IO (VkDescriptorPoolMType a)

        writeVkDescriptorPool :: Ptr a -> VkDescriptorPoolMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'descriptorPool'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDescriptorPool a

class HasVkDescriptorSet a where
        type VkDescriptorSetMType a :: *

        vkDescriptorSet :: a -> VkDescriptorSetMType a

        vkDescriptorSetByteOffset :: a -> Int

        readVkDescriptorSet :: Ptr a -> IO (VkDescriptorSetMType a)

        writeVkDescriptorSet :: Ptr a -> VkDescriptorSetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'descriptorSet'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDescriptorSet a

class HasVkDescriptorSetCount a where
        type VkDescriptorSetCountMType a :: *

        vkDescriptorSetCount :: a -> VkDescriptorSetCountMType a

        vkDescriptorSetCountByteOffset :: a -> Int

        readVkDescriptorSetCount ::
                                 Ptr a -> IO (VkDescriptorSetCountMType a)

        writeVkDescriptorSetCount ::
                                  Ptr a -> VkDescriptorSetCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'descriptorSetCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDescriptorSetCount a

class HasVkDescriptorSetLayout a where
        type VkDescriptorSetLayoutMType a :: *

        vkDescriptorSetLayout :: a -> VkDescriptorSetLayoutMType a

        vkDescriptorSetLayoutByteOffset :: a -> Int

        readVkDescriptorSetLayout ::
                                  Ptr a -> IO (VkDescriptorSetLayoutMType a)

        writeVkDescriptorSetLayout ::
                                   Ptr a -> VkDescriptorSetLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'descriptorSetLayout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDescriptorSetLayout a

class HasVkDescriptorType a where
        type VkDescriptorTypeMType a :: *

        vkDescriptorType :: a -> VkDescriptorTypeMType a

        vkDescriptorTypeByteOffset :: a -> Int

        readVkDescriptorType :: Ptr a -> IO (VkDescriptorTypeMType a)

        writeVkDescriptorType :: Ptr a -> VkDescriptorTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'descriptorType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDescriptorType a

class HasVkDescriptorUpdateEntryCount a where
        type VkDescriptorUpdateEntryCountMType a :: *

        vkDescriptorUpdateEntryCount ::
                                     a -> VkDescriptorUpdateEntryCountMType a

        vkDescriptorUpdateEntryCountByteOffset :: a -> Int

        readVkDescriptorUpdateEntryCount ::
                                         Ptr a -> IO (VkDescriptorUpdateEntryCountMType a)

        writeVkDescriptorUpdateEntryCount ::
                                          Ptr a -> VkDescriptorUpdateEntryCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'descriptorUpdateEntryCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDescriptorUpdateEntryCount a

class HasVkDesiredPresentTime a where
        type VkDesiredPresentTimeMType a :: *

        vkDesiredPresentTime :: a -> VkDesiredPresentTimeMType a

        vkDesiredPresentTimeByteOffset :: a -> Int

        readVkDesiredPresentTime ::
                                 Ptr a -> IO (VkDesiredPresentTimeMType a)

        writeVkDesiredPresentTime ::
                                  Ptr a -> VkDesiredPresentTimeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'desiredPresentTime'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDesiredPresentTime a

class HasVkDeviceEvent a where
        type VkDeviceEventMType a :: *

        vkDeviceEvent :: a -> VkDeviceEventMType a

        vkDeviceEventByteOffset :: a -> Int

        readVkDeviceEvent :: Ptr a -> IO (VkDeviceEventMType a)

        writeVkDeviceEvent :: Ptr a -> VkDeviceEventMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceEvent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceEvent a

class HasVkDeviceID a where
        type VkDeviceIDMType a :: *

        vkDeviceID :: a -> VkDeviceIDMType a

        vkDeviceIDByteOffset :: a -> Int

        readVkDeviceID :: Ptr a -> IO (VkDeviceIDMType a)

        writeVkDeviceID :: Ptr a -> VkDeviceIDMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceID'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceID a

class HasVkDeviceIndexCount a where
        type VkDeviceIndexCountMType a :: *

        vkDeviceIndexCount :: a -> VkDeviceIndexCountMType a

        vkDeviceIndexCountByteOffset :: a -> Int

        readVkDeviceIndexCount :: Ptr a -> IO (VkDeviceIndexCountMType a)

        writeVkDeviceIndexCount ::
                                Ptr a -> VkDeviceIndexCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceIndexCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceIndexCount a

class HasVkDeviceLUIDArray a where
        type VkDeviceLUIDArrayMType a :: *

        vkDeviceLUIDArray :: a -> Int -> VkDeviceLUIDArrayMType a

        vkDeviceLUIDArrayByteOffset :: a -> Int

        readVkDeviceLUIDArray ::
                              Ptr a -> Int -> IO (VkDeviceLUIDArrayMType a)

        writeVkDeviceLUIDArray ::
                               Ptr a -> Int -> VkDeviceLUIDArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceLUID'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceLUIDArray a

class HasVkDeviceLUIDValid a where
        type VkDeviceLUIDValidMType a :: *

        vkDeviceLUIDValid :: a -> VkDeviceLUIDValidMType a

        vkDeviceLUIDValidByteOffset :: a -> Int

        readVkDeviceLUIDValid :: Ptr a -> IO (VkDeviceLUIDValidMType a)

        writeVkDeviceLUIDValid ::
                               Ptr a -> VkDeviceLUIDValidMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceLUIDValid'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceLUIDValid a

class HasVkDeviceMask a where
        type VkDeviceMaskMType a :: *

        vkDeviceMask :: a -> VkDeviceMaskMType a

        vkDeviceMaskByteOffset :: a -> Int

        readVkDeviceMask :: Ptr a -> IO (VkDeviceMaskMType a)

        writeVkDeviceMask :: Ptr a -> VkDeviceMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceMask a

class HasVkDeviceNameArray a where
        type VkDeviceNameArrayMType a :: *

        vkDeviceNameArray :: a -> Int -> VkDeviceNameArrayMType a

        vkDeviceNameArrayByteOffset :: a -> Int

        readVkDeviceNameArray ::
                              Ptr a -> Int -> IO (VkDeviceNameArrayMType a)

        writeVkDeviceNameArray ::
                               Ptr a -> Int -> VkDeviceNameArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceNameArray a

class HasVkDeviceNodeMask a where
        type VkDeviceNodeMaskMType a :: *

        vkDeviceNodeMask :: a -> VkDeviceNodeMaskMType a

        vkDeviceNodeMaskByteOffset :: a -> Int

        readVkDeviceNodeMask :: Ptr a -> IO (VkDeviceNodeMaskMType a)

        writeVkDeviceNodeMask :: Ptr a -> VkDeviceNodeMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceNodeMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceNodeMask a

class HasVkDeviceRenderAreaCount a where
        type VkDeviceRenderAreaCountMType a :: *

        vkDeviceRenderAreaCount :: a -> VkDeviceRenderAreaCountMType a

        vkDeviceRenderAreaCountByteOffset :: a -> Int

        readVkDeviceRenderAreaCount ::
                                    Ptr a -> IO (VkDeviceRenderAreaCountMType a)

        writeVkDeviceRenderAreaCount ::
                                     Ptr a -> VkDeviceRenderAreaCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceRenderAreaCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceRenderAreaCount a

class HasVkDeviceType a where
        type VkDeviceTypeMType a :: *

        vkDeviceType :: a -> VkDeviceTypeMType a

        vkDeviceTypeByteOffset :: a -> Int

        readVkDeviceType :: Ptr a -> IO (VkDeviceTypeMType a)

        writeVkDeviceType :: Ptr a -> VkDeviceTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceType a

class HasVkDeviceUUIDArray a where
        type VkDeviceUUIDArrayMType a :: *

        vkDeviceUUIDArray :: a -> Int -> VkDeviceUUIDArrayMType a

        vkDeviceUUIDArrayByteOffset :: a -> Int

        readVkDeviceUUIDArray ::
                              Ptr a -> Int -> IO (VkDeviceUUIDArrayMType a)

        writeVkDeviceUUIDArray ::
                               Ptr a -> Int -> VkDeviceUUIDArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'deviceUUID'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDeviceUUIDArray a

class HasVkDisabledValidationCheckCount a where
        type VkDisabledValidationCheckCountMType a :: *

        vkDisabledValidationCheckCount ::
                                       a -> VkDisabledValidationCheckCountMType a

        vkDisabledValidationCheckCountByteOffset :: a -> Int

        readVkDisabledValidationCheckCount ::
                                           Ptr a -> IO (VkDisabledValidationCheckCountMType a)

        writeVkDisabledValidationCheckCount ::
                                            Ptr a -> VkDisabledValidationCheckCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'disabledValidationCheckCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDisabledValidationCheckCount a

class HasVkDiscardRectangleCount a where
        type VkDiscardRectangleCountMType a :: *

        vkDiscardRectangleCount :: a -> VkDiscardRectangleCountMType a

        vkDiscardRectangleCountByteOffset :: a -> Int

        readVkDiscardRectangleCount ::
                                    Ptr a -> IO (VkDiscardRectangleCountMType a)

        writeVkDiscardRectangleCount ::
                                     Ptr a -> VkDiscardRectangleCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'discardRectangleCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDiscardRectangleCount a

class HasVkDiscardRectangleMode a where
        type VkDiscardRectangleModeMType a :: *

        vkDiscardRectangleMode :: a -> VkDiscardRectangleModeMType a

        vkDiscardRectangleModeByteOffset :: a -> Int

        readVkDiscardRectangleMode ::
                                   Ptr a -> IO (VkDiscardRectangleModeMType a)

        writeVkDiscardRectangleMode ::
                                    Ptr a -> VkDiscardRectangleModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'discardRectangleMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDiscardRectangleMode a

class HasVkDiscreteQueuePriorities a where
        type VkDiscreteQueuePrioritiesMType a :: *

        vkDiscreteQueuePriorities :: a -> VkDiscreteQueuePrioritiesMType a

        vkDiscreteQueuePrioritiesByteOffset :: a -> Int

        readVkDiscreteQueuePriorities ::
                                      Ptr a -> IO (VkDiscreteQueuePrioritiesMType a)

        writeVkDiscreteQueuePriorities ::
                                       Ptr a -> VkDiscreteQueuePrioritiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'discreteQueuePriorities'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDiscreteQueuePriorities a

class HasVkDisplay a where
        type VkDisplayMType a :: *

        vkDisplay :: a -> VkDisplayMType a

        vkDisplayByteOffset :: a -> Int

        readVkDisplay :: Ptr a -> IO (VkDisplayMType a)

        writeVkDisplay :: Ptr a -> VkDisplayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'display'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDisplay a

class HasVkDisplayEvent a where
        type VkDisplayEventMType a :: *

        vkDisplayEvent :: a -> VkDisplayEventMType a

        vkDisplayEventByteOffset :: a -> Int

        readVkDisplayEvent :: Ptr a -> IO (VkDisplayEventMType a)

        writeVkDisplayEvent :: Ptr a -> VkDisplayEventMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'displayEvent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDisplayEvent a

class HasVkDisplayMode a where
        type VkDisplayModeMType a :: *

        vkDisplayMode :: a -> VkDisplayModeMType a

        vkDisplayModeByteOffset :: a -> Int

        readVkDisplayMode :: Ptr a -> IO (VkDisplayModeMType a)

        writeVkDisplayMode :: Ptr a -> VkDisplayModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'displayMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDisplayMode a

class HasVkDisplayName a where
        type VkDisplayNameMType a :: *

        vkDisplayName :: a -> VkDisplayNameMType a

        vkDisplayNameByteOffset :: a -> Int

        readVkDisplayName :: Ptr a -> IO (VkDisplayNameMType a)

        writeVkDisplayName :: Ptr a -> VkDisplayNameMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'displayName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDisplayName a

class HasVkDisplayPrimaryBlue a where
        type VkDisplayPrimaryBlueMType a :: *

        vkDisplayPrimaryBlue :: a -> VkDisplayPrimaryBlueMType a

        vkDisplayPrimaryBlueByteOffset :: a -> Int

        readVkDisplayPrimaryBlue ::
                                 Ptr a -> IO (VkDisplayPrimaryBlueMType a)

        writeVkDisplayPrimaryBlue ::
                                  Ptr a -> VkDisplayPrimaryBlueMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'displayPrimaryBlue'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDisplayPrimaryBlue a

class HasVkDisplayPrimaryGreen a where
        type VkDisplayPrimaryGreenMType a :: *

        vkDisplayPrimaryGreen :: a -> VkDisplayPrimaryGreenMType a

        vkDisplayPrimaryGreenByteOffset :: a -> Int

        readVkDisplayPrimaryGreen ::
                                  Ptr a -> IO (VkDisplayPrimaryGreenMType a)

        writeVkDisplayPrimaryGreen ::
                                   Ptr a -> VkDisplayPrimaryGreenMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'displayPrimaryGreen'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDisplayPrimaryGreen a

class HasVkDisplayPrimaryRed a where
        type VkDisplayPrimaryRedMType a :: *

        vkDisplayPrimaryRed :: a -> VkDisplayPrimaryRedMType a

        vkDisplayPrimaryRedByteOffset :: a -> Int

        readVkDisplayPrimaryRed :: Ptr a -> IO (VkDisplayPrimaryRedMType a)

        writeVkDisplayPrimaryRed ::
                                 Ptr a -> VkDisplayPrimaryRedMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'displayPrimaryRed'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDisplayPrimaryRed a

class HasVkDivisor a where
        type VkDivisorMType a :: *

        vkDivisor :: a -> VkDivisorMType a

        vkDivisorByteOffset :: a -> Int

        readVkDivisor :: Ptr a -> IO (VkDivisorMType a)

        writeVkDivisor :: Ptr a -> VkDivisorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'divisor'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDivisor a

class HasVkDomainOrigin a where
        type VkDomainOriginMType a :: *

        vkDomainOrigin :: a -> VkDomainOriginMType a

        vkDomainOriginByteOffset :: a -> Int

        readVkDomainOrigin :: Ptr a -> IO (VkDomainOriginMType a)

        writeVkDomainOrigin :: Ptr a -> VkDomainOriginMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'domainOrigin'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDomainOrigin a

class HasVkDpy a where
        type VkDpyMType a :: *

        vkDpy :: a -> VkDpyMType a

        vkDpyByteOffset :: a -> Int

        readVkDpy :: Ptr a -> IO (VkDpyMType a)

        writeVkDpy :: Ptr a -> VkDpyMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dpy'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDpy a

class HasVkDrawIndirectFirstInstance a where
        type VkDrawIndirectFirstInstanceMType a :: *

        vkDrawIndirectFirstInstance ::
                                    a -> VkDrawIndirectFirstInstanceMType a

        vkDrawIndirectFirstInstanceByteOffset :: a -> Int

        readVkDrawIndirectFirstInstance ::
                                        Ptr a -> IO (VkDrawIndirectFirstInstanceMType a)

        writeVkDrawIndirectFirstInstance ::
                                         Ptr a -> VkDrawIndirectFirstInstanceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'drawIndirectFirstInstance'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDrawIndirectFirstInstance a

class HasVkDriverUUIDArray a where
        type VkDriverUUIDArrayMType a :: *

        vkDriverUUIDArray :: a -> Int -> VkDriverUUIDArrayMType a

        vkDriverUUIDArrayByteOffset :: a -> Int

        readVkDriverUUIDArray ::
                              Ptr a -> Int -> IO (VkDriverUUIDArrayMType a)

        writeVkDriverUUIDArray ::
                               Ptr a -> Int -> VkDriverUUIDArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'driverUUID'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDriverUUIDArray a

class HasVkDriverVersion a where
        type VkDriverVersionMType a :: *

        vkDriverVersion :: a -> VkDriverVersionMType a

        vkDriverVersionByteOffset :: a -> Int

        readVkDriverVersion :: Ptr a -> IO (VkDriverVersionMType a)

        writeVkDriverVersion :: Ptr a -> VkDriverVersionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'driverVersion'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDriverVersion a

class HasVkDstAccessMask a where
        type VkDstAccessMaskMType a :: *

        vkDstAccessMask :: a -> VkDstAccessMaskMType a

        vkDstAccessMaskByteOffset :: a -> Int

        readVkDstAccessMask :: Ptr a -> IO (VkDstAccessMaskMType a)

        writeVkDstAccessMask :: Ptr a -> VkDstAccessMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstAccessMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstAccessMask a

class HasVkDstAlphaBlendFactor a where
        type VkDstAlphaBlendFactorMType a :: *

        vkDstAlphaBlendFactor :: a -> VkDstAlphaBlendFactorMType a

        vkDstAlphaBlendFactorByteOffset :: a -> Int

        readVkDstAlphaBlendFactor ::
                                  Ptr a -> IO (VkDstAlphaBlendFactorMType a)

        writeVkDstAlphaBlendFactor ::
                                   Ptr a -> VkDstAlphaBlendFactorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstAlphaBlendFactor'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstAlphaBlendFactor a

class HasVkDstArrayElement a where
        type VkDstArrayElementMType a :: *

        vkDstArrayElement :: a -> VkDstArrayElementMType a

        vkDstArrayElementByteOffset :: a -> Int

        readVkDstArrayElement :: Ptr a -> IO (VkDstArrayElementMType a)

        writeVkDstArrayElement ::
                               Ptr a -> VkDstArrayElementMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstArrayElement'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstArrayElement a

class HasVkDstBinding a where
        type VkDstBindingMType a :: *

        vkDstBinding :: a -> VkDstBindingMType a

        vkDstBindingByteOffset :: a -> Int

        readVkDstBinding :: Ptr a -> IO (VkDstBindingMType a)

        writeVkDstBinding :: Ptr a -> VkDstBindingMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstBinding'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstBinding a

class HasVkDstColorBlendFactor a where
        type VkDstColorBlendFactorMType a :: *

        vkDstColorBlendFactor :: a -> VkDstColorBlendFactorMType a

        vkDstColorBlendFactorByteOffset :: a -> Int

        readVkDstColorBlendFactor ::
                                  Ptr a -> IO (VkDstColorBlendFactorMType a)

        writeVkDstColorBlendFactor ::
                                   Ptr a -> VkDstColorBlendFactorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstColorBlendFactor'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstColorBlendFactor a

class HasVkDstOffset a where
        type VkDstOffsetMType a :: *

        vkDstOffset :: a -> VkDstOffsetMType a

        vkDstOffsetByteOffset :: a -> Int

        readVkDstOffset :: Ptr a -> IO (VkDstOffsetMType a)

        writeVkDstOffset :: Ptr a -> VkDstOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstOffset a

class HasVkDstOffsetsArray a where
        type VkDstOffsetsArrayMType a :: *

        vkDstOffsetsArray :: a -> Int -> VkDstOffsetsArrayMType a

        vkDstOffsetsArrayByteOffset :: a -> Int

        readVkDstOffsetsArray ::
                              Ptr a -> Int -> IO (VkDstOffsetsArrayMType a)

        writeVkDstOffsetsArray ::
                               Ptr a -> Int -> VkDstOffsetsArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstOffsets'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstOffsetsArray a

class HasVkDstPremultiplied a where
        type VkDstPremultipliedMType a :: *

        vkDstPremultiplied :: a -> VkDstPremultipliedMType a

        vkDstPremultipliedByteOffset :: a -> Int

        readVkDstPremultiplied :: Ptr a -> IO (VkDstPremultipliedMType a)

        writeVkDstPremultiplied ::
                                Ptr a -> VkDstPremultipliedMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstPremultiplied'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstPremultiplied a

class HasVkDstQueueFamilyIndex a where
        type VkDstQueueFamilyIndexMType a :: *

        vkDstQueueFamilyIndex :: a -> VkDstQueueFamilyIndexMType a

        vkDstQueueFamilyIndexByteOffset :: a -> Int

        readVkDstQueueFamilyIndex ::
                                  Ptr a -> IO (VkDstQueueFamilyIndexMType a)

        writeVkDstQueueFamilyIndex ::
                                   Ptr a -> VkDstQueueFamilyIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstQueueFamilyIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstQueueFamilyIndex a

class HasVkDstRect a where
        type VkDstRectMType a :: *

        vkDstRect :: a -> VkDstRectMType a

        vkDstRectByteOffset :: a -> Int

        readVkDstRect :: Ptr a -> IO (VkDstRectMType a)

        writeVkDstRect :: Ptr a -> VkDstRectMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstRect'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstRect a

class HasVkDstSet a where
        type VkDstSetMType a :: *

        vkDstSet :: a -> VkDstSetMType a

        vkDstSetByteOffset :: a -> Int

        readVkDstSet :: Ptr a -> IO (VkDstSetMType a)

        writeVkDstSet :: Ptr a -> VkDstSetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstSet'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstSet a

class HasVkDstStageMask a where
        type VkDstStageMaskMType a :: *

        vkDstStageMask :: a -> VkDstStageMaskMType a

        vkDstStageMaskByteOffset :: a -> Int

        readVkDstStageMask :: Ptr a -> IO (VkDstStageMaskMType a)

        writeVkDstStageMask :: Ptr a -> VkDstStageMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstStageMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstStageMask a

class HasVkDstSubpass a where
        type VkDstSubpassMType a :: *

        vkDstSubpass :: a -> VkDstSubpassMType a

        vkDstSubpassByteOffset :: a -> Int

        readVkDstSubpass :: Ptr a -> IO (VkDstSubpassMType a)

        writeVkDstSubpass :: Ptr a -> VkDstSubpassMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstSubpass'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstSubpass a

class HasVkDstSubresource a where
        type VkDstSubresourceMType a :: *

        vkDstSubresource :: a -> VkDstSubresourceMType a

        vkDstSubresourceByteOffset :: a -> Int

        readVkDstSubresource :: Ptr a -> IO (VkDstSubresourceMType a)

        writeVkDstSubresource :: Ptr a -> VkDstSubresourceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dstSubresource'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDstSubresource a

class HasVkDualSrcBlend a where
        type VkDualSrcBlendMType a :: *

        vkDualSrcBlend :: a -> VkDualSrcBlendMType a

        vkDualSrcBlendByteOffset :: a -> Int

        readVkDualSrcBlend :: Ptr a -> IO (VkDualSrcBlendMType a)

        writeVkDualSrcBlend :: Ptr a -> VkDualSrcBlendMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dualSrcBlend'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDualSrcBlend a

class HasVkDwAccess a where
        type VkDwAccessMType a :: *

        vkDwAccess :: a -> VkDwAccessMType a

        vkDwAccessByteOffset :: a -> Int

        readVkDwAccess :: Ptr a -> IO (VkDwAccessMType a)

        writeVkDwAccess :: Ptr a -> VkDwAccessMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dwAccess'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDwAccess a

class HasVkDynamicCount a where
        type VkDynamicCountMType a :: *

        vkDynamicCount :: a -> VkDynamicCountMType a

        vkDynamicCountByteOffset :: a -> Int

        readVkDynamicCount :: Ptr a -> IO (VkDynamicCountMType a)

        writeVkDynamicCount :: Ptr a -> VkDynamicCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dynamicCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDynamicCount a

class HasVkDynamicStateCount a where
        type VkDynamicStateCountMType a :: *

        vkDynamicStateCount :: a -> VkDynamicStateCountMType a

        vkDynamicStateCountByteOffset :: a -> Int

        readVkDynamicStateCount :: Ptr a -> IO (VkDynamicStateCountMType a)

        writeVkDynamicStateCount ::
                                 Ptr a -> VkDynamicStateCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'dynamicStateCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkDynamicStateCount a

class HasVkEarliestPresentTime a where
        type VkEarliestPresentTimeMType a :: *

        vkEarliestPresentTime :: a -> VkEarliestPresentTimeMType a

        vkEarliestPresentTimeByteOffset :: a -> Int

        readVkEarliestPresentTime ::
                                  Ptr a -> IO (VkEarliestPresentTimeMType a)

        writeVkEarliestPresentTime ::
                                   Ptr a -> VkEarliestPresentTimeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'earliestPresentTime'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkEarliestPresentTime a

class HasVkEnabledExtensionCount a where
        type VkEnabledExtensionCountMType a :: *

        vkEnabledExtensionCount :: a -> VkEnabledExtensionCountMType a

        vkEnabledExtensionCountByteOffset :: a -> Int

        readVkEnabledExtensionCount ::
                                    Ptr a -> IO (VkEnabledExtensionCountMType a)

        writeVkEnabledExtensionCount ::
                                     Ptr a -> VkEnabledExtensionCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'enabledExtensionCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkEnabledExtensionCount a

class HasVkEnabledLayerCount a where
        type VkEnabledLayerCountMType a :: *

        vkEnabledLayerCount :: a -> VkEnabledLayerCountMType a

        vkEnabledLayerCountByteOffset :: a -> Int

        readVkEnabledLayerCount :: Ptr a -> IO (VkEnabledLayerCountMType a)

        writeVkEnabledLayerCount ::
                                 Ptr a -> VkEnabledLayerCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'enabledLayerCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkEnabledLayerCount a

class HasVkEngineVersion a where
        type VkEngineVersionMType a :: *

        vkEngineVersion :: a -> VkEngineVersionMType a

        vkEngineVersionByteOffset :: a -> Int

        readVkEngineVersion :: Ptr a -> IO (VkEngineVersionMType a)

        writeVkEngineVersion :: Ptr a -> VkEngineVersionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'engineVersion'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkEngineVersion a

class HasVkExportFromImportedHandleTypes a where
        type VkExportFromImportedHandleTypesMType a :: *

        vkExportFromImportedHandleTypes ::
                                        a -> VkExportFromImportedHandleTypesMType a

        vkExportFromImportedHandleTypesByteOffset :: a -> Int

        readVkExportFromImportedHandleTypes ::
                                            Ptr a -> IO (VkExportFromImportedHandleTypesMType a)

        writeVkExportFromImportedHandleTypes ::
                                             Ptr a ->
                                               VkExportFromImportedHandleTypesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'exportFromImportedHandleTypes'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExportFromImportedHandleTypes a

class HasVkExtensionNameArray a where
        type VkExtensionNameArrayMType a :: *

        vkExtensionNameArray :: a -> Int -> VkExtensionNameArrayMType a

        vkExtensionNameArrayByteOffset :: a -> Int

        readVkExtensionNameArray ::
                                 Ptr a -> Int -> IO (VkExtensionNameArrayMType a)

        writeVkExtensionNameArray ::
                                  Ptr a -> Int -> VkExtensionNameArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'extensionName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExtensionNameArray a

class HasVkExtent a where
        type VkExtentMType a :: *

        vkExtent :: a -> VkExtentMType a

        vkExtentByteOffset :: a -> Int

        readVkExtent :: Ptr a -> IO (VkExtentMType a)

        writeVkExtent :: Ptr a -> VkExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'extent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExtent a

class HasVkExternalFenceFeatures a where
        type VkExternalFenceFeaturesMType a :: *

        vkExternalFenceFeatures :: a -> VkExternalFenceFeaturesMType a

        vkExternalFenceFeaturesByteOffset :: a -> Int

        readVkExternalFenceFeatures ::
                                    Ptr a -> IO (VkExternalFenceFeaturesMType a)

        writeVkExternalFenceFeatures ::
                                     Ptr a -> VkExternalFenceFeaturesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'externalFenceFeatures'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExternalFenceFeatures a

class HasVkExternalMemoryFeatures a where
        type VkExternalMemoryFeaturesMType a :: *

        vkExternalMemoryFeatures :: a -> VkExternalMemoryFeaturesMType a

        vkExternalMemoryFeaturesByteOffset :: a -> Int

        readVkExternalMemoryFeatures ::
                                     Ptr a -> IO (VkExternalMemoryFeaturesMType a)

        writeVkExternalMemoryFeatures ::
                                      Ptr a -> VkExternalMemoryFeaturesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'externalMemoryFeatures'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExternalMemoryFeatures a

class HasVkExternalMemoryProperties a where
        type VkExternalMemoryPropertiesMType a :: *

        vkExternalMemoryProperties ::
                                   a -> VkExternalMemoryPropertiesMType a

        vkExternalMemoryPropertiesByteOffset :: a -> Int

        readVkExternalMemoryProperties ::
                                       Ptr a -> IO (VkExternalMemoryPropertiesMType a)

        writeVkExternalMemoryProperties ::
                                        Ptr a -> VkExternalMemoryPropertiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'externalMemoryProperties'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExternalMemoryProperties a

class HasVkExternalSemaphoreFeatures a where
        type VkExternalSemaphoreFeaturesMType a :: *

        vkExternalSemaphoreFeatures ::
                                    a -> VkExternalSemaphoreFeaturesMType a

        vkExternalSemaphoreFeaturesByteOffset :: a -> Int

        readVkExternalSemaphoreFeatures ::
                                        Ptr a -> IO (VkExternalSemaphoreFeaturesMType a)

        writeVkExternalSemaphoreFeatures ::
                                         Ptr a -> VkExternalSemaphoreFeaturesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'externalSemaphoreFeatures'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExternalSemaphoreFeatures a

class HasVkExtraPrimitiveOverestimationSize a where
        type VkExtraPrimitiveOverestimationSizeMType a :: *

        vkExtraPrimitiveOverestimationSize ::
                                           a -> VkExtraPrimitiveOverestimationSizeMType a

        vkExtraPrimitiveOverestimationSizeByteOffset :: a -> Int

        readVkExtraPrimitiveOverestimationSize ::
                                               Ptr a ->
                                                 IO (VkExtraPrimitiveOverestimationSizeMType a)

        writeVkExtraPrimitiveOverestimationSize ::
                                                Ptr a ->
                                                  VkExtraPrimitiveOverestimationSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'extraPrimitiveOverestimationSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExtraPrimitiveOverestimationSize a

class HasVkExtraPrimitiveOverestimationSizeGranularity a where
        type VkExtraPrimitiveOverestimationSizeGranularityMType a :: *

        vkExtraPrimitiveOverestimationSizeGranularity ::
                                                      a ->
                                                        VkExtraPrimitiveOverestimationSizeGranularityMType
                                                          a

        vkExtraPrimitiveOverestimationSizeGranularityByteOffset :: a -> Int

        readVkExtraPrimitiveOverestimationSizeGranularity ::
                                                          Ptr a ->
                                                            IO
                                                              (VkExtraPrimitiveOverestimationSizeGranularityMType
                                                                 a)

        writeVkExtraPrimitiveOverestimationSizeGranularity ::
                                                           Ptr a ->
                                                             VkExtraPrimitiveOverestimationSizeGranularityMType
                                                               a
                                                               -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'extraPrimitiveOverestimationSizeGranularity'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkExtraPrimitiveOverestimationSizeGranularity a

class HasVkFailOp a where
        type VkFailOpMType a :: *

        vkFailOp :: a -> VkFailOpMType a

        vkFailOpByteOffset :: a -> Int

        readVkFailOp :: Ptr a -> IO (VkFailOpMType a)

        writeVkFailOp :: Ptr a -> VkFailOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'failOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFailOp a

class HasVkFd a where
        type VkFdMType a :: *

        vkFd :: a -> VkFdMType a

        vkFdByteOffset :: a -> Int

        readVkFd :: Ptr a -> IO (VkFdMType a)

        writeVkFd :: Ptr a -> VkFdMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'fd'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFd a

class HasVkFeatures a where
        type VkFeaturesMType a :: *

        vkFeatures :: a -> VkFeaturesMType a

        vkFeaturesByteOffset :: a -> Int

        readVkFeatures :: Ptr a -> IO (VkFeaturesMType a)

        writeVkFeatures :: Ptr a -> VkFeaturesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'features'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFeatures a

class HasVkFence a where
        type VkFenceMType a :: *

        vkFence :: a -> VkFenceMType a

        vkFenceByteOffset :: a -> Int

        readVkFence :: Ptr a -> IO (VkFenceMType a)

        writeVkFence :: Ptr a -> VkFenceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'fence'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFence a

class HasVkFillModeNonSolid a where
        type VkFillModeNonSolidMType a :: *

        vkFillModeNonSolid :: a -> VkFillModeNonSolidMType a

        vkFillModeNonSolidByteOffset :: a -> Int

        readVkFillModeNonSolid :: Ptr a -> IO (VkFillModeNonSolidMType a)

        writeVkFillModeNonSolid ::
                                Ptr a -> VkFillModeNonSolidMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'fillModeNonSolid'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFillModeNonSolid a

class HasVkFilterMinmaxImageComponentMapping a where
        type VkFilterMinmaxImageComponentMappingMType a :: *

        vkFilterMinmaxImageComponentMapping ::
                                            a -> VkFilterMinmaxImageComponentMappingMType a

        vkFilterMinmaxImageComponentMappingByteOffset :: a -> Int

        readVkFilterMinmaxImageComponentMapping ::
                                                Ptr a ->
                                                  IO (VkFilterMinmaxImageComponentMappingMType a)

        writeVkFilterMinmaxImageComponentMapping ::
                                                 Ptr a ->
                                                   VkFilterMinmaxImageComponentMappingMType a ->
                                                     IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'filterMinmaxImageComponentMapping'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFilterMinmaxImageComponentMapping a

class HasVkFilterMinmaxSingleComponentFormats a where
        type VkFilterMinmaxSingleComponentFormatsMType a :: *

        vkFilterMinmaxSingleComponentFormats ::
                                             a -> VkFilterMinmaxSingleComponentFormatsMType a

        vkFilterMinmaxSingleComponentFormatsByteOffset :: a -> Int

        readVkFilterMinmaxSingleComponentFormats ::
                                                 Ptr a ->
                                                   IO (VkFilterMinmaxSingleComponentFormatsMType a)

        writeVkFilterMinmaxSingleComponentFormats ::
                                                  Ptr a ->
                                                    VkFilterMinmaxSingleComponentFormatsMType a ->
                                                      IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'filterMinmaxSingleComponentFormats'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFilterMinmaxSingleComponentFormats a

class HasVkFinalLayout a where
        type VkFinalLayoutMType a :: *

        vkFinalLayout :: a -> VkFinalLayoutMType a

        vkFinalLayoutByteOffset :: a -> Int

        readVkFinalLayout :: Ptr a -> IO (VkFinalLayoutMType a)

        writeVkFinalLayout :: Ptr a -> VkFinalLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'finalLayout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFinalLayout a

class HasVkFirstIndex a where
        type VkFirstIndexMType a :: *

        vkFirstIndex :: a -> VkFirstIndexMType a

        vkFirstIndexByteOffset :: a -> Int

        readVkFirstIndex :: Ptr a -> IO (VkFirstIndexMType a)

        writeVkFirstIndex :: Ptr a -> VkFirstIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'firstIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFirstIndex a

class HasVkFirstInstance a where
        type VkFirstInstanceMType a :: *

        vkFirstInstance :: a -> VkFirstInstanceMType a

        vkFirstInstanceByteOffset :: a -> Int

        readVkFirstInstance :: Ptr a -> IO (VkFirstInstanceMType a)

        writeVkFirstInstance :: Ptr a -> VkFirstInstanceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'firstInstance'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFirstInstance a

class HasVkFirstVertex a where
        type VkFirstVertexMType a :: *

        vkFirstVertex :: a -> VkFirstVertexMType a

        vkFirstVertexByteOffset :: a -> Int

        readVkFirstVertex :: Ptr a -> IO (VkFirstVertexMType a)

        writeVkFirstVertex :: Ptr a -> VkFirstVertexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'firstVertex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFirstVertex a

class HasVkFlags a where
        type VkFlagsMType a :: *

        vkFlags :: a -> VkFlagsMType a

        vkFlagsByteOffset :: a -> Int

        readVkFlags :: Ptr a -> IO (VkFlagsMType a)

        writeVkFlags :: Ptr a -> VkFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'flags'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFlags a

class HasVkFloat32Array a where
        type VkFloat32ArrayMType a :: *

        vkFloat32Array :: a -> Int -> VkFloat32ArrayMType a

        vkFloat32ArrayByteOffset :: a -> Int

        readVkFloat32Array :: Ptr a -> Int -> IO (VkFloat32ArrayMType a)

        writeVkFloat32Array ::
                            Ptr a -> Int -> VkFloat32ArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'float32'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFloat32Array a

class HasVkForceExplicitReconstruction a where
        type VkForceExplicitReconstructionMType a :: *

        vkForceExplicitReconstruction ::
                                      a -> VkForceExplicitReconstructionMType a

        vkForceExplicitReconstructionByteOffset :: a -> Int

        readVkForceExplicitReconstruction ::
                                          Ptr a -> IO (VkForceExplicitReconstructionMType a)

        writeVkForceExplicitReconstruction ::
                                           Ptr a -> VkForceExplicitReconstructionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'forceExplicitReconstruction'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkForceExplicitReconstruction a

class HasVkFormat a where
        type VkFormatMType a :: *

        vkFormat :: a -> VkFormatMType a

        vkFormatByteOffset :: a -> Int

        readVkFormat :: Ptr a -> IO (VkFormatMType a)

        writeVkFormat :: Ptr a -> VkFormatMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'format'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFormat a

class HasVkFormatProperties a where
        type VkFormatPropertiesMType a :: *

        vkFormatProperties :: a -> VkFormatPropertiesMType a

        vkFormatPropertiesByteOffset :: a -> Int

        readVkFormatProperties :: Ptr a -> IO (VkFormatPropertiesMType a)

        writeVkFormatProperties ::
                                Ptr a -> VkFormatPropertiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'formatProperties'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFormatProperties a

class HasVkFragmentStoresAndAtomics a where
        type VkFragmentStoresAndAtomicsMType a :: *

        vkFragmentStoresAndAtomics ::
                                   a -> VkFragmentStoresAndAtomicsMType a

        vkFragmentStoresAndAtomicsByteOffset :: a -> Int

        readVkFragmentStoresAndAtomics ::
                                       Ptr a -> IO (VkFragmentStoresAndAtomicsMType a)

        writeVkFragmentStoresAndAtomics ::
                                        Ptr a -> VkFragmentStoresAndAtomicsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'fragmentStoresAndAtomics'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFragmentStoresAndAtomics a

class HasVkFramebuffer a where
        type VkFramebufferMType a :: *

        vkFramebuffer :: a -> VkFramebufferMType a

        vkFramebufferByteOffset :: a -> Int

        readVkFramebuffer :: Ptr a -> IO (VkFramebufferMType a)

        writeVkFramebuffer :: Ptr a -> VkFramebufferMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'framebuffer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFramebuffer a

class HasVkFramebufferColorSampleCounts a where
        type VkFramebufferColorSampleCountsMType a :: *

        vkFramebufferColorSampleCounts ::
                                       a -> VkFramebufferColorSampleCountsMType a

        vkFramebufferColorSampleCountsByteOffset :: a -> Int

        readVkFramebufferColorSampleCounts ::
                                           Ptr a -> IO (VkFramebufferColorSampleCountsMType a)

        writeVkFramebufferColorSampleCounts ::
                                            Ptr a -> VkFramebufferColorSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'framebufferColorSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFramebufferColorSampleCounts a

class HasVkFramebufferDepthSampleCounts a where
        type VkFramebufferDepthSampleCountsMType a :: *

        vkFramebufferDepthSampleCounts ::
                                       a -> VkFramebufferDepthSampleCountsMType a

        vkFramebufferDepthSampleCountsByteOffset :: a -> Int

        readVkFramebufferDepthSampleCounts ::
                                           Ptr a -> IO (VkFramebufferDepthSampleCountsMType a)

        writeVkFramebufferDepthSampleCounts ::
                                            Ptr a -> VkFramebufferDepthSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'framebufferDepthSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFramebufferDepthSampleCounts a

class HasVkFramebufferNoAttachmentsSampleCounts a where
        type VkFramebufferNoAttachmentsSampleCountsMType a :: *

        vkFramebufferNoAttachmentsSampleCounts ::
                                               a -> VkFramebufferNoAttachmentsSampleCountsMType a

        vkFramebufferNoAttachmentsSampleCountsByteOffset :: a -> Int

        readVkFramebufferNoAttachmentsSampleCounts ::
                                                   Ptr a ->
                                                     IO
                                                       (VkFramebufferNoAttachmentsSampleCountsMType
                                                          a)

        writeVkFramebufferNoAttachmentsSampleCounts ::
                                                    Ptr a ->
                                                      VkFramebufferNoAttachmentsSampleCountsMType a
                                                        -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'framebufferNoAttachmentsSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFramebufferNoAttachmentsSampleCounts a

class HasVkFramebufferStencilSampleCounts a where
        type VkFramebufferStencilSampleCountsMType a :: *

        vkFramebufferStencilSampleCounts ::
                                         a -> VkFramebufferStencilSampleCountsMType a

        vkFramebufferStencilSampleCountsByteOffset :: a -> Int

        readVkFramebufferStencilSampleCounts ::
                                             Ptr a -> IO (VkFramebufferStencilSampleCountsMType a)

        writeVkFramebufferStencilSampleCounts ::
                                              Ptr a ->
                                                VkFramebufferStencilSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'framebufferStencilSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFramebufferStencilSampleCounts a

class HasVkFront a where
        type VkFrontMType a :: *

        vkFront :: a -> VkFrontMType a

        vkFrontByteOffset :: a -> Int

        readVkFront :: Ptr a -> IO (VkFrontMType a)

        writeVkFront :: Ptr a -> VkFrontMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'front'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFront a

class HasVkFrontFace a where
        type VkFrontFaceMType a :: *

        vkFrontFace :: a -> VkFrontFaceMType a

        vkFrontFaceByteOffset :: a -> Int

        readVkFrontFace :: Ptr a -> IO (VkFrontFaceMType a)

        writeVkFrontFace :: Ptr a -> VkFrontFaceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'frontFace'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFrontFace a

class HasVkFullDrawIndexUint32 a where
        type VkFullDrawIndexUint32MType a :: *

        vkFullDrawIndexUint32 :: a -> VkFullDrawIndexUint32MType a

        vkFullDrawIndexUint32ByteOffset :: a -> Int

        readVkFullDrawIndexUint32 ::
                                  Ptr a -> IO (VkFullDrawIndexUint32MType a)

        writeVkFullDrawIndexUint32 ::
                                   Ptr a -> VkFullDrawIndexUint32MType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'fullDrawIndexUint32'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFullDrawIndexUint32 a

class HasVkFullyCoveredFragmentShaderInputVariable a where
        type VkFullyCoveredFragmentShaderInputVariableMType a :: *

        vkFullyCoveredFragmentShaderInputVariable ::
                                                  a ->
                                                    VkFullyCoveredFragmentShaderInputVariableMType a

        vkFullyCoveredFragmentShaderInputVariableByteOffset :: a -> Int

        readVkFullyCoveredFragmentShaderInputVariable ::
                                                      Ptr a ->
                                                        IO
                                                          (VkFullyCoveredFragmentShaderInputVariableMType
                                                             a)

        writeVkFullyCoveredFragmentShaderInputVariable ::
                                                       Ptr a ->
                                                         VkFullyCoveredFragmentShaderInputVariableMType
                                                           a
                                                           -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'fullyCoveredFragmentShaderInputVariable'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkFullyCoveredFragmentShaderInputVariable a

class HasVkG a where
        type VkGMType a :: *

        vkG :: a -> VkGMType a

        vkGByteOffset :: a -> Int

        readVkG :: Ptr a -> IO (VkGMType a)

        writeVkG :: Ptr a -> VkGMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'g'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkG a

class HasVkGeometryShader a where
        type VkGeometryShaderMType a :: *

        vkGeometryShader :: a -> VkGeometryShaderMType a

        vkGeometryShaderByteOffset :: a -> Int

        readVkGeometryShader :: Ptr a -> IO (VkGeometryShaderMType a)

        writeVkGeometryShader :: Ptr a -> VkGeometryShaderMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'geometryShader'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkGeometryShader a

class HasVkGlobalAlpha a where
        type VkGlobalAlphaMType a :: *

        vkGlobalAlpha :: a -> VkGlobalAlphaMType a

        vkGlobalAlphaByteOffset :: a -> Int

        readVkGlobalAlpha :: Ptr a -> IO (VkGlobalAlphaMType a)

        writeVkGlobalAlpha :: Ptr a -> VkGlobalAlphaMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'globalAlpha'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkGlobalAlpha a

class HasVkGlobalPriority a where
        type VkGlobalPriorityMType a :: *

        vkGlobalPriority :: a -> VkGlobalPriorityMType a

        vkGlobalPriorityByteOffset :: a -> Int

        readVkGlobalPriority :: Ptr a -> IO (VkGlobalPriorityMType a)

        writeVkGlobalPriority :: Ptr a -> VkGlobalPriorityMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'globalPriority'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkGlobalPriority a

class HasVkHandle a where
        type VkHandleMType a :: *

        vkHandle :: a -> VkHandleMType a

        vkHandleByteOffset :: a -> Int

        readVkHandle :: Ptr a -> IO (VkHandleMType a)

        writeVkHandle :: Ptr a -> VkHandleMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'handle'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkHandle a

class HasVkHandleType a where
        type VkHandleTypeMType a :: *

        vkHandleType :: a -> VkHandleTypeMType a

        vkHandleTypeByteOffset :: a -> Int

        readVkHandleType :: Ptr a -> IO (VkHandleTypeMType a)

        writeVkHandleType :: Ptr a -> VkHandleTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'handleType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkHandleType a

class HasVkHandleTypes a where
        type VkHandleTypesMType a :: *

        vkHandleTypes :: a -> VkHandleTypesMType a

        vkHandleTypesByteOffset :: a -> Int

        readVkHandleTypes :: Ptr a -> IO (VkHandleTypesMType a)

        writeVkHandleTypes :: Ptr a -> VkHandleTypesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'handleTypes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkHandleTypes a

class HasVkHeapIndex a where
        type VkHeapIndexMType a :: *

        vkHeapIndex :: a -> VkHeapIndexMType a

        vkHeapIndexByteOffset :: a -> Int

        readVkHeapIndex :: Ptr a -> IO (VkHeapIndexMType a)

        writeVkHeapIndex :: Ptr a -> VkHeapIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'heapIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkHeapIndex a

class HasVkHeight a where
        type VkHeightMType a :: *

        vkHeight :: a -> VkHeightMType a

        vkHeightByteOffset :: a -> Int

        readVkHeight :: Ptr a -> IO (VkHeightMType a)

        writeVkHeight :: Ptr a -> VkHeightMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'height'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkHeight a

class HasVkHinstance a where
        type VkHinstanceMType a :: *

        vkHinstance :: a -> VkHinstanceMType a

        vkHinstanceByteOffset :: a -> Int

        readVkHinstance :: Ptr a -> IO (VkHinstanceMType a)

        writeVkHinstance :: Ptr a -> VkHinstanceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'hinstance'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkHinstance a

class HasVkHwnd a where
        type VkHwndMType a :: *

        vkHwnd :: a -> VkHwndMType a

        vkHwndByteOffset :: a -> Int

        readVkHwnd :: Ptr a -> IO (VkHwndMType a)

        writeVkHwnd :: Ptr a -> VkHwndMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'hwnd'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkHwnd a

class HasVkImage a where
        type VkImageMType a :: *

        vkImage :: a -> VkImageMType a

        vkImageByteOffset :: a -> Int

        readVkImage :: Ptr a -> IO (VkImageMType a)

        writeVkImage :: Ptr a -> VkImageMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'image'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImage a

class HasVkImageArrayLayers a where
        type VkImageArrayLayersMType a :: *

        vkImageArrayLayers :: a -> VkImageArrayLayersMType a

        vkImageArrayLayersByteOffset :: a -> Int

        readVkImageArrayLayers :: Ptr a -> IO (VkImageArrayLayersMType a)

        writeVkImageArrayLayers ::
                                Ptr a -> VkImageArrayLayersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageArrayLayers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageArrayLayers a

class HasVkImageBindCount a where
        type VkImageBindCountMType a :: *

        vkImageBindCount :: a -> VkImageBindCountMType a

        vkImageBindCountByteOffset :: a -> Int

        readVkImageBindCount :: Ptr a -> IO (VkImageBindCountMType a)

        writeVkImageBindCount :: Ptr a -> VkImageBindCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageBindCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageBindCount a

class HasVkImageColorSpace a where
        type VkImageColorSpaceMType a :: *

        vkImageColorSpace :: a -> VkImageColorSpaceMType a

        vkImageColorSpaceByteOffset :: a -> Int

        readVkImageColorSpace :: Ptr a -> IO (VkImageColorSpaceMType a)

        writeVkImageColorSpace ::
                               Ptr a -> VkImageColorSpaceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageColorSpace'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageColorSpace a

class HasVkImageCubeArray a where
        type VkImageCubeArrayMType a :: *

        vkImageCubeArray :: a -> VkImageCubeArrayMType a

        vkImageCubeArrayByteOffset :: a -> Int

        readVkImageCubeArray :: Ptr a -> IO (VkImageCubeArrayMType a)

        writeVkImageCubeArray :: Ptr a -> VkImageCubeArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageCubeArray'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageCubeArray a

class HasVkImageExtent a where
        type VkImageExtentMType a :: *

        vkImageExtent :: a -> VkImageExtentMType a

        vkImageExtentByteOffset :: a -> Int

        readVkImageExtent :: Ptr a -> IO (VkImageExtentMType a)

        writeVkImageExtent :: Ptr a -> VkImageExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageExtent a

class HasVkImageFormat a where
        type VkImageFormatMType a :: *

        vkImageFormat :: a -> VkImageFormatMType a

        vkImageFormatByteOffset :: a -> Int

        readVkImageFormat :: Ptr a -> IO (VkImageFormatMType a)

        writeVkImageFormat :: Ptr a -> VkImageFormatMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageFormat'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageFormat a

class HasVkImageFormatProperties a where
        type VkImageFormatPropertiesMType a :: *

        vkImageFormatProperties :: a -> VkImageFormatPropertiesMType a

        vkImageFormatPropertiesByteOffset :: a -> Int

        readVkImageFormatProperties ::
                                    Ptr a -> IO (VkImageFormatPropertiesMType a)

        writeVkImageFormatProperties ::
                                     Ptr a -> VkImageFormatPropertiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageFormatProperties'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageFormatProperties a

class HasVkImageGranularity a where
        type VkImageGranularityMType a :: *

        vkImageGranularity :: a -> VkImageGranularityMType a

        vkImageGranularityByteOffset :: a -> Int

        readVkImageGranularity :: Ptr a -> IO (VkImageGranularityMType a)

        writeVkImageGranularity ::
                                Ptr a -> VkImageGranularityMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageGranularity'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageGranularity a

class HasVkImageIndex a where
        type VkImageIndexMType a :: *

        vkImageIndex :: a -> VkImageIndexMType a

        vkImageIndexByteOffset :: a -> Int

        readVkImageIndex :: Ptr a -> IO (VkImageIndexMType a)

        writeVkImageIndex :: Ptr a -> VkImageIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageIndex a

class HasVkImageLayout a where
        type VkImageLayoutMType a :: *

        vkImageLayout :: a -> VkImageLayoutMType a

        vkImageLayoutByteOffset :: a -> Int

        readVkImageLayout :: Ptr a -> IO (VkImageLayoutMType a)

        writeVkImageLayout :: Ptr a -> VkImageLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageLayout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageLayout a

class HasVkImageMipTailFirstLod a where
        type VkImageMipTailFirstLodMType a :: *

        vkImageMipTailFirstLod :: a -> VkImageMipTailFirstLodMType a

        vkImageMipTailFirstLodByteOffset :: a -> Int

        readVkImageMipTailFirstLod ::
                                   Ptr a -> IO (VkImageMipTailFirstLodMType a)

        writeVkImageMipTailFirstLod ::
                                    Ptr a -> VkImageMipTailFirstLodMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageMipTailFirstLod'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageMipTailFirstLod a

class HasVkImageMipTailOffset a where
        type VkImageMipTailOffsetMType a :: *

        vkImageMipTailOffset :: a -> VkImageMipTailOffsetMType a

        vkImageMipTailOffsetByteOffset :: a -> Int

        readVkImageMipTailOffset ::
                                 Ptr a -> IO (VkImageMipTailOffsetMType a)

        writeVkImageMipTailOffset ::
                                  Ptr a -> VkImageMipTailOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageMipTailOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageMipTailOffset a

class HasVkImageMipTailSize a where
        type VkImageMipTailSizeMType a :: *

        vkImageMipTailSize :: a -> VkImageMipTailSizeMType a

        vkImageMipTailSizeByteOffset :: a -> Int

        readVkImageMipTailSize :: Ptr a -> IO (VkImageMipTailSizeMType a)

        writeVkImageMipTailSize ::
                                Ptr a -> VkImageMipTailSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageMipTailSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageMipTailSize a

class HasVkImageMipTailStride a where
        type VkImageMipTailStrideMType a :: *

        vkImageMipTailStride :: a -> VkImageMipTailStrideMType a

        vkImageMipTailStrideByteOffset :: a -> Int

        readVkImageMipTailStride ::
                                 Ptr a -> IO (VkImageMipTailStrideMType a)

        writeVkImageMipTailStride ::
                                  Ptr a -> VkImageMipTailStrideMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageMipTailStride'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageMipTailStride a

class HasVkImageOffset a where
        type VkImageOffsetMType a :: *

        vkImageOffset :: a -> VkImageOffsetMType a

        vkImageOffsetByteOffset :: a -> Int

        readVkImageOffset :: Ptr a -> IO (VkImageOffsetMType a)

        writeVkImageOffset :: Ptr a -> VkImageOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageOffset a

class HasVkImageOpaqueBindCount a where
        type VkImageOpaqueBindCountMType a :: *

        vkImageOpaqueBindCount :: a -> VkImageOpaqueBindCountMType a

        vkImageOpaqueBindCountByteOffset :: a -> Int

        readVkImageOpaqueBindCount ::
                                   Ptr a -> IO (VkImageOpaqueBindCountMType a)

        writeVkImageOpaqueBindCount ::
                                    Ptr a -> VkImageOpaqueBindCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageOpaqueBindCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageOpaqueBindCount a

class HasVkImageSharingMode a where
        type VkImageSharingModeMType a :: *

        vkImageSharingMode :: a -> VkImageSharingModeMType a

        vkImageSharingModeByteOffset :: a -> Int

        readVkImageSharingMode :: Ptr a -> IO (VkImageSharingModeMType a)

        writeVkImageSharingMode ::
                                Ptr a -> VkImageSharingModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageSharingMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageSharingMode a

class HasVkImageSubresource a where
        type VkImageSubresourceMType a :: *

        vkImageSubresource :: a -> VkImageSubresourceMType a

        vkImageSubresourceByteOffset :: a -> Int

        readVkImageSubresource :: Ptr a -> IO (VkImageSubresourceMType a)

        writeVkImageSubresource ::
                                Ptr a -> VkImageSubresourceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageSubresource'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageSubresource a

class HasVkImageType a where
        type VkImageTypeMType a :: *

        vkImageType :: a -> VkImageTypeMType a

        vkImageTypeByteOffset :: a -> Int

        readVkImageType :: Ptr a -> IO (VkImageTypeMType a)

        writeVkImageType :: Ptr a -> VkImageTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageType a

class HasVkImageUsage a where
        type VkImageUsageMType a :: *

        vkImageUsage :: a -> VkImageUsageMType a

        vkImageUsageByteOffset :: a -> Int

        readVkImageUsage :: Ptr a -> IO (VkImageUsageMType a)

        writeVkImageUsage :: Ptr a -> VkImageUsageMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageUsage'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageUsage a

class HasVkImageView a where
        type VkImageViewMType a :: *

        vkImageView :: a -> VkImageViewMType a

        vkImageViewByteOffset :: a -> Int

        readVkImageView :: Ptr a -> IO (VkImageViewMType a)

        writeVkImageView :: Ptr a -> VkImageViewMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'imageView'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImageView a

class HasVkImplementationVersion a where
        type VkImplementationVersionMType a :: *

        vkImplementationVersion :: a -> VkImplementationVersionMType a

        vkImplementationVersionByteOffset :: a -> Int

        readVkImplementationVersion ::
                                    Ptr a -> IO (VkImplementationVersionMType a)

        writeVkImplementationVersion ::
                                     Ptr a -> VkImplementationVersionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'implementationVersion'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkImplementationVersion a

class HasVkIndependentBlend a where
        type VkIndependentBlendMType a :: *

        vkIndependentBlend :: a -> VkIndependentBlendMType a

        vkIndependentBlendByteOffset :: a -> Int

        readVkIndependentBlend :: Ptr a -> IO (VkIndependentBlendMType a)

        writeVkIndependentBlend ::
                                Ptr a -> VkIndependentBlendMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'independentBlend'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkIndependentBlend a

class HasVkIndexCount a where
        type VkIndexCountMType a :: *

        vkIndexCount :: a -> VkIndexCountMType a

        vkIndexCountByteOffset :: a -> Int

        readVkIndexCount :: Ptr a -> IO (VkIndexCountMType a)

        writeVkIndexCount :: Ptr a -> VkIndexCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'indexCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkIndexCount a

class HasVkIndexType a where
        type VkIndexTypeMType a :: *

        vkIndexType :: a -> VkIndexTypeMType a

        vkIndexTypeByteOffset :: a -> Int

        readVkIndexType :: Ptr a -> IO (VkIndexTypeMType a)

        writeVkIndexType :: Ptr a -> VkIndexTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'indexType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkIndexType a

class HasVkIndirectCommandsLayout a where
        type VkIndirectCommandsLayoutMType a :: *

        vkIndirectCommandsLayout :: a -> VkIndirectCommandsLayoutMType a

        vkIndirectCommandsLayoutByteOffset :: a -> Int

        readVkIndirectCommandsLayout ::
                                     Ptr a -> IO (VkIndirectCommandsLayoutMType a)

        writeVkIndirectCommandsLayout ::
                                      Ptr a -> VkIndirectCommandsLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'indirectCommandsLayout'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkIndirectCommandsLayout a

class HasVkIndirectCommandsTokenCount a where
        type VkIndirectCommandsTokenCountMType a :: *

        vkIndirectCommandsTokenCount ::
                                     a -> VkIndirectCommandsTokenCountMType a

        vkIndirectCommandsTokenCountByteOffset :: a -> Int

        readVkIndirectCommandsTokenCount ::
                                         Ptr a -> IO (VkIndirectCommandsTokenCountMType a)

        writeVkIndirectCommandsTokenCount ::
                                          Ptr a -> VkIndirectCommandsTokenCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'indirectCommandsTokenCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkIndirectCommandsTokenCount a

class HasVkInheritedQueries a where
        type VkInheritedQueriesMType a :: *

        vkInheritedQueries :: a -> VkInheritedQueriesMType a

        vkInheritedQueriesByteOffset :: a -> Int

        readVkInheritedQueries :: Ptr a -> IO (VkInheritedQueriesMType a)

        writeVkInheritedQueries ::
                                Ptr a -> VkInheritedQueriesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'inheritedQueries'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkInheritedQueries a

class HasVkInitialDataSize a where
        type VkInitialDataSizeMType a :: *

        vkInitialDataSize :: a -> VkInitialDataSizeMType a

        vkInitialDataSizeByteOffset :: a -> Int

        readVkInitialDataSize :: Ptr a -> IO (VkInitialDataSizeMType a)

        writeVkInitialDataSize ::
                               Ptr a -> VkInitialDataSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'initialDataSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkInitialDataSize a

class HasVkInitialLayout a where
        type VkInitialLayoutMType a :: *

        vkInitialLayout :: a -> VkInitialLayoutMType a

        vkInitialLayoutByteOffset :: a -> Int

        readVkInitialLayout :: Ptr a -> IO (VkInitialLayoutMType a)

        writeVkInitialLayout :: Ptr a -> VkInitialLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'initialLayout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkInitialLayout a

class HasVkInputAttachmentCount a where
        type VkInputAttachmentCountMType a :: *

        vkInputAttachmentCount :: a -> VkInputAttachmentCountMType a

        vkInputAttachmentCountByteOffset :: a -> Int

        readVkInputAttachmentCount ::
                                   Ptr a -> IO (VkInputAttachmentCountMType a)

        writeVkInputAttachmentCount ::
                                    Ptr a -> VkInputAttachmentCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'inputAttachmentCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkInputAttachmentCount a

class HasVkInputAttachmentIndex a where
        type VkInputAttachmentIndexMType a :: *

        vkInputAttachmentIndex :: a -> VkInputAttachmentIndexMType a

        vkInputAttachmentIndexByteOffset :: a -> Int

        readVkInputAttachmentIndex ::
                                   Ptr a -> IO (VkInputAttachmentIndexMType a)

        writeVkInputAttachmentIndex ::
                                    Ptr a -> VkInputAttachmentIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'inputAttachmentIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkInputAttachmentIndex a

class HasVkInputRate a where
        type VkInputRateMType a :: *

        vkInputRate :: a -> VkInputRateMType a

        vkInputRateByteOffset :: a -> Int

        readVkInputRate :: Ptr a -> IO (VkInputRateMType a)

        writeVkInputRate :: Ptr a -> VkInputRateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'inputRate'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkInputRate a

class HasVkInstanceCount a where
        type VkInstanceCountMType a :: *

        vkInstanceCount :: a -> VkInstanceCountMType a

        vkInstanceCountByteOffset :: a -> Int

        readVkInstanceCount :: Ptr a -> IO (VkInstanceCountMType a)

        writeVkInstanceCount :: Ptr a -> VkInstanceCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'instanceCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkInstanceCount a

class HasVkInt32Array a where
        type VkInt32ArrayMType a :: *

        vkInt32Array :: a -> Int -> VkInt32ArrayMType a

        vkInt32ArrayByteOffset :: a -> Int

        readVkInt32Array :: Ptr a -> Int -> IO (VkInt32ArrayMType a)

        writeVkInt32Array :: Ptr a -> Int -> VkInt32ArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'int32'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkInt32Array a

class HasVkLargePoints a where
        type VkLargePointsMType a :: *

        vkLargePoints :: a -> VkLargePointsMType a

        vkLargePointsByteOffset :: a -> Int

        readVkLargePoints :: Ptr a -> IO (VkLargePointsMType a)

        writeVkLargePoints :: Ptr a -> VkLargePointsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'largePoints'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLargePoints a

class HasVkLayer a where
        type VkLayerMType a :: *

        vkLayer :: a -> VkLayerMType a

        vkLayerByteOffset :: a -> Int

        readVkLayer :: Ptr a -> IO (VkLayerMType a)

        writeVkLayer :: Ptr a -> VkLayerMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'layer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLayer a

class HasVkLayerCount a where
        type VkLayerCountMType a :: *

        vkLayerCount :: a -> VkLayerCountMType a

        vkLayerCountByteOffset :: a -> Int

        readVkLayerCount :: Ptr a -> IO (VkLayerCountMType a)

        writeVkLayerCount :: Ptr a -> VkLayerCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'layerCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLayerCount a

class HasVkLayerNameArray a where
        type VkLayerNameArrayMType a :: *

        vkLayerNameArray :: a -> Int -> VkLayerNameArrayMType a

        vkLayerNameArrayByteOffset :: a -> Int

        readVkLayerNameArray ::
                             Ptr a -> Int -> IO (VkLayerNameArrayMType a)

        writeVkLayerNameArray ::
                              Ptr a -> Int -> VkLayerNameArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'layerName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLayerNameArray a

class HasVkLayers a where
        type VkLayersMType a :: *

        vkLayers :: a -> VkLayersMType a

        vkLayersByteOffset :: a -> Int

        readVkLayers :: Ptr a -> IO (VkLayersMType a)

        writeVkLayers :: Ptr a -> VkLayersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'layers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLayers a

class HasVkLayout a where
        type VkLayoutMType a :: *

        vkLayout :: a -> VkLayoutMType a

        vkLayoutByteOffset :: a -> Int

        readVkLayout :: Ptr a -> IO (VkLayoutMType a)

        writeVkLayout :: Ptr a -> VkLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'layout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLayout a

class HasVkLdsSizePerLocalWorkGroup a where
        type VkLdsSizePerLocalWorkGroupMType a :: *

        vkLdsSizePerLocalWorkGroup ::
                                   a -> VkLdsSizePerLocalWorkGroupMType a

        vkLdsSizePerLocalWorkGroupByteOffset :: a -> Int

        readVkLdsSizePerLocalWorkGroup ::
                                       Ptr a -> IO (VkLdsSizePerLocalWorkGroupMType a)

        writeVkLdsSizePerLocalWorkGroup ::
                                        Ptr a -> VkLdsSizePerLocalWorkGroupMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'ldsSizePerLocalWorkGroup'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLdsSizePerLocalWorkGroup a

class HasVkLdsUsageSizeInBytes a where
        type VkLdsUsageSizeInBytesMType a :: *

        vkLdsUsageSizeInBytes :: a -> VkLdsUsageSizeInBytesMType a

        vkLdsUsageSizeInBytesByteOffset :: a -> Int

        readVkLdsUsageSizeInBytes ::
                                  Ptr a -> IO (VkLdsUsageSizeInBytesMType a)

        writeVkLdsUsageSizeInBytes ::
                                   Ptr a -> VkLdsUsageSizeInBytesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'ldsUsageSizeInBytes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLdsUsageSizeInBytes a

class HasVkLevel a where
        type VkLevelMType a :: *

        vkLevel :: a -> VkLevelMType a

        vkLevelByteOffset :: a -> Int

        readVkLevel :: Ptr a -> IO (VkLevelMType a)

        writeVkLevel :: Ptr a -> VkLevelMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'level'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLevel a

class HasVkLevelCount a where
        type VkLevelCountMType a :: *

        vkLevelCount :: a -> VkLevelCountMType a

        vkLevelCountByteOffset :: a -> Int

        readVkLevelCount :: Ptr a -> IO (VkLevelCountMType a)

        writeVkLevelCount :: Ptr a -> VkLevelCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'levelCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLevelCount a

class HasVkLimits a where
        type VkLimitsMType a :: *

        vkLimits :: a -> VkLimitsMType a

        vkLimitsByteOffset :: a -> Int

        readVkLimits :: Ptr a -> IO (VkLimitsMType a)

        writeVkLimits :: Ptr a -> VkLimitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'limits'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLimits a

class HasVkLineWidth a where
        type VkLineWidthMType a :: *

        vkLineWidth :: a -> VkLineWidthMType a

        vkLineWidthByteOffset :: a -> Int

        readVkLineWidth :: Ptr a -> IO (VkLineWidthMType a)

        writeVkLineWidth :: Ptr a -> VkLineWidthMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'lineWidth'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLineWidth a

class HasVkLineWidthGranularity a where
        type VkLineWidthGranularityMType a :: *

        vkLineWidthGranularity :: a -> VkLineWidthGranularityMType a

        vkLineWidthGranularityByteOffset :: a -> Int

        readVkLineWidthGranularity ::
                                   Ptr a -> IO (VkLineWidthGranularityMType a)

        writeVkLineWidthGranularity ::
                                    Ptr a -> VkLineWidthGranularityMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'lineWidthGranularity'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLineWidthGranularity a

class HasVkLineWidthRangeArray a where
        type VkLineWidthRangeArrayMType a :: *

        vkLineWidthRangeArray :: a -> Int -> VkLineWidthRangeArrayMType a

        vkLineWidthRangeArrayByteOffset :: a -> Int

        readVkLineWidthRangeArray ::
                                  Ptr a -> Int -> IO (VkLineWidthRangeArrayMType a)

        writeVkLineWidthRangeArray ::
                                   Ptr a -> Int -> VkLineWidthRangeArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'lineWidthRange'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLineWidthRangeArray a

class HasVkLinearTilingFeatures a where
        type VkLinearTilingFeaturesMType a :: *

        vkLinearTilingFeatures :: a -> VkLinearTilingFeaturesMType a

        vkLinearTilingFeaturesByteOffset :: a -> Int

        readVkLinearTilingFeatures ::
                                   Ptr a -> IO (VkLinearTilingFeaturesMType a)

        writeVkLinearTilingFeatures ::
                                    Ptr a -> VkLinearTilingFeaturesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'linearTilingFeatures'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLinearTilingFeatures a

class HasVkLoadOp a where
        type VkLoadOpMType a :: *

        vkLoadOp :: a -> VkLoadOpMType a

        vkLoadOpByteOffset :: a -> Int

        readVkLoadOp :: Ptr a -> IO (VkLoadOpMType a)

        writeVkLoadOp :: Ptr a -> VkLoadOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'loadOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLoadOp a

class HasVkLocation a where
        type VkLocationMType a :: *

        vkLocation :: a -> VkLocationMType a

        vkLocationByteOffset :: a -> Int

        readVkLocation :: Ptr a -> IO (VkLocationMType a)

        writeVkLocation :: Ptr a -> VkLocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'location'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLocation a

class HasVkLogicOp a where
        type VkLogicOpMType a :: *

        vkLogicOp :: a -> VkLogicOpMType a

        vkLogicOpByteOffset :: a -> Int

        readVkLogicOp :: Ptr a -> IO (VkLogicOpMType a)

        writeVkLogicOp :: Ptr a -> VkLogicOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'logicOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLogicOp a

class HasVkLogicOpEnable a where
        type VkLogicOpEnableMType a :: *

        vkLogicOpEnable :: a -> VkLogicOpEnableMType a

        vkLogicOpEnableByteOffset :: a -> Int

        readVkLogicOpEnable :: Ptr a -> IO (VkLogicOpEnableMType a)

        writeVkLogicOpEnable :: Ptr a -> VkLogicOpEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'logicOpEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkLogicOpEnable a

class HasVkMagFilter a where
        type VkMagFilterMType a :: *

        vkMagFilter :: a -> VkMagFilterMType a

        vkMagFilterByteOffset :: a -> Int

        readVkMagFilter :: Ptr a -> IO (VkMagFilterMType a)

        writeVkMagFilter :: Ptr a -> VkMagFilterMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'magFilter'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMagFilter a

class HasVkMapEntryCount a where
        type VkMapEntryCountMType a :: *

        vkMapEntryCount :: a -> VkMapEntryCountMType a

        vkMapEntryCountByteOffset :: a -> Int

        readVkMapEntryCount :: Ptr a -> IO (VkMapEntryCountMType a)

        writeVkMapEntryCount :: Ptr a -> VkMapEntryCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'mapEntryCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMapEntryCount a

class HasVkMaxAnisotropy a where
        type VkMaxAnisotropyMType a :: *

        vkMaxAnisotropy :: a -> VkMaxAnisotropyMType a

        vkMaxAnisotropyByteOffset :: a -> Int

        readVkMaxAnisotropy :: Ptr a -> IO (VkMaxAnisotropyMType a)

        writeVkMaxAnisotropy :: Ptr a -> VkMaxAnisotropyMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxAnisotropy'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxAnisotropy a

class HasVkMaxArrayLayers a where
        type VkMaxArrayLayersMType a :: *

        vkMaxArrayLayers :: a -> VkMaxArrayLayersMType a

        vkMaxArrayLayersByteOffset :: a -> Int

        readVkMaxArrayLayers :: Ptr a -> IO (VkMaxArrayLayersMType a)

        writeVkMaxArrayLayers :: Ptr a -> VkMaxArrayLayersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxArrayLayers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxArrayLayers a

class HasVkMaxBoundDescriptorSets a where
        type VkMaxBoundDescriptorSetsMType a :: *

        vkMaxBoundDescriptorSets :: a -> VkMaxBoundDescriptorSetsMType a

        vkMaxBoundDescriptorSetsByteOffset :: a -> Int

        readVkMaxBoundDescriptorSets ::
                                     Ptr a -> IO (VkMaxBoundDescriptorSetsMType a)

        writeVkMaxBoundDescriptorSets ::
                                      Ptr a -> VkMaxBoundDescriptorSetsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxBoundDescriptorSets'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxBoundDescriptorSets a

class HasVkMaxClipDistances a where
        type VkMaxClipDistancesMType a :: *

        vkMaxClipDistances :: a -> VkMaxClipDistancesMType a

        vkMaxClipDistancesByteOffset :: a -> Int

        readVkMaxClipDistances :: Ptr a -> IO (VkMaxClipDistancesMType a)

        writeVkMaxClipDistances ::
                                Ptr a -> VkMaxClipDistancesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxClipDistances'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxClipDistances a

class HasVkMaxColorAttachments a where
        type VkMaxColorAttachmentsMType a :: *

        vkMaxColorAttachments :: a -> VkMaxColorAttachmentsMType a

        vkMaxColorAttachmentsByteOffset :: a -> Int

        readVkMaxColorAttachments ::
                                  Ptr a -> IO (VkMaxColorAttachmentsMType a)

        writeVkMaxColorAttachments ::
                                   Ptr a -> VkMaxColorAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxColorAttachments'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxColorAttachments a

class HasVkMaxCombinedClipAndCullDistances a where
        type VkMaxCombinedClipAndCullDistancesMType a :: *

        vkMaxCombinedClipAndCullDistances ::
                                          a -> VkMaxCombinedClipAndCullDistancesMType a

        vkMaxCombinedClipAndCullDistancesByteOffset :: a -> Int

        readVkMaxCombinedClipAndCullDistances ::
                                              Ptr a -> IO (VkMaxCombinedClipAndCullDistancesMType a)

        writeVkMaxCombinedClipAndCullDistances ::
                                               Ptr a ->
                                                 VkMaxCombinedClipAndCullDistancesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxCombinedClipAndCullDistances'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxCombinedClipAndCullDistances a

class HasVkMaxComputeSharedMemorySize a where
        type VkMaxComputeSharedMemorySizeMType a :: *

        vkMaxComputeSharedMemorySize ::
                                     a -> VkMaxComputeSharedMemorySizeMType a

        vkMaxComputeSharedMemorySizeByteOffset :: a -> Int

        readVkMaxComputeSharedMemorySize ::
                                         Ptr a -> IO (VkMaxComputeSharedMemorySizeMType a)

        writeVkMaxComputeSharedMemorySize ::
                                          Ptr a -> VkMaxComputeSharedMemorySizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxComputeSharedMemorySize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxComputeSharedMemorySize a

class HasVkMaxComputeWorkGroupCountArray a where
        type VkMaxComputeWorkGroupCountArrayMType a :: *

        vkMaxComputeWorkGroupCountArray ::
                                        a -> Int -> VkMaxComputeWorkGroupCountArrayMType a

        vkMaxComputeWorkGroupCountArrayByteOffset :: a -> Int

        readVkMaxComputeWorkGroupCountArray ::
                                            Ptr a ->
                                              Int -> IO (VkMaxComputeWorkGroupCountArrayMType a)

        writeVkMaxComputeWorkGroupCountArray ::
                                             Ptr a ->
                                               Int ->
                                                 VkMaxComputeWorkGroupCountArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxComputeWorkGroupCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxComputeWorkGroupCountArray a

class HasVkMaxComputeWorkGroupInvocations a where
        type VkMaxComputeWorkGroupInvocationsMType a :: *

        vkMaxComputeWorkGroupInvocations ::
                                         a -> VkMaxComputeWorkGroupInvocationsMType a

        vkMaxComputeWorkGroupInvocationsByteOffset :: a -> Int

        readVkMaxComputeWorkGroupInvocations ::
                                             Ptr a -> IO (VkMaxComputeWorkGroupInvocationsMType a)

        writeVkMaxComputeWorkGroupInvocations ::
                                              Ptr a ->
                                                VkMaxComputeWorkGroupInvocationsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxComputeWorkGroupInvocations'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxComputeWorkGroupInvocations a

class HasVkMaxComputeWorkGroupSizeArray a where
        type VkMaxComputeWorkGroupSizeArrayMType a :: *

        vkMaxComputeWorkGroupSizeArray ::
                                       a -> Int -> VkMaxComputeWorkGroupSizeArrayMType a

        vkMaxComputeWorkGroupSizeArrayByteOffset :: a -> Int

        readVkMaxComputeWorkGroupSizeArray ::
                                           Ptr a ->
                                             Int -> IO (VkMaxComputeWorkGroupSizeArrayMType a)

        writeVkMaxComputeWorkGroupSizeArray ::
                                            Ptr a ->
                                              Int -> VkMaxComputeWorkGroupSizeArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxComputeWorkGroupSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxComputeWorkGroupSizeArray a

class HasVkMaxContentLightLevel a where
        type VkMaxContentLightLevelMType a :: *

        vkMaxContentLightLevel :: a -> VkMaxContentLightLevelMType a

        vkMaxContentLightLevelByteOffset :: a -> Int

        readVkMaxContentLightLevel ::
                                   Ptr a -> IO (VkMaxContentLightLevelMType a)

        writeVkMaxContentLightLevel ::
                                    Ptr a -> VkMaxContentLightLevelMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxContentLightLevel'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxContentLightLevel a

class HasVkMaxCullDistances a where
        type VkMaxCullDistancesMType a :: *

        vkMaxCullDistances :: a -> VkMaxCullDistancesMType a

        vkMaxCullDistancesByteOffset :: a -> Int

        readVkMaxCullDistances :: Ptr a -> IO (VkMaxCullDistancesMType a)

        writeVkMaxCullDistances ::
                                Ptr a -> VkMaxCullDistancesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxCullDistances'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxCullDistances a

class HasVkMaxDepth a where
        type VkMaxDepthMType a :: *

        vkMaxDepth :: a -> VkMaxDepthMType a

        vkMaxDepthByteOffset :: a -> Int

        readVkMaxDepth :: Ptr a -> IO (VkMaxDepthMType a)

        writeVkMaxDepth :: Ptr a -> VkMaxDepthMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxDepth'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDepth a

class HasVkMaxDepthBounds a where
        type VkMaxDepthBoundsMType a :: *

        vkMaxDepthBounds :: a -> VkMaxDepthBoundsMType a

        vkMaxDepthBoundsByteOffset :: a -> Int

        readVkMaxDepthBounds :: Ptr a -> IO (VkMaxDepthBoundsMType a)

        writeVkMaxDepthBounds :: Ptr a -> VkMaxDepthBoundsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxDepthBounds'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDepthBounds a

class HasVkMaxDescriptorSetInputAttachments a where
        type VkMaxDescriptorSetInputAttachmentsMType a :: *

        vkMaxDescriptorSetInputAttachments ::
                                           a -> VkMaxDescriptorSetInputAttachmentsMType a

        vkMaxDescriptorSetInputAttachmentsByteOffset :: a -> Int

        readVkMaxDescriptorSetInputAttachments ::
                                               Ptr a ->
                                                 IO (VkMaxDescriptorSetInputAttachmentsMType a)

        writeVkMaxDescriptorSetInputAttachments ::
                                                Ptr a ->
                                                  VkMaxDescriptorSetInputAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxDescriptorSetInputAttachments'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDescriptorSetInputAttachments a

class HasVkMaxDescriptorSetSampledImages a where
        type VkMaxDescriptorSetSampledImagesMType a :: *

        vkMaxDescriptorSetSampledImages ::
                                        a -> VkMaxDescriptorSetSampledImagesMType a

        vkMaxDescriptorSetSampledImagesByteOffset :: a -> Int

        readVkMaxDescriptorSetSampledImages ::
                                            Ptr a -> IO (VkMaxDescriptorSetSampledImagesMType a)

        writeVkMaxDescriptorSetSampledImages ::
                                             Ptr a ->
                                               VkMaxDescriptorSetSampledImagesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxDescriptorSetSampledImages'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDescriptorSetSampledImages a

class HasVkMaxDescriptorSetSamplers a where
        type VkMaxDescriptorSetSamplersMType a :: *

        vkMaxDescriptorSetSamplers ::
                                   a -> VkMaxDescriptorSetSamplersMType a

        vkMaxDescriptorSetSamplersByteOffset :: a -> Int

        readVkMaxDescriptorSetSamplers ::
                                       Ptr a -> IO (VkMaxDescriptorSetSamplersMType a)

        writeVkMaxDescriptorSetSamplers ::
                                        Ptr a -> VkMaxDescriptorSetSamplersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxDescriptorSetSamplers'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDescriptorSetSamplers a

class HasVkMaxDescriptorSetStorageBuffers a where
        type VkMaxDescriptorSetStorageBuffersMType a :: *

        vkMaxDescriptorSetStorageBuffers ::
                                         a -> VkMaxDescriptorSetStorageBuffersMType a

        vkMaxDescriptorSetStorageBuffersByteOffset :: a -> Int

        readVkMaxDescriptorSetStorageBuffers ::
                                             Ptr a -> IO (VkMaxDescriptorSetStorageBuffersMType a)

        writeVkMaxDescriptorSetStorageBuffers ::
                                              Ptr a ->
                                                VkMaxDescriptorSetStorageBuffersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxDescriptorSetStorageBuffers'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDescriptorSetStorageBuffers a

class HasVkMaxDescriptorSetStorageBuffersDynamic a where
        type VkMaxDescriptorSetStorageBuffersDynamicMType a :: *

        vkMaxDescriptorSetStorageBuffersDynamic ::
                                                a -> VkMaxDescriptorSetStorageBuffersDynamicMType a

        vkMaxDescriptorSetStorageBuffersDynamicByteOffset :: a -> Int

        readVkMaxDescriptorSetStorageBuffersDynamic ::
                                                    Ptr a ->
                                                      IO
                                                        (VkMaxDescriptorSetStorageBuffersDynamicMType
                                                           a)

        writeVkMaxDescriptorSetStorageBuffersDynamic ::
                                                     Ptr a ->
                                                       VkMaxDescriptorSetStorageBuffersDynamicMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxDescriptorSetStorageBuffersDynamic'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDescriptorSetStorageBuffersDynamic a

class HasVkMaxDescriptorSetStorageImages a where
        type VkMaxDescriptorSetStorageImagesMType a :: *

        vkMaxDescriptorSetStorageImages ::
                                        a -> VkMaxDescriptorSetStorageImagesMType a

        vkMaxDescriptorSetStorageImagesByteOffset :: a -> Int

        readVkMaxDescriptorSetStorageImages ::
                                            Ptr a -> IO (VkMaxDescriptorSetStorageImagesMType a)

        writeVkMaxDescriptorSetStorageImages ::
                                             Ptr a ->
                                               VkMaxDescriptorSetStorageImagesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxDescriptorSetStorageImages'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDescriptorSetStorageImages a

class HasVkMaxDescriptorSetUniformBuffers a where
        type VkMaxDescriptorSetUniformBuffersMType a :: *

        vkMaxDescriptorSetUniformBuffers ::
                                         a -> VkMaxDescriptorSetUniformBuffersMType a

        vkMaxDescriptorSetUniformBuffersByteOffset :: a -> Int

        readVkMaxDescriptorSetUniformBuffers ::
                                             Ptr a -> IO (VkMaxDescriptorSetUniformBuffersMType a)

        writeVkMaxDescriptorSetUniformBuffers ::
                                              Ptr a ->
                                                VkMaxDescriptorSetUniformBuffersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxDescriptorSetUniformBuffers'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDescriptorSetUniformBuffers a

class HasVkMaxDescriptorSetUniformBuffersDynamic a where
        type VkMaxDescriptorSetUniformBuffersDynamicMType a :: *

        vkMaxDescriptorSetUniformBuffersDynamic ::
                                                a -> VkMaxDescriptorSetUniformBuffersDynamicMType a

        vkMaxDescriptorSetUniformBuffersDynamicByteOffset :: a -> Int

        readVkMaxDescriptorSetUniformBuffersDynamic ::
                                                    Ptr a ->
                                                      IO
                                                        (VkMaxDescriptorSetUniformBuffersDynamicMType
                                                           a)

        writeVkMaxDescriptorSetUniformBuffersDynamic ::
                                                     Ptr a ->
                                                       VkMaxDescriptorSetUniformBuffersDynamicMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxDescriptorSetUniformBuffersDynamic'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDescriptorSetUniformBuffersDynamic a

class HasVkMaxDiscardRectangles a where
        type VkMaxDiscardRectanglesMType a :: *

        vkMaxDiscardRectangles :: a -> VkMaxDiscardRectanglesMType a

        vkMaxDiscardRectanglesByteOffset :: a -> Int

        readVkMaxDiscardRectangles ::
                                   Ptr a -> IO (VkMaxDiscardRectanglesMType a)

        writeVkMaxDiscardRectangles ::
                                    Ptr a -> VkMaxDiscardRectanglesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxDiscardRectangles'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDiscardRectangles a

class HasVkMaxDrawIndexedIndexValue a where
        type VkMaxDrawIndexedIndexValueMType a :: *

        vkMaxDrawIndexedIndexValue ::
                                   a -> VkMaxDrawIndexedIndexValueMType a

        vkMaxDrawIndexedIndexValueByteOffset :: a -> Int

        readVkMaxDrawIndexedIndexValue ::
                                       Ptr a -> IO (VkMaxDrawIndexedIndexValueMType a)

        writeVkMaxDrawIndexedIndexValue ::
                                        Ptr a -> VkMaxDrawIndexedIndexValueMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxDrawIndexedIndexValue'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDrawIndexedIndexValue a

class HasVkMaxDrawIndirectCount a where
        type VkMaxDrawIndirectCountMType a :: *

        vkMaxDrawIndirectCount :: a -> VkMaxDrawIndirectCountMType a

        vkMaxDrawIndirectCountByteOffset :: a -> Int

        readVkMaxDrawIndirectCount ::
                                   Ptr a -> IO (VkMaxDrawIndirectCountMType a)

        writeVkMaxDrawIndirectCount ::
                                    Ptr a -> VkMaxDrawIndirectCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxDrawIndirectCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDrawIndirectCount a

class HasVkMaxDstExtent a where
        type VkMaxDstExtentMType a :: *

        vkMaxDstExtent :: a -> VkMaxDstExtentMType a

        vkMaxDstExtentByteOffset :: a -> Int

        readVkMaxDstExtent :: Ptr a -> IO (VkMaxDstExtentMType a)

        writeVkMaxDstExtent :: Ptr a -> VkMaxDstExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxDstExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDstExtent a

class HasVkMaxDstPosition a where
        type VkMaxDstPositionMType a :: *

        vkMaxDstPosition :: a -> VkMaxDstPositionMType a

        vkMaxDstPositionByteOffset :: a -> Int

        readVkMaxDstPosition :: Ptr a -> IO (VkMaxDstPositionMType a)

        writeVkMaxDstPosition :: Ptr a -> VkMaxDstPositionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxDstPosition'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxDstPosition a

class HasVkMaxExtent a where
        type VkMaxExtentMType a :: *

        vkMaxExtent :: a -> VkMaxExtentMType a

        vkMaxExtentByteOffset :: a -> Int

        readVkMaxExtent :: Ptr a -> IO (VkMaxExtentMType a)

        writeVkMaxExtent :: Ptr a -> VkMaxExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxExtent a

class HasVkMaxExtraPrimitiveOverestimationSize a where
        type VkMaxExtraPrimitiveOverestimationSizeMType a :: *

        vkMaxExtraPrimitiveOverestimationSize ::
                                              a -> VkMaxExtraPrimitiveOverestimationSizeMType a

        vkMaxExtraPrimitiveOverestimationSizeByteOffset :: a -> Int

        readVkMaxExtraPrimitiveOverestimationSize ::
                                                  Ptr a ->
                                                    IO
                                                      (VkMaxExtraPrimitiveOverestimationSizeMType a)

        writeVkMaxExtraPrimitiveOverestimationSize ::
                                                   Ptr a ->
                                                     VkMaxExtraPrimitiveOverestimationSizeMType a ->
                                                       IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxExtraPrimitiveOverestimationSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxExtraPrimitiveOverestimationSize a

class HasVkMaxFragmentCombinedOutputResources a where
        type VkMaxFragmentCombinedOutputResourcesMType a :: *

        vkMaxFragmentCombinedOutputResources ::
                                             a -> VkMaxFragmentCombinedOutputResourcesMType a

        vkMaxFragmentCombinedOutputResourcesByteOffset :: a -> Int

        readVkMaxFragmentCombinedOutputResources ::
                                                 Ptr a ->
                                                   IO (VkMaxFragmentCombinedOutputResourcesMType a)

        writeVkMaxFragmentCombinedOutputResources ::
                                                  Ptr a ->
                                                    VkMaxFragmentCombinedOutputResourcesMType a ->
                                                      IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxFragmentCombinedOutputResources'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxFragmentCombinedOutputResources a

class HasVkMaxFragmentDualSrcAttachments a where
        type VkMaxFragmentDualSrcAttachmentsMType a :: *

        vkMaxFragmentDualSrcAttachments ::
                                        a -> VkMaxFragmentDualSrcAttachmentsMType a

        vkMaxFragmentDualSrcAttachmentsByteOffset :: a -> Int

        readVkMaxFragmentDualSrcAttachments ::
                                            Ptr a -> IO (VkMaxFragmentDualSrcAttachmentsMType a)

        writeVkMaxFragmentDualSrcAttachments ::
                                             Ptr a ->
                                               VkMaxFragmentDualSrcAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxFragmentDualSrcAttachments'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxFragmentDualSrcAttachments a

class HasVkMaxFragmentInputComponents a where
        type VkMaxFragmentInputComponentsMType a :: *

        vkMaxFragmentInputComponents ::
                                     a -> VkMaxFragmentInputComponentsMType a

        vkMaxFragmentInputComponentsByteOffset :: a -> Int

        readVkMaxFragmentInputComponents ::
                                         Ptr a -> IO (VkMaxFragmentInputComponentsMType a)

        writeVkMaxFragmentInputComponents ::
                                          Ptr a -> VkMaxFragmentInputComponentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxFragmentInputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxFragmentInputComponents a

class HasVkMaxFragmentOutputAttachments a where
        type VkMaxFragmentOutputAttachmentsMType a :: *

        vkMaxFragmentOutputAttachments ::
                                       a -> VkMaxFragmentOutputAttachmentsMType a

        vkMaxFragmentOutputAttachmentsByteOffset :: a -> Int

        readVkMaxFragmentOutputAttachments ::
                                           Ptr a -> IO (VkMaxFragmentOutputAttachmentsMType a)

        writeVkMaxFragmentOutputAttachments ::
                                            Ptr a -> VkMaxFragmentOutputAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxFragmentOutputAttachments'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxFragmentOutputAttachments a

class HasVkMaxFrameAverageLightLevel a where
        type VkMaxFrameAverageLightLevelMType a :: *

        vkMaxFrameAverageLightLevel ::
                                    a -> VkMaxFrameAverageLightLevelMType a

        vkMaxFrameAverageLightLevelByteOffset :: a -> Int

        readVkMaxFrameAverageLightLevel ::
                                        Ptr a -> IO (VkMaxFrameAverageLightLevelMType a)

        writeVkMaxFrameAverageLightLevel ::
                                         Ptr a -> VkMaxFrameAverageLightLevelMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxFrameAverageLightLevel'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxFrameAverageLightLevel a

class HasVkMaxFramebufferHeight a where
        type VkMaxFramebufferHeightMType a :: *

        vkMaxFramebufferHeight :: a -> VkMaxFramebufferHeightMType a

        vkMaxFramebufferHeightByteOffset :: a -> Int

        readVkMaxFramebufferHeight ::
                                   Ptr a -> IO (VkMaxFramebufferHeightMType a)

        writeVkMaxFramebufferHeight ::
                                    Ptr a -> VkMaxFramebufferHeightMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxFramebufferHeight'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxFramebufferHeight a

class HasVkMaxFramebufferLayers a where
        type VkMaxFramebufferLayersMType a :: *

        vkMaxFramebufferLayers :: a -> VkMaxFramebufferLayersMType a

        vkMaxFramebufferLayersByteOffset :: a -> Int

        readVkMaxFramebufferLayers ::
                                   Ptr a -> IO (VkMaxFramebufferLayersMType a)

        writeVkMaxFramebufferLayers ::
                                    Ptr a -> VkMaxFramebufferLayersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxFramebufferLayers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxFramebufferLayers a

class HasVkMaxFramebufferWidth a where
        type VkMaxFramebufferWidthMType a :: *

        vkMaxFramebufferWidth :: a -> VkMaxFramebufferWidthMType a

        vkMaxFramebufferWidthByteOffset :: a -> Int

        readVkMaxFramebufferWidth ::
                                  Ptr a -> IO (VkMaxFramebufferWidthMType a)

        writeVkMaxFramebufferWidth ::
                                   Ptr a -> VkMaxFramebufferWidthMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxFramebufferWidth'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxFramebufferWidth a

class HasVkMaxGeometryInputComponents a where
        type VkMaxGeometryInputComponentsMType a :: *

        vkMaxGeometryInputComponents ::
                                     a -> VkMaxGeometryInputComponentsMType a

        vkMaxGeometryInputComponentsByteOffset :: a -> Int

        readVkMaxGeometryInputComponents ::
                                         Ptr a -> IO (VkMaxGeometryInputComponentsMType a)

        writeVkMaxGeometryInputComponents ::
                                          Ptr a -> VkMaxGeometryInputComponentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxGeometryInputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxGeometryInputComponents a

class HasVkMaxGeometryOutputComponents a where
        type VkMaxGeometryOutputComponentsMType a :: *

        vkMaxGeometryOutputComponents ::
                                      a -> VkMaxGeometryOutputComponentsMType a

        vkMaxGeometryOutputComponentsByteOffset :: a -> Int

        readVkMaxGeometryOutputComponents ::
                                          Ptr a -> IO (VkMaxGeometryOutputComponentsMType a)

        writeVkMaxGeometryOutputComponents ::
                                           Ptr a -> VkMaxGeometryOutputComponentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxGeometryOutputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxGeometryOutputComponents a

class HasVkMaxGeometryOutputVertices a where
        type VkMaxGeometryOutputVerticesMType a :: *

        vkMaxGeometryOutputVertices ::
                                    a -> VkMaxGeometryOutputVerticesMType a

        vkMaxGeometryOutputVerticesByteOffset :: a -> Int

        readVkMaxGeometryOutputVertices ::
                                        Ptr a -> IO (VkMaxGeometryOutputVerticesMType a)

        writeVkMaxGeometryOutputVertices ::
                                         Ptr a -> VkMaxGeometryOutputVerticesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxGeometryOutputVertices'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxGeometryOutputVertices a

class HasVkMaxGeometryShaderInvocations a where
        type VkMaxGeometryShaderInvocationsMType a :: *

        vkMaxGeometryShaderInvocations ::
                                       a -> VkMaxGeometryShaderInvocationsMType a

        vkMaxGeometryShaderInvocationsByteOffset :: a -> Int

        readVkMaxGeometryShaderInvocations ::
                                           Ptr a -> IO (VkMaxGeometryShaderInvocationsMType a)

        writeVkMaxGeometryShaderInvocations ::
                                            Ptr a -> VkMaxGeometryShaderInvocationsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxGeometryShaderInvocations'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxGeometryShaderInvocations a

class HasVkMaxGeometryTotalOutputComponents a where
        type VkMaxGeometryTotalOutputComponentsMType a :: *

        vkMaxGeometryTotalOutputComponents ::
                                           a -> VkMaxGeometryTotalOutputComponentsMType a

        vkMaxGeometryTotalOutputComponentsByteOffset :: a -> Int

        readVkMaxGeometryTotalOutputComponents ::
                                               Ptr a ->
                                                 IO (VkMaxGeometryTotalOutputComponentsMType a)

        writeVkMaxGeometryTotalOutputComponents ::
                                                Ptr a ->
                                                  VkMaxGeometryTotalOutputComponentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxGeometryTotalOutputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxGeometryTotalOutputComponents a

class HasVkMaxImageArrayLayers a where
        type VkMaxImageArrayLayersMType a :: *

        vkMaxImageArrayLayers :: a -> VkMaxImageArrayLayersMType a

        vkMaxImageArrayLayersByteOffset :: a -> Int

        readVkMaxImageArrayLayers ::
                                  Ptr a -> IO (VkMaxImageArrayLayersMType a)

        writeVkMaxImageArrayLayers ::
                                   Ptr a -> VkMaxImageArrayLayersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxImageArrayLayers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxImageArrayLayers a

class HasVkMaxImageCount a where
        type VkMaxImageCountMType a :: *

        vkMaxImageCount :: a -> VkMaxImageCountMType a

        vkMaxImageCountByteOffset :: a -> Int

        readVkMaxImageCount :: Ptr a -> IO (VkMaxImageCountMType a)

        writeVkMaxImageCount :: Ptr a -> VkMaxImageCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxImageCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxImageCount a

class HasVkMaxImageDimension1D a where
        type VkMaxImageDimension1DMType a :: *

        vkMaxImageDimension1D :: a -> VkMaxImageDimension1DMType a

        vkMaxImageDimension1DByteOffset :: a -> Int

        readVkMaxImageDimension1D ::
                                  Ptr a -> IO (VkMaxImageDimension1DMType a)

        writeVkMaxImageDimension1D ::
                                   Ptr a -> VkMaxImageDimension1DMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxImageDimension1D'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxImageDimension1D a

class HasVkMaxImageDimension2D a where
        type VkMaxImageDimension2DMType a :: *

        vkMaxImageDimension2D :: a -> VkMaxImageDimension2DMType a

        vkMaxImageDimension2DByteOffset :: a -> Int

        readVkMaxImageDimension2D ::
                                  Ptr a -> IO (VkMaxImageDimension2DMType a)

        writeVkMaxImageDimension2D ::
                                   Ptr a -> VkMaxImageDimension2DMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxImageDimension2D'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxImageDimension2D a

class HasVkMaxImageDimension3D a where
        type VkMaxImageDimension3DMType a :: *

        vkMaxImageDimension3D :: a -> VkMaxImageDimension3DMType a

        vkMaxImageDimension3DByteOffset :: a -> Int

        readVkMaxImageDimension3D ::
                                  Ptr a -> IO (VkMaxImageDimension3DMType a)

        writeVkMaxImageDimension3D ::
                                   Ptr a -> VkMaxImageDimension3DMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxImageDimension3D'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxImageDimension3D a

class HasVkMaxImageDimensionCube a where
        type VkMaxImageDimensionCubeMType a :: *

        vkMaxImageDimensionCube :: a -> VkMaxImageDimensionCubeMType a

        vkMaxImageDimensionCubeByteOffset :: a -> Int

        readVkMaxImageDimensionCube ::
                                    Ptr a -> IO (VkMaxImageDimensionCubeMType a)

        writeVkMaxImageDimensionCube ::
                                     Ptr a -> VkMaxImageDimensionCubeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxImageDimensionCube'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxImageDimensionCube a

class HasVkMaxImageExtent a where
        type VkMaxImageExtentMType a :: *

        vkMaxImageExtent :: a -> VkMaxImageExtentMType a

        vkMaxImageExtentByteOffset :: a -> Int

        readVkMaxImageExtent :: Ptr a -> IO (VkMaxImageExtentMType a)

        writeVkMaxImageExtent :: Ptr a -> VkMaxImageExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxImageExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxImageExtent a

class HasVkMaxIndirectCommandsLayoutTokenCount a where
        type VkMaxIndirectCommandsLayoutTokenCountMType a :: *

        vkMaxIndirectCommandsLayoutTokenCount ::
                                              a -> VkMaxIndirectCommandsLayoutTokenCountMType a

        vkMaxIndirectCommandsLayoutTokenCountByteOffset :: a -> Int

        readVkMaxIndirectCommandsLayoutTokenCount ::
                                                  Ptr a ->
                                                    IO
                                                      (VkMaxIndirectCommandsLayoutTokenCountMType a)

        writeVkMaxIndirectCommandsLayoutTokenCount ::
                                                   Ptr a ->
                                                     VkMaxIndirectCommandsLayoutTokenCountMType a ->
                                                       IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxIndirectCommandsLayoutTokenCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxIndirectCommandsLayoutTokenCount a

class HasVkMaxInterpolationOffset a where
        type VkMaxInterpolationOffsetMType a :: *

        vkMaxInterpolationOffset :: a -> VkMaxInterpolationOffsetMType a

        vkMaxInterpolationOffsetByteOffset :: a -> Int

        readVkMaxInterpolationOffset ::
                                     Ptr a -> IO (VkMaxInterpolationOffsetMType a)

        writeVkMaxInterpolationOffset ::
                                      Ptr a -> VkMaxInterpolationOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxInterpolationOffset'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxInterpolationOffset a

class HasVkMaxLod a where
        type VkMaxLodMType a :: *

        vkMaxLod :: a -> VkMaxLodMType a

        vkMaxLodByteOffset :: a -> Int

        readVkMaxLod :: Ptr a -> IO (VkMaxLodMType a)

        writeVkMaxLod :: Ptr a -> VkMaxLodMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxLod'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxLod a

class HasVkMaxLuminance a where
        type VkMaxLuminanceMType a :: *

        vkMaxLuminance :: a -> VkMaxLuminanceMType a

        vkMaxLuminanceByteOffset :: a -> Int

        readVkMaxLuminance :: Ptr a -> IO (VkMaxLuminanceMType a)

        writeVkMaxLuminance :: Ptr a -> VkMaxLuminanceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxLuminance'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxLuminance a

class HasVkMaxMemoryAllocationCount a where
        type VkMaxMemoryAllocationCountMType a :: *

        vkMaxMemoryAllocationCount ::
                                   a -> VkMaxMemoryAllocationCountMType a

        vkMaxMemoryAllocationCountByteOffset :: a -> Int

        readVkMaxMemoryAllocationCount ::
                                       Ptr a -> IO (VkMaxMemoryAllocationCountMType a)

        writeVkMaxMemoryAllocationCount ::
                                        Ptr a -> VkMaxMemoryAllocationCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxMemoryAllocationCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxMemoryAllocationCount a

class HasVkMaxMipLevels a where
        type VkMaxMipLevelsMType a :: *

        vkMaxMipLevels :: a -> VkMaxMipLevelsMType a

        vkMaxMipLevelsByteOffset :: a -> Int

        readVkMaxMipLevels :: Ptr a -> IO (VkMaxMipLevelsMType a)

        writeVkMaxMipLevels :: Ptr a -> VkMaxMipLevelsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxMipLevels'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxMipLevels a

class HasVkMaxMultiviewInstanceIndex a where
        type VkMaxMultiviewInstanceIndexMType a :: *

        vkMaxMultiviewInstanceIndex ::
                                    a -> VkMaxMultiviewInstanceIndexMType a

        vkMaxMultiviewInstanceIndexByteOffset :: a -> Int

        readVkMaxMultiviewInstanceIndex ::
                                        Ptr a -> IO (VkMaxMultiviewInstanceIndexMType a)

        writeVkMaxMultiviewInstanceIndex ::
                                         Ptr a -> VkMaxMultiviewInstanceIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxMultiviewInstanceIndex'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxMultiviewInstanceIndex a

class HasVkMaxMultiviewViewCount a where
        type VkMaxMultiviewViewCountMType a :: *

        vkMaxMultiviewViewCount :: a -> VkMaxMultiviewViewCountMType a

        vkMaxMultiviewViewCountByteOffset :: a -> Int

        readVkMaxMultiviewViewCount ::
                                    Ptr a -> IO (VkMaxMultiviewViewCountMType a)

        writeVkMaxMultiviewViewCount ::
                                     Ptr a -> VkMaxMultiviewViewCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxMultiviewViewCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxMultiviewViewCount a

class HasVkMaxObjectEntryCounts a where
        type VkMaxObjectEntryCountsMType a :: *

        vkMaxObjectEntryCounts :: a -> VkMaxObjectEntryCountsMType a

        vkMaxObjectEntryCountsByteOffset :: a -> Int

        readVkMaxObjectEntryCounts ::
                                   Ptr a -> IO (VkMaxObjectEntryCountsMType a)

        writeVkMaxObjectEntryCounts ::
                                    Ptr a -> VkMaxObjectEntryCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxObjectEntryCounts'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxObjectEntryCounts a

class HasVkMaxPerStageDescriptorInputAttachments a where
        type VkMaxPerStageDescriptorInputAttachmentsMType a :: *

        vkMaxPerStageDescriptorInputAttachments ::
                                                a -> VkMaxPerStageDescriptorInputAttachmentsMType a

        vkMaxPerStageDescriptorInputAttachmentsByteOffset :: a -> Int

        readVkMaxPerStageDescriptorInputAttachments ::
                                                    Ptr a ->
                                                      IO
                                                        (VkMaxPerStageDescriptorInputAttachmentsMType
                                                           a)

        writeVkMaxPerStageDescriptorInputAttachments ::
                                                     Ptr a ->
                                                       VkMaxPerStageDescriptorInputAttachmentsMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxPerStageDescriptorInputAttachments'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPerStageDescriptorInputAttachments a

class HasVkMaxPerStageDescriptorSampledImages a where
        type VkMaxPerStageDescriptorSampledImagesMType a :: *

        vkMaxPerStageDescriptorSampledImages ::
                                             a -> VkMaxPerStageDescriptorSampledImagesMType a

        vkMaxPerStageDescriptorSampledImagesByteOffset :: a -> Int

        readVkMaxPerStageDescriptorSampledImages ::
                                                 Ptr a ->
                                                   IO (VkMaxPerStageDescriptorSampledImagesMType a)

        writeVkMaxPerStageDescriptorSampledImages ::
                                                  Ptr a ->
                                                    VkMaxPerStageDescriptorSampledImagesMType a ->
                                                      IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxPerStageDescriptorSampledImages'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPerStageDescriptorSampledImages a

class HasVkMaxPerStageDescriptorSamplers a where
        type VkMaxPerStageDescriptorSamplersMType a :: *

        vkMaxPerStageDescriptorSamplers ::
                                        a -> VkMaxPerStageDescriptorSamplersMType a

        vkMaxPerStageDescriptorSamplersByteOffset :: a -> Int

        readVkMaxPerStageDescriptorSamplers ::
                                            Ptr a -> IO (VkMaxPerStageDescriptorSamplersMType a)

        writeVkMaxPerStageDescriptorSamplers ::
                                             Ptr a ->
                                               VkMaxPerStageDescriptorSamplersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxPerStageDescriptorSamplers'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPerStageDescriptorSamplers a

class HasVkMaxPerStageDescriptorStorageBuffers a where
        type VkMaxPerStageDescriptorStorageBuffersMType a :: *

        vkMaxPerStageDescriptorStorageBuffers ::
                                              a -> VkMaxPerStageDescriptorStorageBuffersMType a

        vkMaxPerStageDescriptorStorageBuffersByteOffset :: a -> Int

        readVkMaxPerStageDescriptorStorageBuffers ::
                                                  Ptr a ->
                                                    IO
                                                      (VkMaxPerStageDescriptorStorageBuffersMType a)

        writeVkMaxPerStageDescriptorStorageBuffers ::
                                                   Ptr a ->
                                                     VkMaxPerStageDescriptorStorageBuffersMType a ->
                                                       IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxPerStageDescriptorStorageBuffers'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPerStageDescriptorStorageBuffers a

class HasVkMaxPerStageDescriptorStorageImages a where
        type VkMaxPerStageDescriptorStorageImagesMType a :: *

        vkMaxPerStageDescriptorStorageImages ::
                                             a -> VkMaxPerStageDescriptorStorageImagesMType a

        vkMaxPerStageDescriptorStorageImagesByteOffset :: a -> Int

        readVkMaxPerStageDescriptorStorageImages ::
                                                 Ptr a ->
                                                   IO (VkMaxPerStageDescriptorStorageImagesMType a)

        writeVkMaxPerStageDescriptorStorageImages ::
                                                  Ptr a ->
                                                    VkMaxPerStageDescriptorStorageImagesMType a ->
                                                      IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxPerStageDescriptorStorageImages'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPerStageDescriptorStorageImages a

class HasVkMaxPerStageDescriptorUniformBuffers a where
        type VkMaxPerStageDescriptorUniformBuffersMType a :: *

        vkMaxPerStageDescriptorUniformBuffers ::
                                              a -> VkMaxPerStageDescriptorUniformBuffersMType a

        vkMaxPerStageDescriptorUniformBuffersByteOffset :: a -> Int

        readVkMaxPerStageDescriptorUniformBuffers ::
                                                  Ptr a ->
                                                    IO
                                                      (VkMaxPerStageDescriptorUniformBuffersMType a)

        writeVkMaxPerStageDescriptorUniformBuffers ::
                                                   Ptr a ->
                                                     VkMaxPerStageDescriptorUniformBuffersMType a ->
                                                       IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxPerStageDescriptorUniformBuffers'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPerStageDescriptorUniformBuffers a

class HasVkMaxPerStageResources a where
        type VkMaxPerStageResourcesMType a :: *

        vkMaxPerStageResources :: a -> VkMaxPerStageResourcesMType a

        vkMaxPerStageResourcesByteOffset :: a -> Int

        readVkMaxPerStageResources ::
                                   Ptr a -> IO (VkMaxPerStageResourcesMType a)

        writeVkMaxPerStageResources ::
                                    Ptr a -> VkMaxPerStageResourcesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxPerStageResources'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPerStageResources a

class HasVkMaxPipelineLayouts a where
        type VkMaxPipelineLayoutsMType a :: *

        vkMaxPipelineLayouts :: a -> VkMaxPipelineLayoutsMType a

        vkMaxPipelineLayoutsByteOffset :: a -> Int

        readVkMaxPipelineLayouts ::
                                 Ptr a -> IO (VkMaxPipelineLayoutsMType a)

        writeVkMaxPipelineLayouts ::
                                  Ptr a -> VkMaxPipelineLayoutsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxPipelineLayouts'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPipelineLayouts a

class HasVkMaxPushConstantsSize a where
        type VkMaxPushConstantsSizeMType a :: *

        vkMaxPushConstantsSize :: a -> VkMaxPushConstantsSizeMType a

        vkMaxPushConstantsSizeByteOffset :: a -> Int

        readVkMaxPushConstantsSize ::
                                   Ptr a -> IO (VkMaxPushConstantsSizeMType a)

        writeVkMaxPushConstantsSize ::
                                    Ptr a -> VkMaxPushConstantsSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxPushConstantsSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPushConstantsSize a

class HasVkMaxPushDescriptors a where
        type VkMaxPushDescriptorsMType a :: *

        vkMaxPushDescriptors :: a -> VkMaxPushDescriptorsMType a

        vkMaxPushDescriptorsByteOffset :: a -> Int

        readVkMaxPushDescriptors ::
                                 Ptr a -> IO (VkMaxPushDescriptorsMType a)

        writeVkMaxPushDescriptors ::
                                  Ptr a -> VkMaxPushDescriptorsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxPushDescriptors'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxPushDescriptors a

class HasVkMaxResourceSize a where
        type VkMaxResourceSizeMType a :: *

        vkMaxResourceSize :: a -> VkMaxResourceSizeMType a

        vkMaxResourceSizeByteOffset :: a -> Int

        readVkMaxResourceSize :: Ptr a -> IO (VkMaxResourceSizeMType a)

        writeVkMaxResourceSize ::
                               Ptr a -> VkMaxResourceSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxResourceSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxResourceSize a

class HasVkMaxSampleLocationGridSize a where
        type VkMaxSampleLocationGridSizeMType a :: *

        vkMaxSampleLocationGridSize ::
                                    a -> VkMaxSampleLocationGridSizeMType a

        vkMaxSampleLocationGridSizeByteOffset :: a -> Int

        readVkMaxSampleLocationGridSize ::
                                        Ptr a -> IO (VkMaxSampleLocationGridSizeMType a)

        writeVkMaxSampleLocationGridSize ::
                                         Ptr a -> VkMaxSampleLocationGridSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSampleLocationGridSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSampleLocationGridSize a

class HasVkMaxSampleMaskWords a where
        type VkMaxSampleMaskWordsMType a :: *

        vkMaxSampleMaskWords :: a -> VkMaxSampleMaskWordsMType a

        vkMaxSampleMaskWordsByteOffset :: a -> Int

        readVkMaxSampleMaskWords ::
                                 Ptr a -> IO (VkMaxSampleMaskWordsMType a)

        writeVkMaxSampleMaskWords ::
                                  Ptr a -> VkMaxSampleMaskWordsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSampleMaskWords'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSampleMaskWords a

class HasVkMaxSampledImagesPerDescriptor a where
        type VkMaxSampledImagesPerDescriptorMType a :: *

        vkMaxSampledImagesPerDescriptor ::
                                        a -> VkMaxSampledImagesPerDescriptorMType a

        vkMaxSampledImagesPerDescriptorByteOffset :: a -> Int

        readVkMaxSampledImagesPerDescriptor ::
                                            Ptr a -> IO (VkMaxSampledImagesPerDescriptorMType a)

        writeVkMaxSampledImagesPerDescriptor ::
                                             Ptr a ->
                                               VkMaxSampledImagesPerDescriptorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxSampledImagesPerDescriptor'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSampledImagesPerDescriptor a

class HasVkMaxSamplerAllocationCount a where
        type VkMaxSamplerAllocationCountMType a :: *

        vkMaxSamplerAllocationCount ::
                                    a -> VkMaxSamplerAllocationCountMType a

        vkMaxSamplerAllocationCountByteOffset :: a -> Int

        readVkMaxSamplerAllocationCount ::
                                        Ptr a -> IO (VkMaxSamplerAllocationCountMType a)

        writeVkMaxSamplerAllocationCount ::
                                         Ptr a -> VkMaxSamplerAllocationCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSamplerAllocationCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSamplerAllocationCount a

class HasVkMaxSamplerAnisotropy a where
        type VkMaxSamplerAnisotropyMType a :: *

        vkMaxSamplerAnisotropy :: a -> VkMaxSamplerAnisotropyMType a

        vkMaxSamplerAnisotropyByteOffset :: a -> Int

        readVkMaxSamplerAnisotropy ::
                                   Ptr a -> IO (VkMaxSamplerAnisotropyMType a)

        writeVkMaxSamplerAnisotropy ::
                                    Ptr a -> VkMaxSamplerAnisotropyMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSamplerAnisotropy'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSamplerAnisotropy a

class HasVkMaxSamplerLodBias a where
        type VkMaxSamplerLodBiasMType a :: *

        vkMaxSamplerLodBias :: a -> VkMaxSamplerLodBiasMType a

        vkMaxSamplerLodBiasByteOffset :: a -> Int

        readVkMaxSamplerLodBias :: Ptr a -> IO (VkMaxSamplerLodBiasMType a)

        writeVkMaxSamplerLodBias ::
                                 Ptr a -> VkMaxSamplerLodBiasMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSamplerLodBias'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSamplerLodBias a

class HasVkMaxSequencesCount a where
        type VkMaxSequencesCountMType a :: *

        vkMaxSequencesCount :: a -> VkMaxSequencesCountMType a

        vkMaxSequencesCountByteOffset :: a -> Int

        readVkMaxSequencesCount :: Ptr a -> IO (VkMaxSequencesCountMType a)

        writeVkMaxSequencesCount ::
                                 Ptr a -> VkMaxSequencesCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSequencesCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSequencesCount a

class HasVkMaxSets a where
        type VkMaxSetsMType a :: *

        vkMaxSets :: a -> VkMaxSetsMType a

        vkMaxSetsByteOffset :: a -> Int

        readVkMaxSets :: Ptr a -> IO (VkMaxSetsMType a)

        writeVkMaxSets :: Ptr a -> VkMaxSetsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSets'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSets a

class HasVkMaxSrcExtent a where
        type VkMaxSrcExtentMType a :: *

        vkMaxSrcExtent :: a -> VkMaxSrcExtentMType a

        vkMaxSrcExtentByteOffset :: a -> Int

        readVkMaxSrcExtent :: Ptr a -> IO (VkMaxSrcExtentMType a)

        writeVkMaxSrcExtent :: Ptr a -> VkMaxSrcExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSrcExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSrcExtent a

class HasVkMaxSrcPosition a where
        type VkMaxSrcPositionMType a :: *

        vkMaxSrcPosition :: a -> VkMaxSrcPositionMType a

        vkMaxSrcPositionByteOffset :: a -> Int

        readVkMaxSrcPosition :: Ptr a -> IO (VkMaxSrcPositionMType a)

        writeVkMaxSrcPosition :: Ptr a -> VkMaxSrcPositionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxSrcPosition'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxSrcPosition a

class HasVkMaxStorageBufferRange a where
        type VkMaxStorageBufferRangeMType a :: *

        vkMaxStorageBufferRange :: a -> VkMaxStorageBufferRangeMType a

        vkMaxStorageBufferRangeByteOffset :: a -> Int

        readVkMaxStorageBufferRange ::
                                    Ptr a -> IO (VkMaxStorageBufferRangeMType a)

        writeVkMaxStorageBufferRange ::
                                     Ptr a -> VkMaxStorageBufferRangeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxStorageBufferRange'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxStorageBufferRange a

class HasVkMaxStorageBuffersPerDescriptor a where
        type VkMaxStorageBuffersPerDescriptorMType a :: *

        vkMaxStorageBuffersPerDescriptor ::
                                         a -> VkMaxStorageBuffersPerDescriptorMType a

        vkMaxStorageBuffersPerDescriptorByteOffset :: a -> Int

        readVkMaxStorageBuffersPerDescriptor ::
                                             Ptr a -> IO (VkMaxStorageBuffersPerDescriptorMType a)

        writeVkMaxStorageBuffersPerDescriptor ::
                                              Ptr a ->
                                                VkMaxStorageBuffersPerDescriptorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxStorageBuffersPerDescriptor'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxStorageBuffersPerDescriptor a

class HasVkMaxStorageImagesPerDescriptor a where
        type VkMaxStorageImagesPerDescriptorMType a :: *

        vkMaxStorageImagesPerDescriptor ::
                                        a -> VkMaxStorageImagesPerDescriptorMType a

        vkMaxStorageImagesPerDescriptorByteOffset :: a -> Int

        readVkMaxStorageImagesPerDescriptor ::
                                            Ptr a -> IO (VkMaxStorageImagesPerDescriptorMType a)

        writeVkMaxStorageImagesPerDescriptor ::
                                             Ptr a ->
                                               VkMaxStorageImagesPerDescriptorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxStorageImagesPerDescriptor'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxStorageImagesPerDescriptor a

class HasVkMaxTessellationControlPerPatchOutputComponents a where
        type VkMaxTessellationControlPerPatchOutputComponentsMType a :: *

        vkMaxTessellationControlPerPatchOutputComponents ::
                                                         a ->
                                                           VkMaxTessellationControlPerPatchOutputComponentsMType
                                                             a

        vkMaxTessellationControlPerPatchOutputComponentsByteOffset ::
                                                                   a -> Int

        readVkMaxTessellationControlPerPatchOutputComponents ::
                                                             Ptr a ->
                                                               IO
                                                                 (VkMaxTessellationControlPerPatchOutputComponentsMType
                                                                    a)

        writeVkMaxTessellationControlPerPatchOutputComponents ::
                                                              Ptr a ->
                                                                VkMaxTessellationControlPerPatchOutputComponentsMType
                                                                  a
                                                                  -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxTessellationControlPerPatchOutputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTessellationControlPerPatchOutputComponents a

class HasVkMaxTessellationControlPerVertexInputComponents a where
        type VkMaxTessellationControlPerVertexInputComponentsMType a :: *

        vkMaxTessellationControlPerVertexInputComponents ::
                                                         a ->
                                                           VkMaxTessellationControlPerVertexInputComponentsMType
                                                             a

        vkMaxTessellationControlPerVertexInputComponentsByteOffset ::
                                                                   a -> Int

        readVkMaxTessellationControlPerVertexInputComponents ::
                                                             Ptr a ->
                                                               IO
                                                                 (VkMaxTessellationControlPerVertexInputComponentsMType
                                                                    a)

        writeVkMaxTessellationControlPerVertexInputComponents ::
                                                              Ptr a ->
                                                                VkMaxTessellationControlPerVertexInputComponentsMType
                                                                  a
                                                                  -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxTessellationControlPerVertexInputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTessellationControlPerVertexInputComponents a

class HasVkMaxTessellationControlPerVertexOutputComponents a where
        type VkMaxTessellationControlPerVertexOutputComponentsMType a :: *

        vkMaxTessellationControlPerVertexOutputComponents ::
                                                          a ->
                                                            VkMaxTessellationControlPerVertexOutputComponentsMType
                                                              a

        vkMaxTessellationControlPerVertexOutputComponentsByteOffset ::
                                                                    a -> Int

        readVkMaxTessellationControlPerVertexOutputComponents ::
                                                              Ptr a ->
                                                                IO
                                                                  (VkMaxTessellationControlPerVertexOutputComponentsMType
                                                                     a)

        writeVkMaxTessellationControlPerVertexOutputComponents ::
                                                               Ptr a ->
                                                                 VkMaxTessellationControlPerVertexOutputComponentsMType
                                                                   a
                                                                   -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxTessellationControlPerVertexOutputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTessellationControlPerVertexOutputComponents a

class HasVkMaxTessellationControlTotalOutputComponents a where
        type VkMaxTessellationControlTotalOutputComponentsMType a :: *

        vkMaxTessellationControlTotalOutputComponents ::
                                                      a ->
                                                        VkMaxTessellationControlTotalOutputComponentsMType
                                                          a

        vkMaxTessellationControlTotalOutputComponentsByteOffset :: a -> Int

        readVkMaxTessellationControlTotalOutputComponents ::
                                                          Ptr a ->
                                                            IO
                                                              (VkMaxTessellationControlTotalOutputComponentsMType
                                                                 a)

        writeVkMaxTessellationControlTotalOutputComponents ::
                                                           Ptr a ->
                                                             VkMaxTessellationControlTotalOutputComponentsMType
                                                               a
                                                               -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxTessellationControlTotalOutputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTessellationControlTotalOutputComponents a

class HasVkMaxTessellationEvaluationInputComponents a where
        type VkMaxTessellationEvaluationInputComponentsMType a :: *

        vkMaxTessellationEvaluationInputComponents ::
                                                   a ->
                                                     VkMaxTessellationEvaluationInputComponentsMType
                                                       a

        vkMaxTessellationEvaluationInputComponentsByteOffset :: a -> Int

        readVkMaxTessellationEvaluationInputComponents ::
                                                       Ptr a ->
                                                         IO
                                                           (VkMaxTessellationEvaluationInputComponentsMType
                                                              a)

        writeVkMaxTessellationEvaluationInputComponents ::
                                                        Ptr a ->
                                                          VkMaxTessellationEvaluationInputComponentsMType
                                                            a
                                                            -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxTessellationEvaluationInputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTessellationEvaluationInputComponents a

class HasVkMaxTessellationEvaluationOutputComponents a where
        type VkMaxTessellationEvaluationOutputComponentsMType a :: *

        vkMaxTessellationEvaluationOutputComponents ::
                                                    a ->
                                                      VkMaxTessellationEvaluationOutputComponentsMType
                                                        a

        vkMaxTessellationEvaluationOutputComponentsByteOffset :: a -> Int

        readVkMaxTessellationEvaluationOutputComponents ::
                                                        Ptr a ->
                                                          IO
                                                            (VkMaxTessellationEvaluationOutputComponentsMType
                                                               a)

        writeVkMaxTessellationEvaluationOutputComponents ::
                                                         Ptr a ->
                                                           VkMaxTessellationEvaluationOutputComponentsMType
                                                             a
                                                             -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxTessellationEvaluationOutputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTessellationEvaluationOutputComponents a

class HasVkMaxTessellationGenerationLevel a where
        type VkMaxTessellationGenerationLevelMType a :: *

        vkMaxTessellationGenerationLevel ::
                                         a -> VkMaxTessellationGenerationLevelMType a

        vkMaxTessellationGenerationLevelByteOffset :: a -> Int

        readVkMaxTessellationGenerationLevel ::
                                             Ptr a -> IO (VkMaxTessellationGenerationLevelMType a)

        writeVkMaxTessellationGenerationLevel ::
                                              Ptr a ->
                                                VkMaxTessellationGenerationLevelMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxTessellationGenerationLevel'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTessellationGenerationLevel a

class HasVkMaxTessellationPatchSize a where
        type VkMaxTessellationPatchSizeMType a :: *

        vkMaxTessellationPatchSize ::
                                   a -> VkMaxTessellationPatchSizeMType a

        vkMaxTessellationPatchSizeByteOffset :: a -> Int

        readVkMaxTessellationPatchSize ::
                                       Ptr a -> IO (VkMaxTessellationPatchSizeMType a)

        writeVkMaxTessellationPatchSize ::
                                        Ptr a -> VkMaxTessellationPatchSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxTessellationPatchSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTessellationPatchSize a

class HasVkMaxTexelBufferElements a where
        type VkMaxTexelBufferElementsMType a :: *

        vkMaxTexelBufferElements :: a -> VkMaxTexelBufferElementsMType a

        vkMaxTexelBufferElementsByteOffset :: a -> Int

        readVkMaxTexelBufferElements ::
                                     Ptr a -> IO (VkMaxTexelBufferElementsMType a)

        writeVkMaxTexelBufferElements ::
                                      Ptr a -> VkMaxTexelBufferElementsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxTexelBufferElements'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTexelBufferElements a

class HasVkMaxTexelGatherOffset a where
        type VkMaxTexelGatherOffsetMType a :: *

        vkMaxTexelGatherOffset :: a -> VkMaxTexelGatherOffsetMType a

        vkMaxTexelGatherOffsetByteOffset :: a -> Int

        readVkMaxTexelGatherOffset ::
                                   Ptr a -> IO (VkMaxTexelGatherOffsetMType a)

        writeVkMaxTexelGatherOffset ::
                                    Ptr a -> VkMaxTexelGatherOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxTexelGatherOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTexelGatherOffset a

class HasVkMaxTexelOffset a where
        type VkMaxTexelOffsetMType a :: *

        vkMaxTexelOffset :: a -> VkMaxTexelOffsetMType a

        vkMaxTexelOffsetByteOffset :: a -> Int

        readVkMaxTexelOffset :: Ptr a -> IO (VkMaxTexelOffsetMType a)

        writeVkMaxTexelOffset :: Ptr a -> VkMaxTexelOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxTexelOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxTexelOffset a

class HasVkMaxUniformBufferRange a where
        type VkMaxUniformBufferRangeMType a :: *

        vkMaxUniformBufferRange :: a -> VkMaxUniformBufferRangeMType a

        vkMaxUniformBufferRangeByteOffset :: a -> Int

        readVkMaxUniformBufferRange ::
                                    Ptr a -> IO (VkMaxUniformBufferRangeMType a)

        writeVkMaxUniformBufferRange ::
                                     Ptr a -> VkMaxUniformBufferRangeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxUniformBufferRange'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxUniformBufferRange a

class HasVkMaxUniformBuffersPerDescriptor a where
        type VkMaxUniformBuffersPerDescriptorMType a :: *

        vkMaxUniformBuffersPerDescriptor ::
                                         a -> VkMaxUniformBuffersPerDescriptorMType a

        vkMaxUniformBuffersPerDescriptorByteOffset :: a -> Int

        readVkMaxUniformBuffersPerDescriptor ::
                                             Ptr a -> IO (VkMaxUniformBuffersPerDescriptorMType a)

        writeVkMaxUniformBuffersPerDescriptor ::
                                              Ptr a ->
                                                VkMaxUniformBuffersPerDescriptorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxUniformBuffersPerDescriptor'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxUniformBuffersPerDescriptor a

class HasVkMaxVertexInputAttributeOffset a where
        type VkMaxVertexInputAttributeOffsetMType a :: *

        vkMaxVertexInputAttributeOffset ::
                                        a -> VkMaxVertexInputAttributeOffsetMType a

        vkMaxVertexInputAttributeOffsetByteOffset :: a -> Int

        readVkMaxVertexInputAttributeOffset ::
                                            Ptr a -> IO (VkMaxVertexInputAttributeOffsetMType a)

        writeVkMaxVertexInputAttributeOffset ::
                                             Ptr a ->
                                               VkMaxVertexInputAttributeOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'maxVertexInputAttributeOffset'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxVertexInputAttributeOffset a

class HasVkMaxVertexInputAttributes a where
        type VkMaxVertexInputAttributesMType a :: *

        vkMaxVertexInputAttributes ::
                                   a -> VkMaxVertexInputAttributesMType a

        vkMaxVertexInputAttributesByteOffset :: a -> Int

        readVkMaxVertexInputAttributes ::
                                       Ptr a -> IO (VkMaxVertexInputAttributesMType a)

        writeVkMaxVertexInputAttributes ::
                                        Ptr a -> VkMaxVertexInputAttributesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxVertexInputAttributes'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxVertexInputAttributes a

class HasVkMaxVertexInputBindingStride a where
        type VkMaxVertexInputBindingStrideMType a :: *

        vkMaxVertexInputBindingStride ::
                                      a -> VkMaxVertexInputBindingStrideMType a

        vkMaxVertexInputBindingStrideByteOffset :: a -> Int

        readVkMaxVertexInputBindingStride ::
                                          Ptr a -> IO (VkMaxVertexInputBindingStrideMType a)

        writeVkMaxVertexInputBindingStride ::
                                           Ptr a -> VkMaxVertexInputBindingStrideMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxVertexInputBindingStride'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxVertexInputBindingStride a

class HasVkMaxVertexInputBindings a where
        type VkMaxVertexInputBindingsMType a :: *

        vkMaxVertexInputBindings :: a -> VkMaxVertexInputBindingsMType a

        vkMaxVertexInputBindingsByteOffset :: a -> Int

        readVkMaxVertexInputBindings ::
                                     Ptr a -> IO (VkMaxVertexInputBindingsMType a)

        writeVkMaxVertexInputBindings ::
                                      Ptr a -> VkMaxVertexInputBindingsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxVertexInputBindings'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxVertexInputBindings a

class HasVkMaxVertexOutputComponents a where
        type VkMaxVertexOutputComponentsMType a :: *

        vkMaxVertexOutputComponents ::
                                    a -> VkMaxVertexOutputComponentsMType a

        vkMaxVertexOutputComponentsByteOffset :: a -> Int

        readVkMaxVertexOutputComponents ::
                                        Ptr a -> IO (VkMaxVertexOutputComponentsMType a)

        writeVkMaxVertexOutputComponents ::
                                         Ptr a -> VkMaxVertexOutputComponentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxVertexOutputComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxVertexOutputComponents a

class HasVkMaxViewportDimensionsArray a where
        type VkMaxViewportDimensionsArrayMType a :: *

        vkMaxViewportDimensionsArray ::
                                     a -> Int -> VkMaxViewportDimensionsArrayMType a

        vkMaxViewportDimensionsArrayByteOffset :: a -> Int

        readVkMaxViewportDimensionsArray ::
                                         Ptr a -> Int -> IO (VkMaxViewportDimensionsArrayMType a)

        writeVkMaxViewportDimensionsArray ::
                                          Ptr a ->
                                            Int -> VkMaxViewportDimensionsArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxViewportDimensions'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxViewportDimensionsArray a

class HasVkMaxViewports a where
        type VkMaxViewportsMType a :: *

        vkMaxViewports :: a -> VkMaxViewportsMType a

        vkMaxViewportsByteOffset :: a -> Int

        readVkMaxViewports :: Ptr a -> IO (VkMaxViewportsMType a)

        writeVkMaxViewports :: Ptr a -> VkMaxViewportsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'maxViewports'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMaxViewports a

class HasVkMemory a where
        type VkMemoryMType a :: *

        vkMemory :: a -> VkMemoryMType a

        vkMemoryByteOffset :: a -> Int

        readVkMemory :: Ptr a -> IO (VkMemoryMType a)

        writeVkMemory :: Ptr a -> VkMemoryMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memory'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemory a

class HasVkMemoryDeviceIndex a where
        type VkMemoryDeviceIndexMType a :: *

        vkMemoryDeviceIndex :: a -> VkMemoryDeviceIndexMType a

        vkMemoryDeviceIndexByteOffset :: a -> Int

        readVkMemoryDeviceIndex :: Ptr a -> IO (VkMemoryDeviceIndexMType a)

        writeVkMemoryDeviceIndex ::
                                 Ptr a -> VkMemoryDeviceIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryDeviceIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryDeviceIndex a

class HasVkMemoryHeapCount a where
        type VkMemoryHeapCountMType a :: *

        vkMemoryHeapCount :: a -> VkMemoryHeapCountMType a

        vkMemoryHeapCountByteOffset :: a -> Int

        readVkMemoryHeapCount :: Ptr a -> IO (VkMemoryHeapCountMType a)

        writeVkMemoryHeapCount ::
                               Ptr a -> VkMemoryHeapCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryHeapCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryHeapCount a

class HasVkMemoryHeapsArray a where
        type VkMemoryHeapsArrayMType a :: *

        vkMemoryHeapsArray :: a -> Int -> VkMemoryHeapsArrayMType a

        vkMemoryHeapsArrayByteOffset :: a -> Int

        readVkMemoryHeapsArray ::
                               Ptr a -> Int -> IO (VkMemoryHeapsArrayMType a)

        writeVkMemoryHeapsArray ::
                                Ptr a -> Int -> VkMemoryHeapsArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryHeaps'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryHeapsArray a

class HasVkMemoryOffset a where
        type VkMemoryOffsetMType a :: *

        vkMemoryOffset :: a -> VkMemoryOffsetMType a

        vkMemoryOffsetByteOffset :: a -> Int

        readVkMemoryOffset :: Ptr a -> IO (VkMemoryOffsetMType a)

        writeVkMemoryOffset :: Ptr a -> VkMemoryOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryOffset a

class HasVkMemoryProperties a where
        type VkMemoryPropertiesMType a :: *

        vkMemoryProperties :: a -> VkMemoryPropertiesMType a

        vkMemoryPropertiesByteOffset :: a -> Int

        readVkMemoryProperties :: Ptr a -> IO (VkMemoryPropertiesMType a)

        writeVkMemoryProperties ::
                                Ptr a -> VkMemoryPropertiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryProperties'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryProperties a

class HasVkMemoryRequirements a where
        type VkMemoryRequirementsMType a :: *

        vkMemoryRequirements :: a -> VkMemoryRequirementsMType a

        vkMemoryRequirementsByteOffset :: a -> Int

        readVkMemoryRequirements ::
                                 Ptr a -> IO (VkMemoryRequirementsMType a)

        writeVkMemoryRequirements ::
                                  Ptr a -> VkMemoryRequirementsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryRequirements'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryRequirements a

class HasVkMemoryTypeBits a where
        type VkMemoryTypeBitsMType a :: *

        vkMemoryTypeBits :: a -> VkMemoryTypeBitsMType a

        vkMemoryTypeBitsByteOffset :: a -> Int

        readVkMemoryTypeBits :: Ptr a -> IO (VkMemoryTypeBitsMType a)

        writeVkMemoryTypeBits :: Ptr a -> VkMemoryTypeBitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryTypeBits'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryTypeBits a

class HasVkMemoryTypeCount a where
        type VkMemoryTypeCountMType a :: *

        vkMemoryTypeCount :: a -> VkMemoryTypeCountMType a

        vkMemoryTypeCountByteOffset :: a -> Int

        readVkMemoryTypeCount :: Ptr a -> IO (VkMemoryTypeCountMType a)

        writeVkMemoryTypeCount ::
                               Ptr a -> VkMemoryTypeCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryTypeCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryTypeCount a

class HasVkMemoryTypeIndex a where
        type VkMemoryTypeIndexMType a :: *

        vkMemoryTypeIndex :: a -> VkMemoryTypeIndexMType a

        vkMemoryTypeIndexByteOffset :: a -> Int

        readVkMemoryTypeIndex :: Ptr a -> IO (VkMemoryTypeIndexMType a)

        writeVkMemoryTypeIndex ::
                               Ptr a -> VkMemoryTypeIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryTypeIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryTypeIndex a

class HasVkMemoryTypesArray a where
        type VkMemoryTypesArrayMType a :: *

        vkMemoryTypesArray :: a -> Int -> VkMemoryTypesArrayMType a

        vkMemoryTypesArrayByteOffset :: a -> Int

        readVkMemoryTypesArray ::
                               Ptr a -> Int -> IO (VkMemoryTypesArrayMType a)

        writeVkMemoryTypesArray ::
                                Ptr a -> Int -> VkMemoryTypesArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'memoryTypes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMemoryTypesArray a

class HasVkMinCommandsTokenBufferOffsetAlignment a where
        type VkMinCommandsTokenBufferOffsetAlignmentMType a :: *

        vkMinCommandsTokenBufferOffsetAlignment ::
                                                a -> VkMinCommandsTokenBufferOffsetAlignmentMType a

        vkMinCommandsTokenBufferOffsetAlignmentByteOffset :: a -> Int

        readVkMinCommandsTokenBufferOffsetAlignment ::
                                                    Ptr a ->
                                                      IO
                                                        (VkMinCommandsTokenBufferOffsetAlignmentMType
                                                           a)

        writeVkMinCommandsTokenBufferOffsetAlignment ::
                                                     Ptr a ->
                                                       VkMinCommandsTokenBufferOffsetAlignmentMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'minCommandsTokenBufferOffsetAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinCommandsTokenBufferOffsetAlignment a

class HasVkMinDepth a where
        type VkMinDepthMType a :: *

        vkMinDepth :: a -> VkMinDepthMType a

        vkMinDepthByteOffset :: a -> Int

        readVkMinDepth :: Ptr a -> IO (VkMinDepthMType a)

        writeVkMinDepth :: Ptr a -> VkMinDepthMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minDepth'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinDepth a

class HasVkMinDepthBounds a where
        type VkMinDepthBoundsMType a :: *

        vkMinDepthBounds :: a -> VkMinDepthBoundsMType a

        vkMinDepthBoundsByteOffset :: a -> Int

        readVkMinDepthBounds :: Ptr a -> IO (VkMinDepthBoundsMType a)

        writeVkMinDepthBounds :: Ptr a -> VkMinDepthBoundsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minDepthBounds'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinDepthBounds a

class HasVkMinDstExtent a where
        type VkMinDstExtentMType a :: *

        vkMinDstExtent :: a -> VkMinDstExtentMType a

        vkMinDstExtentByteOffset :: a -> Int

        readVkMinDstExtent :: Ptr a -> IO (VkMinDstExtentMType a)

        writeVkMinDstExtent :: Ptr a -> VkMinDstExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minDstExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinDstExtent a

class HasVkMinDstPosition a where
        type VkMinDstPositionMType a :: *

        vkMinDstPosition :: a -> VkMinDstPositionMType a

        vkMinDstPositionByteOffset :: a -> Int

        readVkMinDstPosition :: Ptr a -> IO (VkMinDstPositionMType a)

        writeVkMinDstPosition :: Ptr a -> VkMinDstPositionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minDstPosition'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinDstPosition a

class HasVkMinFilter a where
        type VkMinFilterMType a :: *

        vkMinFilter :: a -> VkMinFilterMType a

        vkMinFilterByteOffset :: a -> Int

        readVkMinFilter :: Ptr a -> IO (VkMinFilterMType a)

        writeVkMinFilter :: Ptr a -> VkMinFilterMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minFilter'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinFilter a

class HasVkMinImageCount a where
        type VkMinImageCountMType a :: *

        vkMinImageCount :: a -> VkMinImageCountMType a

        vkMinImageCountByteOffset :: a -> Int

        readVkMinImageCount :: Ptr a -> IO (VkMinImageCountMType a)

        writeVkMinImageCount :: Ptr a -> VkMinImageCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minImageCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinImageCount a

class HasVkMinImageExtent a where
        type VkMinImageExtentMType a :: *

        vkMinImageExtent :: a -> VkMinImageExtentMType a

        vkMinImageExtentByteOffset :: a -> Int

        readVkMinImageExtent :: Ptr a -> IO (VkMinImageExtentMType a)

        writeVkMinImageExtent :: Ptr a -> VkMinImageExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minImageExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinImageExtent a

class HasVkMinImageTransferGranularity a where
        type VkMinImageTransferGranularityMType a :: *

        vkMinImageTransferGranularity ::
                                      a -> VkMinImageTransferGranularityMType a

        vkMinImageTransferGranularityByteOffset :: a -> Int

        readVkMinImageTransferGranularity ::
                                          Ptr a -> IO (VkMinImageTransferGranularityMType a)

        writeVkMinImageTransferGranularity ::
                                           Ptr a -> VkMinImageTransferGranularityMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minImageTransferGranularity'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinImageTransferGranularity a

class HasVkMinImportedHostPointerAlignment a where
        type VkMinImportedHostPointerAlignmentMType a :: *

        vkMinImportedHostPointerAlignment ::
                                          a -> VkMinImportedHostPointerAlignmentMType a

        vkMinImportedHostPointerAlignmentByteOffset :: a -> Int

        readVkMinImportedHostPointerAlignment ::
                                              Ptr a -> IO (VkMinImportedHostPointerAlignmentMType a)

        writeVkMinImportedHostPointerAlignment ::
                                               Ptr a ->
                                                 VkMinImportedHostPointerAlignmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'minImportedHostPointerAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinImportedHostPointerAlignment a

class HasVkMinInterpolationOffset a where
        type VkMinInterpolationOffsetMType a :: *

        vkMinInterpolationOffset :: a -> VkMinInterpolationOffsetMType a

        vkMinInterpolationOffsetByteOffset :: a -> Int

        readVkMinInterpolationOffset ::
                                     Ptr a -> IO (VkMinInterpolationOffsetMType a)

        writeVkMinInterpolationOffset ::
                                      Ptr a -> VkMinInterpolationOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minInterpolationOffset'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinInterpolationOffset a

class HasVkMinLod a where
        type VkMinLodMType a :: *

        vkMinLod :: a -> VkMinLodMType a

        vkMinLodByteOffset :: a -> Int

        readVkMinLod :: Ptr a -> IO (VkMinLodMType a)

        writeVkMinLod :: Ptr a -> VkMinLodMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minLod'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinLod a

class HasVkMinLuminance a where
        type VkMinLuminanceMType a :: *

        vkMinLuminance :: a -> VkMinLuminanceMType a

        vkMinLuminanceByteOffset :: a -> Int

        readVkMinLuminance :: Ptr a -> IO (VkMinLuminanceMType a)

        writeVkMinLuminance :: Ptr a -> VkMinLuminanceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minLuminance'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinLuminance a

class HasVkMinMemoryMapAlignment a where
        type VkMinMemoryMapAlignmentMType a :: *

        vkMinMemoryMapAlignment :: a -> VkMinMemoryMapAlignmentMType a

        vkMinMemoryMapAlignmentByteOffset :: a -> Int

        readVkMinMemoryMapAlignment ::
                                    Ptr a -> IO (VkMinMemoryMapAlignmentMType a)

        writeVkMinMemoryMapAlignment ::
                                     Ptr a -> VkMinMemoryMapAlignmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minMemoryMapAlignment'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinMemoryMapAlignment a

class HasVkMinSampleShading a where
        type VkMinSampleShadingMType a :: *

        vkMinSampleShading :: a -> VkMinSampleShadingMType a

        vkMinSampleShadingByteOffset :: a -> Int

        readVkMinSampleShading :: Ptr a -> IO (VkMinSampleShadingMType a)

        writeVkMinSampleShading ::
                                Ptr a -> VkMinSampleShadingMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minSampleShading'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinSampleShading a

class HasVkMinSequenceCountBufferOffsetAlignment a where
        type VkMinSequenceCountBufferOffsetAlignmentMType a :: *

        vkMinSequenceCountBufferOffsetAlignment ::
                                                a -> VkMinSequenceCountBufferOffsetAlignmentMType a

        vkMinSequenceCountBufferOffsetAlignmentByteOffset :: a -> Int

        readVkMinSequenceCountBufferOffsetAlignment ::
                                                    Ptr a ->
                                                      IO
                                                        (VkMinSequenceCountBufferOffsetAlignmentMType
                                                           a)

        writeVkMinSequenceCountBufferOffsetAlignment ::
                                                     Ptr a ->
                                                       VkMinSequenceCountBufferOffsetAlignmentMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'minSequenceCountBufferOffsetAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinSequenceCountBufferOffsetAlignment a

class HasVkMinSequenceIndexBufferOffsetAlignment a where
        type VkMinSequenceIndexBufferOffsetAlignmentMType a :: *

        vkMinSequenceIndexBufferOffsetAlignment ::
                                                a -> VkMinSequenceIndexBufferOffsetAlignmentMType a

        vkMinSequenceIndexBufferOffsetAlignmentByteOffset :: a -> Int

        readVkMinSequenceIndexBufferOffsetAlignment ::
                                                    Ptr a ->
                                                      IO
                                                        (VkMinSequenceIndexBufferOffsetAlignmentMType
                                                           a)

        writeVkMinSequenceIndexBufferOffsetAlignment ::
                                                     Ptr a ->
                                                       VkMinSequenceIndexBufferOffsetAlignmentMType
                                                         a
                                                         -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'minSequenceIndexBufferOffsetAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinSequenceIndexBufferOffsetAlignment a

class HasVkMinSrcExtent a where
        type VkMinSrcExtentMType a :: *

        vkMinSrcExtent :: a -> VkMinSrcExtentMType a

        vkMinSrcExtentByteOffset :: a -> Int

        readVkMinSrcExtent :: Ptr a -> IO (VkMinSrcExtentMType a)

        writeVkMinSrcExtent :: Ptr a -> VkMinSrcExtentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minSrcExtent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinSrcExtent a

class HasVkMinSrcPosition a where
        type VkMinSrcPositionMType a :: *

        vkMinSrcPosition :: a -> VkMinSrcPositionMType a

        vkMinSrcPositionByteOffset :: a -> Int

        readVkMinSrcPosition :: Ptr a -> IO (VkMinSrcPositionMType a)

        writeVkMinSrcPosition :: Ptr a -> VkMinSrcPositionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minSrcPosition'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinSrcPosition a

class HasVkMinStorageBufferOffsetAlignment a where
        type VkMinStorageBufferOffsetAlignmentMType a :: *

        vkMinStorageBufferOffsetAlignment ::
                                          a -> VkMinStorageBufferOffsetAlignmentMType a

        vkMinStorageBufferOffsetAlignmentByteOffset :: a -> Int

        readVkMinStorageBufferOffsetAlignment ::
                                              Ptr a -> IO (VkMinStorageBufferOffsetAlignmentMType a)

        writeVkMinStorageBufferOffsetAlignment ::
                                               Ptr a ->
                                                 VkMinStorageBufferOffsetAlignmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'minStorageBufferOffsetAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinStorageBufferOffsetAlignment a

class HasVkMinTexelBufferOffsetAlignment a where
        type VkMinTexelBufferOffsetAlignmentMType a :: *

        vkMinTexelBufferOffsetAlignment ::
                                        a -> VkMinTexelBufferOffsetAlignmentMType a

        vkMinTexelBufferOffsetAlignmentByteOffset :: a -> Int

        readVkMinTexelBufferOffsetAlignment ::
                                            Ptr a -> IO (VkMinTexelBufferOffsetAlignmentMType a)

        writeVkMinTexelBufferOffsetAlignment ::
                                             Ptr a ->
                                               VkMinTexelBufferOffsetAlignmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'minTexelBufferOffsetAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinTexelBufferOffsetAlignment a

class HasVkMinTexelGatherOffset a where
        type VkMinTexelGatherOffsetMType a :: *

        vkMinTexelGatherOffset :: a -> VkMinTexelGatherOffsetMType a

        vkMinTexelGatherOffsetByteOffset :: a -> Int

        readVkMinTexelGatherOffset ::
                                   Ptr a -> IO (VkMinTexelGatherOffsetMType a)

        writeVkMinTexelGatherOffset ::
                                    Ptr a -> VkMinTexelGatherOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minTexelGatherOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinTexelGatherOffset a

class HasVkMinTexelOffset a where
        type VkMinTexelOffsetMType a :: *

        vkMinTexelOffset :: a -> VkMinTexelOffsetMType a

        vkMinTexelOffsetByteOffset :: a -> Int

        readVkMinTexelOffset :: Ptr a -> IO (VkMinTexelOffsetMType a)

        writeVkMinTexelOffset :: Ptr a -> VkMinTexelOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'minTexelOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinTexelOffset a

class HasVkMinUniformBufferOffsetAlignment a where
        type VkMinUniformBufferOffsetAlignmentMType a :: *

        vkMinUniformBufferOffsetAlignment ::
                                          a -> VkMinUniformBufferOffsetAlignmentMType a

        vkMinUniformBufferOffsetAlignmentByteOffset :: a -> Int

        readVkMinUniformBufferOffsetAlignment ::
                                              Ptr a -> IO (VkMinUniformBufferOffsetAlignmentMType a)

        writeVkMinUniformBufferOffsetAlignment ::
                                               Ptr a ->
                                                 VkMinUniformBufferOffsetAlignmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'minUniformBufferOffsetAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMinUniformBufferOffsetAlignment a

class HasVkMipLevel a where
        type VkMipLevelMType a :: *

        vkMipLevel :: a -> VkMipLevelMType a

        vkMipLevelByteOffset :: a -> Int

        readVkMipLevel :: Ptr a -> IO (VkMipLevelMType a)

        writeVkMipLevel :: Ptr a -> VkMipLevelMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'mipLevel'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMipLevel a

class HasVkMipLevels a where
        type VkMipLevelsMType a :: *

        vkMipLevels :: a -> VkMipLevelsMType a

        vkMipLevelsByteOffset :: a -> Int

        readVkMipLevels :: Ptr a -> IO (VkMipLevelsMType a)

        writeVkMipLevels :: Ptr a -> VkMipLevelsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'mipLevels'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMipLevels a

class HasVkMipLodBias a where
        type VkMipLodBiasMType a :: *

        vkMipLodBias :: a -> VkMipLodBiasMType a

        vkMipLodBiasByteOffset :: a -> Int

        readVkMipLodBias :: Ptr a -> IO (VkMipLodBiasMType a)

        writeVkMipLodBias :: Ptr a -> VkMipLodBiasMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'mipLodBias'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMipLodBias a

class HasVkMipmapMode a where
        type VkMipmapModeMType a :: *

        vkMipmapMode :: a -> VkMipmapModeMType a

        vkMipmapModeByteOffset :: a -> Int

        readVkMipmapMode :: Ptr a -> IO (VkMipmapModeMType a)

        writeVkMipmapMode :: Ptr a -> VkMipmapModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'mipmapMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMipmapMode a

class HasVkMipmapPrecisionBits a where
        type VkMipmapPrecisionBitsMType a :: *

        vkMipmapPrecisionBits :: a -> VkMipmapPrecisionBitsMType a

        vkMipmapPrecisionBitsByteOffset :: a -> Int

        readVkMipmapPrecisionBits ::
                                  Ptr a -> IO (VkMipmapPrecisionBitsMType a)

        writeVkMipmapPrecisionBits ::
                                   Ptr a -> VkMipmapPrecisionBitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'mipmapPrecisionBits'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMipmapPrecisionBits a

class HasVkMirSurface a where
        type VkMirSurfaceMType a :: *

        vkMirSurface :: a -> VkMirSurfaceMType a

        vkMirSurfaceByteOffset :: a -> Int

        readVkMirSurface :: Ptr a -> IO (VkMirSurfaceMType a)

        writeVkMirSurface :: Ptr a -> VkMirSurfaceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'mirSurface'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMirSurface a

class HasVkMode a where
        type VkModeMType a :: *

        vkMode :: a -> VkModeMType a

        vkModeByteOffset :: a -> Int

        readVkMode :: Ptr a -> IO (VkModeMType a)

        writeVkMode :: Ptr a -> VkModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'mode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMode a

class HasVkModes a where
        type VkModesMType a :: *

        vkModes :: a -> VkModesMType a

        vkModesByteOffset :: a -> Int

        readVkModes :: Ptr a -> IO (VkModesMType a)

        writeVkModes :: Ptr a -> VkModesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'modes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkModes a

class HasVkModule a where
        type VkModuleMType a :: *

        vkModule :: a -> VkModuleMType a

        vkModuleByteOffset :: a -> Int

        readVkModule :: Ptr a -> IO (VkModuleMType a)

        writeVkModule :: Ptr a -> VkModuleMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'module'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkModule a

class HasVkMultiDrawIndirect a where
        type VkMultiDrawIndirectMType a :: *

        vkMultiDrawIndirect :: a -> VkMultiDrawIndirectMType a

        vkMultiDrawIndirectByteOffset :: a -> Int

        readVkMultiDrawIndirect :: Ptr a -> IO (VkMultiDrawIndirectMType a)

        writeVkMultiDrawIndirect ::
                                 Ptr a -> VkMultiDrawIndirectMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'multiDrawIndirect'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMultiDrawIndirect a

class HasVkMultiViewport a where
        type VkMultiViewportMType a :: *

        vkMultiViewport :: a -> VkMultiViewportMType a

        vkMultiViewportByteOffset :: a -> Int

        readVkMultiViewport :: Ptr a -> IO (VkMultiViewportMType a)

        writeVkMultiViewport :: Ptr a -> VkMultiViewportMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'multiViewport'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMultiViewport a

class HasVkMultiview a where
        type VkMultiviewMType a :: *

        vkMultiview :: a -> VkMultiviewMType a

        vkMultiviewByteOffset :: a -> Int

        readVkMultiview :: Ptr a -> IO (VkMultiviewMType a)

        writeVkMultiview :: Ptr a -> VkMultiviewMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'multiview'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMultiview a

class HasVkMultiviewGeometryShader a where
        type VkMultiviewGeometryShaderMType a :: *

        vkMultiviewGeometryShader :: a -> VkMultiviewGeometryShaderMType a

        vkMultiviewGeometryShaderByteOffset :: a -> Int

        readVkMultiviewGeometryShader ::
                                      Ptr a -> IO (VkMultiviewGeometryShaderMType a)

        writeVkMultiviewGeometryShader ::
                                       Ptr a -> VkMultiviewGeometryShaderMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'multiviewGeometryShader'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMultiviewGeometryShader a

class HasVkMultiviewTessellationShader a where
        type VkMultiviewTessellationShaderMType a :: *

        vkMultiviewTessellationShader ::
                                      a -> VkMultiviewTessellationShaderMType a

        vkMultiviewTessellationShaderByteOffset :: a -> Int

        readVkMultiviewTessellationShader ::
                                          Ptr a -> IO (VkMultiviewTessellationShaderMType a)

        writeVkMultiviewTessellationShader ::
                                           Ptr a -> VkMultiviewTessellationShaderMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'multiviewTessellationShader'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkMultiviewTessellationShader a

class HasVkName a where
        type VkNameMType a :: *

        vkName :: a -> VkNameMType a

        vkNameByteOffset :: a -> Int

        readVkName :: Ptr a -> IO (VkNameMType a)

        writeVkName :: Ptr a -> VkNameMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'name'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkName a

class HasVkNewLayout a where
        type VkNewLayoutMType a :: *

        vkNewLayout :: a -> VkNewLayoutMType a

        vkNewLayoutByteOffset :: a -> Int

        readVkNewLayout :: Ptr a -> IO (VkNewLayoutMType a)

        writeVkNewLayout :: Ptr a -> VkNewLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'newLayout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkNewLayout a

class HasVkNonCoherentAtomSize a where
        type VkNonCoherentAtomSizeMType a :: *

        vkNonCoherentAtomSize :: a -> VkNonCoherentAtomSizeMType a

        vkNonCoherentAtomSizeByteOffset :: a -> Int

        readVkNonCoherentAtomSize ::
                                  Ptr a -> IO (VkNonCoherentAtomSizeMType a)

        writeVkNonCoherentAtomSize ::
                                   Ptr a -> VkNonCoherentAtomSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'nonCoherentAtomSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkNonCoherentAtomSize a

class HasVkNumAvailableSgprs a where
        type VkNumAvailableSgprsMType a :: *

        vkNumAvailableSgprs :: a -> VkNumAvailableSgprsMType a

        vkNumAvailableSgprsByteOffset :: a -> Int

        readVkNumAvailableSgprs :: Ptr a -> IO (VkNumAvailableSgprsMType a)

        writeVkNumAvailableSgprs ::
                                 Ptr a -> VkNumAvailableSgprsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'numAvailableSgprs'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkNumAvailableSgprs a

class HasVkNumAvailableVgprs a where
        type VkNumAvailableVgprsMType a :: *

        vkNumAvailableVgprs :: a -> VkNumAvailableVgprsMType a

        vkNumAvailableVgprsByteOffset :: a -> Int

        readVkNumAvailableVgprs :: Ptr a -> IO (VkNumAvailableVgprsMType a)

        writeVkNumAvailableVgprs ::
                                 Ptr a -> VkNumAvailableVgprsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'numAvailableVgprs'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkNumAvailableVgprs a

class HasVkNumPhysicalSgprs a where
        type VkNumPhysicalSgprsMType a :: *

        vkNumPhysicalSgprs :: a -> VkNumPhysicalSgprsMType a

        vkNumPhysicalSgprsByteOffset :: a -> Int

        readVkNumPhysicalSgprs :: Ptr a -> IO (VkNumPhysicalSgprsMType a)

        writeVkNumPhysicalSgprs ::
                                Ptr a -> VkNumPhysicalSgprsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'numPhysicalSgprs'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkNumPhysicalSgprs a

class HasVkNumPhysicalVgprs a where
        type VkNumPhysicalVgprsMType a :: *

        vkNumPhysicalVgprs :: a -> VkNumPhysicalVgprsMType a

        vkNumPhysicalVgprsByteOffset :: a -> Int

        readVkNumPhysicalVgprs :: Ptr a -> IO (VkNumPhysicalVgprsMType a)

        writeVkNumPhysicalVgprs ::
                                Ptr a -> VkNumPhysicalVgprsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'numPhysicalVgprs'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkNumPhysicalVgprs a

class HasVkNumUsedSgprs a where
        type VkNumUsedSgprsMType a :: *

        vkNumUsedSgprs :: a -> VkNumUsedSgprsMType a

        vkNumUsedSgprsByteOffset :: a -> Int

        readVkNumUsedSgprs :: Ptr a -> IO (VkNumUsedSgprsMType a)

        writeVkNumUsedSgprs :: Ptr a -> VkNumUsedSgprsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'numUsedSgprs'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkNumUsedSgprs a

class HasVkNumUsedVgprs a where
        type VkNumUsedVgprsMType a :: *

        vkNumUsedVgprs :: a -> VkNumUsedVgprsMType a

        vkNumUsedVgprsByteOffset :: a -> Int

        readVkNumUsedVgprs :: Ptr a -> IO (VkNumUsedVgprsMType a)

        writeVkNumUsedVgprs :: Ptr a -> VkNumUsedVgprsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'numUsedVgprs'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkNumUsedVgprs a

class HasVkObject a where
        type VkObjectMType a :: *

        vkObject :: a -> VkObjectMType a

        vkObjectByteOffset :: a -> Int

        readVkObject :: Ptr a -> IO (VkObjectMType a)

        writeVkObject :: Ptr a -> VkObjectMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'object'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkObject a

class HasVkObjectCount a where
        type VkObjectCountMType a :: *

        vkObjectCount :: a -> VkObjectCountMType a

        vkObjectCountByteOffset :: a -> Int

        readVkObjectCount :: Ptr a -> IO (VkObjectCountMType a)

        writeVkObjectCount :: Ptr a -> VkObjectCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'objectCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkObjectCount a

class HasVkObjectTable a where
        type VkObjectTableMType a :: *

        vkObjectTable :: a -> VkObjectTableMType a

        vkObjectTableByteOffset :: a -> Int

        readVkObjectTable :: Ptr a -> IO (VkObjectTableMType a)

        writeVkObjectTable :: Ptr a -> VkObjectTableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'objectTable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkObjectTable a

class HasVkObjectType a where
        type VkObjectTypeMType a :: *

        vkObjectType :: a -> VkObjectTypeMType a

        vkObjectTypeByteOffset :: a -> Int

        readVkObjectType :: Ptr a -> IO (VkObjectTypeMType a)

        writeVkObjectType :: Ptr a -> VkObjectTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'objectType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkObjectType a

class HasVkOcclusionQueryEnable a where
        type VkOcclusionQueryEnableMType a :: *

        vkOcclusionQueryEnable :: a -> VkOcclusionQueryEnableMType a

        vkOcclusionQueryEnableByteOffset :: a -> Int

        readVkOcclusionQueryEnable ::
                                   Ptr a -> IO (VkOcclusionQueryEnableMType a)

        writeVkOcclusionQueryEnable ::
                                    Ptr a -> VkOcclusionQueryEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'occlusionQueryEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkOcclusionQueryEnable a

class HasVkOcclusionQueryPrecise a where
        type VkOcclusionQueryPreciseMType a :: *

        vkOcclusionQueryPrecise :: a -> VkOcclusionQueryPreciseMType a

        vkOcclusionQueryPreciseByteOffset :: a -> Int

        readVkOcclusionQueryPrecise ::
                                    Ptr a -> IO (VkOcclusionQueryPreciseMType a)

        writeVkOcclusionQueryPrecise ::
                                     Ptr a -> VkOcclusionQueryPreciseMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'occlusionQueryPrecise'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkOcclusionQueryPrecise a

class HasVkOffset a where
        type VkOffsetMType a :: *

        vkOffset :: a -> VkOffsetMType a

        vkOffsetByteOffset :: a -> Int

        readVkOffset :: Ptr a -> IO (VkOffsetMType a)

        writeVkOffset :: Ptr a -> VkOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'offset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkOffset a

class HasVkOldLayout a where
        type VkOldLayoutMType a :: *

        vkOldLayout :: a -> VkOldLayoutMType a

        vkOldLayoutByteOffset :: a -> Int

        readVkOldLayout :: Ptr a -> IO (VkOldLayoutMType a)

        writeVkOldLayout :: Ptr a -> VkOldLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'oldLayout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkOldLayout a

class HasVkOldSwapchain a where
        type VkOldSwapchainMType a :: *

        vkOldSwapchain :: a -> VkOldSwapchainMType a

        vkOldSwapchainByteOffset :: a -> Int

        readVkOldSwapchain :: Ptr a -> IO (VkOldSwapchainMType a)

        writeVkOldSwapchain :: Ptr a -> VkOldSwapchainMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'oldSwapchain'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkOldSwapchain a

class HasVkOptimalBufferCopyOffsetAlignment a where
        type VkOptimalBufferCopyOffsetAlignmentMType a :: *

        vkOptimalBufferCopyOffsetAlignment ::
                                           a -> VkOptimalBufferCopyOffsetAlignmentMType a

        vkOptimalBufferCopyOffsetAlignmentByteOffset :: a -> Int

        readVkOptimalBufferCopyOffsetAlignment ::
                                               Ptr a ->
                                                 IO (VkOptimalBufferCopyOffsetAlignmentMType a)

        writeVkOptimalBufferCopyOffsetAlignment ::
                                                Ptr a ->
                                                  VkOptimalBufferCopyOffsetAlignmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'optimalBufferCopyOffsetAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkOptimalBufferCopyOffsetAlignment a

class HasVkOptimalBufferCopyRowPitchAlignment a where
        type VkOptimalBufferCopyRowPitchAlignmentMType a :: *

        vkOptimalBufferCopyRowPitchAlignment ::
                                             a -> VkOptimalBufferCopyRowPitchAlignmentMType a

        vkOptimalBufferCopyRowPitchAlignmentByteOffset :: a -> Int

        readVkOptimalBufferCopyRowPitchAlignment ::
                                                 Ptr a ->
                                                   IO (VkOptimalBufferCopyRowPitchAlignmentMType a)

        writeVkOptimalBufferCopyRowPitchAlignment ::
                                                  Ptr a ->
                                                    VkOptimalBufferCopyRowPitchAlignmentMType a ->
                                                      IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'optimalBufferCopyRowPitchAlignment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkOptimalBufferCopyRowPitchAlignment a

class HasVkOptimalTilingFeatures a where
        type VkOptimalTilingFeaturesMType a :: *

        vkOptimalTilingFeatures :: a -> VkOptimalTilingFeaturesMType a

        vkOptimalTilingFeaturesByteOffset :: a -> Int

        readVkOptimalTilingFeatures ::
                                    Ptr a -> IO (VkOptimalTilingFeaturesMType a)

        writeVkOptimalTilingFeatures ::
                                     Ptr a -> VkOptimalTilingFeaturesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'optimalTilingFeatures'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkOptimalTilingFeatures a

class HasVkPAcquireKeys a where
        type VkPAcquireKeysMType a :: *

        vkPAcquireKeys :: a -> VkPAcquireKeysMType a

        vkPAcquireKeysByteOffset :: a -> Int

        readVkPAcquireKeys :: Ptr a -> IO (VkPAcquireKeysMType a)

        writeVkPAcquireKeys :: Ptr a -> VkPAcquireKeysMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pAcquireKeys'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPAcquireKeys a

class HasVkPAcquireSyncs a where
        type VkPAcquireSyncsMType a :: *

        vkPAcquireSyncs :: a -> VkPAcquireSyncsMType a

        vkPAcquireSyncsByteOffset :: a -> Int

        readVkPAcquireSyncs :: Ptr a -> IO (VkPAcquireSyncsMType a)

        writeVkPAcquireSyncs :: Ptr a -> VkPAcquireSyncsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pAcquireSyncs'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPAcquireSyncs a

class HasVkPAcquireTimeoutMilliseconds a where
        type VkPAcquireTimeoutMillisecondsMType a :: *

        vkPAcquireTimeoutMilliseconds ::
                                      a -> VkPAcquireTimeoutMillisecondsMType a

        vkPAcquireTimeoutMillisecondsByteOffset :: a -> Int

        readVkPAcquireTimeoutMilliseconds ::
                                          Ptr a -> IO (VkPAcquireTimeoutMillisecondsMType a)

        writeVkPAcquireTimeoutMilliseconds ::
                                           Ptr a -> VkPAcquireTimeoutMillisecondsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pAcquireTimeoutMilliseconds'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPAcquireTimeoutMilliseconds a

class HasVkPAcquireTimeouts a where
        type VkPAcquireTimeoutsMType a :: *

        vkPAcquireTimeouts :: a -> VkPAcquireTimeoutsMType a

        vkPAcquireTimeoutsByteOffset :: a -> Int

        readVkPAcquireTimeouts :: Ptr a -> IO (VkPAcquireTimeoutsMType a)

        writeVkPAcquireTimeouts ::
                                Ptr a -> VkPAcquireTimeoutsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pAcquireTimeouts'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPAcquireTimeouts a

class HasVkPApplicationInfo a where
        type VkPApplicationInfoMType a :: *

        vkPApplicationInfo :: a -> VkPApplicationInfoMType a

        vkPApplicationInfoByteOffset :: a -> Int

        readVkPApplicationInfo :: Ptr a -> IO (VkPApplicationInfoMType a)

        writeVkPApplicationInfo ::
                                Ptr a -> VkPApplicationInfoMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pApplicationInfo'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPApplicationInfo a

class HasVkPApplicationName a where
        type VkPApplicationNameMType a :: *

        vkPApplicationName :: a -> VkPApplicationNameMType a

        vkPApplicationNameByteOffset :: a -> Int

        readVkPApplicationName :: Ptr a -> IO (VkPApplicationNameMType a)

        writeVkPApplicationName ::
                                Ptr a -> VkPApplicationNameMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pApplicationName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPApplicationName a

class HasVkPAspectReferences a where
        type VkPAspectReferencesMType a :: *

        vkPAspectReferences :: a -> VkPAspectReferencesMType a

        vkPAspectReferencesByteOffset :: a -> Int

        readVkPAspectReferences :: Ptr a -> IO (VkPAspectReferencesMType a)

        writeVkPAspectReferences ::
                                 Ptr a -> VkPAspectReferencesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pAspectReferences'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPAspectReferences a

class HasVkPAttachmentInitialSampleLocations a where
        type VkPAttachmentInitialSampleLocationsMType a :: *

        vkPAttachmentInitialSampleLocations ::
                                            a -> VkPAttachmentInitialSampleLocationsMType a

        vkPAttachmentInitialSampleLocationsByteOffset :: a -> Int

        readVkPAttachmentInitialSampleLocations ::
                                                Ptr a ->
                                                  IO (VkPAttachmentInitialSampleLocationsMType a)

        writeVkPAttachmentInitialSampleLocations ::
                                                 Ptr a ->
                                                   VkPAttachmentInitialSampleLocationsMType a ->
                                                     IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'pAttachmentInitialSampleLocations'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPAttachmentInitialSampleLocations a

class HasVkPAttachments a where
        type VkPAttachmentsMType a :: *

        vkPAttachments :: a -> VkPAttachmentsMType a

        vkPAttachmentsByteOffset :: a -> Int

        readVkPAttachments :: Ptr a -> IO (VkPAttachmentsMType a)

        writeVkPAttachments :: Ptr a -> VkPAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pAttachments'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPAttachments a

class HasVkPAttributes a where
        type VkPAttributesMType a :: *

        vkPAttributes :: a -> VkPAttributesMType a

        vkPAttributesByteOffset :: a -> Int

        readVkPAttributes :: Ptr a -> IO (VkPAttributesMType a)

        writeVkPAttributes :: Ptr a -> VkPAttributesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pAttributes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPAttributes a

class HasVkPBindings a where
        type VkPBindingsMType a :: *

        vkPBindings :: a -> VkPBindingsMType a

        vkPBindingsByteOffset :: a -> Int

        readVkPBindings :: Ptr a -> IO (VkPBindingsMType a)

        writeVkPBindings :: Ptr a -> VkPBindingsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pBindings'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPBindings a

class HasVkPBinds a where
        type VkPBindsMType a :: *

        vkPBinds :: a -> VkPBindsMType a

        vkPBindsByteOffset :: a -> Int

        readVkPBinds :: Ptr a -> IO (VkPBindsMType a)

        writeVkPBinds :: Ptr a -> VkPBindsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pBinds'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPBinds a

class HasVkPBufferBinds a where
        type VkPBufferBindsMType a :: *

        vkPBufferBinds :: a -> VkPBufferBindsMType a

        vkPBufferBindsByteOffset :: a -> Int

        readVkPBufferBinds :: Ptr a -> IO (VkPBufferBindsMType a)

        writeVkPBufferBinds :: Ptr a -> VkPBufferBindsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pBufferBinds'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPBufferBinds a

class HasVkPBufferInfo a where
        type VkPBufferInfoMType a :: *

        vkPBufferInfo :: a -> VkPBufferInfoMType a

        vkPBufferInfoByteOffset :: a -> Int

        readVkPBufferInfo :: Ptr a -> IO (VkPBufferInfoMType a)

        writeVkPBufferInfo :: Ptr a -> VkPBufferInfoMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pBufferInfo'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPBufferInfo a

class HasVkPClearValues a where
        type VkPClearValuesMType a :: *

        vkPClearValues :: a -> VkPClearValuesMType a

        vkPClearValuesByteOffset :: a -> Int

        readVkPClearValues :: Ptr a -> IO (VkPClearValuesMType a)

        writeVkPClearValues :: Ptr a -> VkPClearValuesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pClearValues'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPClearValues a

class HasVkPCode a where
        type VkPCodeMType a :: *

        vkPCode :: a -> VkPCodeMType a

        vkPCodeByteOffset :: a -> Int

        readVkPCode :: Ptr a -> IO (VkPCodeMType a)

        writeVkPCode :: Ptr a -> VkPCodeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pCode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPCode a

class HasVkPColorAttachments a where
        type VkPColorAttachmentsMType a :: *

        vkPColorAttachments :: a -> VkPColorAttachmentsMType a

        vkPColorAttachmentsByteOffset :: a -> Int

        readVkPColorAttachments :: Ptr a -> IO (VkPColorAttachmentsMType a)

        writeVkPColorAttachments ::
                                 Ptr a -> VkPColorAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pColorAttachments'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPColorAttachments a

class HasVkPColorBlendState a where
        type VkPColorBlendStateMType a :: *

        vkPColorBlendState :: a -> VkPColorBlendStateMType a

        vkPColorBlendStateByteOffset :: a -> Int

        readVkPColorBlendState :: Ptr a -> IO (VkPColorBlendStateMType a)

        writeVkPColorBlendState ::
                                Ptr a -> VkPColorBlendStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pColorBlendState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPColorBlendState a

class HasVkPCommandBufferDeviceMasks a where
        type VkPCommandBufferDeviceMasksMType a :: *

        vkPCommandBufferDeviceMasks ::
                                    a -> VkPCommandBufferDeviceMasksMType a

        vkPCommandBufferDeviceMasksByteOffset :: a -> Int

        readVkPCommandBufferDeviceMasks ::
                                        Ptr a -> IO (VkPCommandBufferDeviceMasksMType a)

        writeVkPCommandBufferDeviceMasks ::
                                         Ptr a -> VkPCommandBufferDeviceMasksMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pCommandBufferDeviceMasks'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPCommandBufferDeviceMasks a

class HasVkPCommandBuffers a where
        type VkPCommandBuffersMType a :: *

        vkPCommandBuffers :: a -> VkPCommandBuffersMType a

        vkPCommandBuffersByteOffset :: a -> Int

        readVkPCommandBuffers :: Ptr a -> IO (VkPCommandBuffersMType a)

        writeVkPCommandBuffers ::
                               Ptr a -> VkPCommandBuffersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pCommandBuffers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPCommandBuffers a

class HasVkPCorrelationMasks a where
        type VkPCorrelationMasksMType a :: *

        vkPCorrelationMasks :: a -> VkPCorrelationMasksMType a

        vkPCorrelationMasksByteOffset :: a -> Int

        readVkPCorrelationMasks :: Ptr a -> IO (VkPCorrelationMasksMType a)

        writeVkPCorrelationMasks ::
                                 Ptr a -> VkPCorrelationMasksMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pCorrelationMasks'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPCorrelationMasks a

class HasVkPCoverageModulationTable a where
        type VkPCoverageModulationTableMType a :: *

        vkPCoverageModulationTable ::
                                   a -> VkPCoverageModulationTableMType a

        vkPCoverageModulationTableByteOffset :: a -> Int

        readVkPCoverageModulationTable ::
                                       Ptr a -> IO (VkPCoverageModulationTableMType a)

        writeVkPCoverageModulationTable ::
                                        Ptr a -> VkPCoverageModulationTableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pCoverageModulationTable'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPCoverageModulationTable a

class HasVkPData a where
        type VkPDataMType a :: *

        vkPData :: a -> VkPDataMType a

        vkPDataByteOffset :: a -> Int

        readVkPData :: Ptr a -> IO (VkPDataMType a)

        writeVkPData :: Ptr a -> VkPDataMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pData'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPData a

class HasVkPDependencies a where
        type VkPDependenciesMType a :: *

        vkPDependencies :: a -> VkPDependenciesMType a

        vkPDependenciesByteOffset :: a -> Int

        readVkPDependencies :: Ptr a -> IO (VkPDependenciesMType a)

        writeVkPDependencies :: Ptr a -> VkPDependenciesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDependencies'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDependencies a

class HasVkPDepthStencilAttachment a where
        type VkPDepthStencilAttachmentMType a :: *

        vkPDepthStencilAttachment :: a -> VkPDepthStencilAttachmentMType a

        vkPDepthStencilAttachmentByteOffset :: a -> Int

        readVkPDepthStencilAttachment ::
                                      Ptr a -> IO (VkPDepthStencilAttachmentMType a)

        writeVkPDepthStencilAttachment ::
                                       Ptr a -> VkPDepthStencilAttachmentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDepthStencilAttachment'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDepthStencilAttachment a

class HasVkPDepthStencilState a where
        type VkPDepthStencilStateMType a :: *

        vkPDepthStencilState :: a -> VkPDepthStencilStateMType a

        vkPDepthStencilStateByteOffset :: a -> Int

        readVkPDepthStencilState ::
                                 Ptr a -> IO (VkPDepthStencilStateMType a)

        writeVkPDepthStencilState ::
                                  Ptr a -> VkPDepthStencilStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDepthStencilState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDepthStencilState a

class HasVkPDescriptorUpdateEntries a where
        type VkPDescriptorUpdateEntriesMType a :: *

        vkPDescriptorUpdateEntries ::
                                   a -> VkPDescriptorUpdateEntriesMType a

        vkPDescriptorUpdateEntriesByteOffset :: a -> Int

        readVkPDescriptorUpdateEntries ::
                                       Ptr a -> IO (VkPDescriptorUpdateEntriesMType a)

        writeVkPDescriptorUpdateEntries ::
                                        Ptr a -> VkPDescriptorUpdateEntriesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDescriptorUpdateEntries'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDescriptorUpdateEntries a

class HasVkPDeviceIndices a where
        type VkPDeviceIndicesMType a :: *

        vkPDeviceIndices :: a -> VkPDeviceIndicesMType a

        vkPDeviceIndicesByteOffset :: a -> Int

        readVkPDeviceIndices :: Ptr a -> IO (VkPDeviceIndicesMType a)

        writeVkPDeviceIndices :: Ptr a -> VkPDeviceIndicesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDeviceIndices'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDeviceIndices a

class HasVkPDeviceMasks a where
        type VkPDeviceMasksMType a :: *

        vkPDeviceMasks :: a -> VkPDeviceMasksMType a

        vkPDeviceMasksByteOffset :: a -> Int

        readVkPDeviceMasks :: Ptr a -> IO (VkPDeviceMasksMType a)

        writeVkPDeviceMasks :: Ptr a -> VkPDeviceMasksMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDeviceMasks'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDeviceMasks a

class HasVkPDeviceRenderAreas a where
        type VkPDeviceRenderAreasMType a :: *

        vkPDeviceRenderAreas :: a -> VkPDeviceRenderAreasMType a

        vkPDeviceRenderAreasByteOffset :: a -> Int

        readVkPDeviceRenderAreas ::
                                 Ptr a -> IO (VkPDeviceRenderAreasMType a)

        writeVkPDeviceRenderAreas ::
                                  Ptr a -> VkPDeviceRenderAreasMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDeviceRenderAreas'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDeviceRenderAreas a

class HasVkPDisabledValidationChecks a where
        type VkPDisabledValidationChecksMType a :: *

        vkPDisabledValidationChecks ::
                                    a -> VkPDisabledValidationChecksMType a

        vkPDisabledValidationChecksByteOffset :: a -> Int

        readVkPDisabledValidationChecks ::
                                        Ptr a -> IO (VkPDisabledValidationChecksMType a)

        writeVkPDisabledValidationChecks ::
                                         Ptr a -> VkPDisabledValidationChecksMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDisabledValidationChecks'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDisabledValidationChecks a

class HasVkPDiscardRectangles a where
        type VkPDiscardRectanglesMType a :: *

        vkPDiscardRectangles :: a -> VkPDiscardRectanglesMType a

        vkPDiscardRectanglesByteOffset :: a -> Int

        readVkPDiscardRectangles ::
                                 Ptr a -> IO (VkPDiscardRectanglesMType a)

        writeVkPDiscardRectangles ::
                                  Ptr a -> VkPDiscardRectanglesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDiscardRectangles'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDiscardRectangles a

class HasVkPDynamicState a where
        type VkPDynamicStateMType a :: *

        vkPDynamicState :: a -> VkPDynamicStateMType a

        vkPDynamicStateByteOffset :: a -> Int

        readVkPDynamicState :: Ptr a -> IO (VkPDynamicStateMType a)

        writeVkPDynamicState :: Ptr a -> VkPDynamicStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDynamicState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDynamicState a

class HasVkPDynamicStates a where
        type VkPDynamicStatesMType a :: *

        vkPDynamicStates :: a -> VkPDynamicStatesMType a

        vkPDynamicStatesByteOffset :: a -> Int

        readVkPDynamicStates :: Ptr a -> IO (VkPDynamicStatesMType a)

        writeVkPDynamicStates :: Ptr a -> VkPDynamicStatesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pDynamicStates'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPDynamicStates a

class HasVkPEnabledFeatures a where
        type VkPEnabledFeaturesMType a :: *

        vkPEnabledFeatures :: a -> VkPEnabledFeaturesMType a

        vkPEnabledFeaturesByteOffset :: a -> Int

        readVkPEnabledFeatures :: Ptr a -> IO (VkPEnabledFeaturesMType a)

        writeVkPEnabledFeatures ::
                                Ptr a -> VkPEnabledFeaturesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pEnabledFeatures'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPEnabledFeatures a

class HasVkPEngineName a where
        type VkPEngineNameMType a :: *

        vkPEngineName :: a -> VkPEngineNameMType a

        vkPEngineNameByteOffset :: a -> Int

        readVkPEngineName :: Ptr a -> IO (VkPEngineNameMType a)

        writeVkPEngineName :: Ptr a -> VkPEngineNameMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pEngineName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPEngineName a

class HasVkPHostPointer a where
        type VkPHostPointerMType a :: *

        vkPHostPointer :: a -> VkPHostPointerMType a

        vkPHostPointerByteOffset :: a -> Int

        readVkPHostPointer :: Ptr a -> IO (VkPHostPointerMType a)

        writeVkPHostPointer :: Ptr a -> VkPHostPointerMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pHostPointer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPHostPointer a

class HasVkPImageBinds a where
        type VkPImageBindsMType a :: *

        vkPImageBinds :: a -> VkPImageBindsMType a

        vkPImageBindsByteOffset :: a -> Int

        readVkPImageBinds :: Ptr a -> IO (VkPImageBindsMType a)

        writeVkPImageBinds :: Ptr a -> VkPImageBindsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pImageBinds'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPImageBinds a

class HasVkPImageIndices a where
        type VkPImageIndicesMType a :: *

        vkPImageIndices :: a -> VkPImageIndicesMType a

        vkPImageIndicesByteOffset :: a -> Int

        readVkPImageIndices :: Ptr a -> IO (VkPImageIndicesMType a)

        writeVkPImageIndices :: Ptr a -> VkPImageIndicesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pImageIndices'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPImageIndices a

class HasVkPImageInfo a where
        type VkPImageInfoMType a :: *

        vkPImageInfo :: a -> VkPImageInfoMType a

        vkPImageInfoByteOffset :: a -> Int

        readVkPImageInfo :: Ptr a -> IO (VkPImageInfoMType a)

        writeVkPImageInfo :: Ptr a -> VkPImageInfoMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pImageInfo'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPImageInfo a

class HasVkPImageOpaqueBinds a where
        type VkPImageOpaqueBindsMType a :: *

        vkPImageOpaqueBinds :: a -> VkPImageOpaqueBindsMType a

        vkPImageOpaqueBindsByteOffset :: a -> Int

        readVkPImageOpaqueBinds :: Ptr a -> IO (VkPImageOpaqueBindsMType a)

        writeVkPImageOpaqueBinds ::
                                 Ptr a -> VkPImageOpaqueBindsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pImageOpaqueBinds'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPImageOpaqueBinds a

class HasVkPImmutableSamplers a where
        type VkPImmutableSamplersMType a :: *

        vkPImmutableSamplers :: a -> VkPImmutableSamplersMType a

        vkPImmutableSamplersByteOffset :: a -> Int

        readVkPImmutableSamplers ::
                                 Ptr a -> IO (VkPImmutableSamplersMType a)

        writeVkPImmutableSamplers ::
                                  Ptr a -> VkPImmutableSamplersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pImmutableSamplers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPImmutableSamplers a

class HasVkPIndirectCommandsTokens a where
        type VkPIndirectCommandsTokensMType a :: *

        vkPIndirectCommandsTokens :: a -> VkPIndirectCommandsTokensMType a

        vkPIndirectCommandsTokensByteOffset :: a -> Int

        readVkPIndirectCommandsTokens ::
                                      Ptr a -> IO (VkPIndirectCommandsTokensMType a)

        writeVkPIndirectCommandsTokens ::
                                       Ptr a -> VkPIndirectCommandsTokensMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pIndirectCommandsTokens'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPIndirectCommandsTokens a

class HasVkPInheritanceInfo a where
        type VkPInheritanceInfoMType a :: *

        vkPInheritanceInfo :: a -> VkPInheritanceInfoMType a

        vkPInheritanceInfoByteOffset :: a -> Int

        readVkPInheritanceInfo :: Ptr a -> IO (VkPInheritanceInfoMType a)

        writeVkPInheritanceInfo ::
                                Ptr a -> VkPInheritanceInfoMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pInheritanceInfo'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPInheritanceInfo a

class HasVkPInitialData a where
        type VkPInitialDataMType a :: *

        vkPInitialData :: a -> VkPInitialDataMType a

        vkPInitialDataByteOffset :: a -> Int

        readVkPInitialData :: Ptr a -> IO (VkPInitialDataMType a)

        writeVkPInitialData :: Ptr a -> VkPInitialDataMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pInitialData'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPInitialData a

class HasVkPInputAssemblyState a where
        type VkPInputAssemblyStateMType a :: *

        vkPInputAssemblyState :: a -> VkPInputAssemblyStateMType a

        vkPInputAssemblyStateByteOffset :: a -> Int

        readVkPInputAssemblyState ::
                                  Ptr a -> IO (VkPInputAssemblyStateMType a)

        writeVkPInputAssemblyState ::
                                   Ptr a -> VkPInputAssemblyStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pInputAssemblyState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPInputAssemblyState a

class HasVkPInputAttachments a where
        type VkPInputAttachmentsMType a :: *

        vkPInputAttachments :: a -> VkPInputAttachmentsMType a

        vkPInputAttachmentsByteOffset :: a -> Int

        readVkPInputAttachments :: Ptr a -> IO (VkPInputAttachmentsMType a)

        writeVkPInputAttachments ::
                                 Ptr a -> VkPInputAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pInputAttachments'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPInputAttachments a

class HasVkPMapEntries a where
        type VkPMapEntriesMType a :: *

        vkPMapEntries :: a -> VkPMapEntriesMType a

        vkPMapEntriesByteOffset :: a -> Int

        readVkPMapEntries :: Ptr a -> IO (VkPMapEntriesMType a)

        writeVkPMapEntries :: Ptr a -> VkPMapEntriesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pMapEntries'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPMapEntries a

class HasVkPMarkerName a where
        type VkPMarkerNameMType a :: *

        vkPMarkerName :: a -> VkPMarkerNameMType a

        vkPMarkerNameByteOffset :: a -> Int

        readVkPMarkerName :: Ptr a -> IO (VkPMarkerNameMType a)

        writeVkPMarkerName :: Ptr a -> VkPMarkerNameMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pMarkerName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPMarkerName a

class HasVkPMultisampleState a where
        type VkPMultisampleStateMType a :: *

        vkPMultisampleState :: a -> VkPMultisampleStateMType a

        vkPMultisampleStateByteOffset :: a -> Int

        readVkPMultisampleState :: Ptr a -> IO (VkPMultisampleStateMType a)

        writeVkPMultisampleState ::
                                 Ptr a -> VkPMultisampleStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pMultisampleState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPMultisampleState a

class HasVkPName a where
        type VkPNameMType a :: *

        vkPName :: a -> VkPNameMType a

        vkPNameByteOffset :: a -> Int

        readVkPName :: Ptr a -> IO (VkPNameMType a)

        writeVkPName :: Ptr a -> VkPNameMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPName a

class HasVkPNext a where
        type VkPNextMType a :: *

        vkPNext :: a -> VkPNextMType a

        vkPNextByteOffset :: a -> Int

        readVkPNext :: Ptr a -> IO (VkPNextMType a)

        writeVkPNext :: Ptr a -> VkPNextMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pNext'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPNext a

class HasVkPObjectEntryCounts a where
        type VkPObjectEntryCountsMType a :: *

        vkPObjectEntryCounts :: a -> VkPObjectEntryCountsMType a

        vkPObjectEntryCountsByteOffset :: a -> Int

        readVkPObjectEntryCounts ::
                                 Ptr a -> IO (VkPObjectEntryCountsMType a)

        writeVkPObjectEntryCounts ::
                                  Ptr a -> VkPObjectEntryCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pObjectEntryCounts'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPObjectEntryCounts a

class HasVkPObjectEntryTypes a where
        type VkPObjectEntryTypesMType a :: *

        vkPObjectEntryTypes :: a -> VkPObjectEntryTypesMType a

        vkPObjectEntryTypesByteOffset :: a -> Int

        readVkPObjectEntryTypes :: Ptr a -> IO (VkPObjectEntryTypesMType a)

        writeVkPObjectEntryTypes ::
                                 Ptr a -> VkPObjectEntryTypesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pObjectEntryTypes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPObjectEntryTypes a

class HasVkPObjectEntryUsageFlags a where
        type VkPObjectEntryUsageFlagsMType a :: *

        vkPObjectEntryUsageFlags :: a -> VkPObjectEntryUsageFlagsMType a

        vkPObjectEntryUsageFlagsByteOffset :: a -> Int

        readVkPObjectEntryUsageFlags ::
                                     Ptr a -> IO (VkPObjectEntryUsageFlagsMType a)

        writeVkPObjectEntryUsageFlags ::
                                      Ptr a -> VkPObjectEntryUsageFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pObjectEntryUsageFlags'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPObjectEntryUsageFlags a

class HasVkPObjectName a where
        type VkPObjectNameMType a :: *

        vkPObjectName :: a -> VkPObjectNameMType a

        vkPObjectNameByteOffset :: a -> Int

        readVkPObjectName :: Ptr a -> IO (VkPObjectNameMType a)

        writeVkPObjectName :: Ptr a -> VkPObjectNameMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pObjectName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPObjectName a

class HasVkPPhysicalDevices a where
        type VkPPhysicalDevicesMType a :: *

        vkPPhysicalDevices :: a -> VkPPhysicalDevicesMType a

        vkPPhysicalDevicesByteOffset :: a -> Int

        readVkPPhysicalDevices :: Ptr a -> IO (VkPPhysicalDevicesMType a)

        writeVkPPhysicalDevices ::
                                Ptr a -> VkPPhysicalDevicesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pPhysicalDevices'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPPhysicalDevices a

class HasVkPPoolSizes a where
        type VkPPoolSizesMType a :: *

        vkPPoolSizes :: a -> VkPPoolSizesMType a

        vkPPoolSizesByteOffset :: a -> Int

        readVkPPoolSizes :: Ptr a -> IO (VkPPoolSizesMType a)

        writeVkPPoolSizes :: Ptr a -> VkPPoolSizesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pPoolSizes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPPoolSizes a

class HasVkPPostSubpassSampleLocations a where
        type VkPPostSubpassSampleLocationsMType a :: *

        vkPPostSubpassSampleLocations ::
                                      a -> VkPPostSubpassSampleLocationsMType a

        vkPPostSubpassSampleLocationsByteOffset :: a -> Int

        readVkPPostSubpassSampleLocations ::
                                          Ptr a -> IO (VkPPostSubpassSampleLocationsMType a)

        writeVkPPostSubpassSampleLocations ::
                                           Ptr a -> VkPPostSubpassSampleLocationsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pPostSubpassSampleLocations'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPPostSubpassSampleLocations a

class HasVkPPreserveAttachments a where
        type VkPPreserveAttachmentsMType a :: *

        vkPPreserveAttachments :: a -> VkPPreserveAttachmentsMType a

        vkPPreserveAttachmentsByteOffset :: a -> Int

        readVkPPreserveAttachments ::
                                   Ptr a -> IO (VkPPreserveAttachmentsMType a)

        writeVkPPreserveAttachments ::
                                    Ptr a -> VkPPreserveAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pPreserveAttachments'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPPreserveAttachments a

class HasVkPPushConstantRanges a where
        type VkPPushConstantRangesMType a :: *

        vkPPushConstantRanges :: a -> VkPPushConstantRangesMType a

        vkPPushConstantRangesByteOffset :: a -> Int

        readVkPPushConstantRanges ::
                                  Ptr a -> IO (VkPPushConstantRangesMType a)

        writeVkPPushConstantRanges ::
                                   Ptr a -> VkPPushConstantRangesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pPushConstantRanges'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPPushConstantRanges a

class HasVkPQueueCreateInfos a where
        type VkPQueueCreateInfosMType a :: *

        vkPQueueCreateInfos :: a -> VkPQueueCreateInfosMType a

        vkPQueueCreateInfosByteOffset :: a -> Int

        readVkPQueueCreateInfos :: Ptr a -> IO (VkPQueueCreateInfosMType a)

        writeVkPQueueCreateInfos ::
                                 Ptr a -> VkPQueueCreateInfosMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pQueueCreateInfos'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPQueueCreateInfos a

class HasVkPQueueFamilyIndices a where
        type VkPQueueFamilyIndicesMType a :: *

        vkPQueueFamilyIndices :: a -> VkPQueueFamilyIndicesMType a

        vkPQueueFamilyIndicesByteOffset :: a -> Int

        readVkPQueueFamilyIndices ::
                                  Ptr a -> IO (VkPQueueFamilyIndicesMType a)

        writeVkPQueueFamilyIndices ::
                                   Ptr a -> VkPQueueFamilyIndicesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pQueueFamilyIndices'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPQueueFamilyIndices a

class HasVkPQueuePriorities a where
        type VkPQueuePrioritiesMType a :: *

        vkPQueuePriorities :: a -> VkPQueuePrioritiesMType a

        vkPQueuePrioritiesByteOffset :: a -> Int

        readVkPQueuePriorities :: Ptr a -> IO (VkPQueuePrioritiesMType a)

        writeVkPQueuePriorities ::
                                Ptr a -> VkPQueuePrioritiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pQueuePriorities'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPQueuePriorities a

class HasVkPRasterizationState a where
        type VkPRasterizationStateMType a :: *

        vkPRasterizationState :: a -> VkPRasterizationStateMType a

        vkPRasterizationStateByteOffset :: a -> Int

        readVkPRasterizationState ::
                                  Ptr a -> IO (VkPRasterizationStateMType a)

        writeVkPRasterizationState ::
                                   Ptr a -> VkPRasterizationStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pRasterizationState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPRasterizationState a

class HasVkPRectangles a where
        type VkPRectanglesMType a :: *

        vkPRectangles :: a -> VkPRectanglesMType a

        vkPRectanglesByteOffset :: a -> Int

        readVkPRectangles :: Ptr a -> IO (VkPRectanglesMType a)

        writeVkPRectangles :: Ptr a -> VkPRectanglesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pRectangles'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPRectangles a

class HasVkPRegions a where
        type VkPRegionsMType a :: *

        vkPRegions :: a -> VkPRegionsMType a

        vkPRegionsByteOffset :: a -> Int

        readVkPRegions :: Ptr a -> IO (VkPRegionsMType a)

        writeVkPRegions :: Ptr a -> VkPRegionsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pRegions'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPRegions a

class HasVkPReleaseKeys a where
        type VkPReleaseKeysMType a :: *

        vkPReleaseKeys :: a -> VkPReleaseKeysMType a

        vkPReleaseKeysByteOffset :: a -> Int

        readVkPReleaseKeys :: Ptr a -> IO (VkPReleaseKeysMType a)

        writeVkPReleaseKeys :: Ptr a -> VkPReleaseKeysMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pReleaseKeys'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPReleaseKeys a

class HasVkPReleaseSyncs a where
        type VkPReleaseSyncsMType a :: *

        vkPReleaseSyncs :: a -> VkPReleaseSyncsMType a

        vkPReleaseSyncsByteOffset :: a -> Int

        readVkPReleaseSyncs :: Ptr a -> IO (VkPReleaseSyncsMType a)

        writeVkPReleaseSyncs :: Ptr a -> VkPReleaseSyncsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pReleaseSyncs'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPReleaseSyncs a

class HasVkPResolveAttachments a where
        type VkPResolveAttachmentsMType a :: *

        vkPResolveAttachments :: a -> VkPResolveAttachmentsMType a

        vkPResolveAttachmentsByteOffset :: a -> Int

        readVkPResolveAttachments ::
                                  Ptr a -> IO (VkPResolveAttachmentsMType a)

        writeVkPResolveAttachments ::
                                   Ptr a -> VkPResolveAttachmentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pResolveAttachments'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPResolveAttachments a

class HasVkPResults a where
        type VkPResultsMType a :: *

        vkPResults :: a -> VkPResultsMType a

        vkPResultsByteOffset :: a -> Int

        readVkPResults :: Ptr a -> IO (VkPResultsMType a)

        writeVkPResults :: Ptr a -> VkPResultsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pResults'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPResults a

class HasVkPSFRRects a where
        type VkPSFRRectsMType a :: *

        vkPSFRRects :: a -> VkPSFRRectsMType a

        vkPSFRRectsByteOffset :: a -> Int

        readVkPSFRRects :: Ptr a -> IO (VkPSFRRectsMType a)

        writeVkPSFRRects :: Ptr a -> VkPSFRRectsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSFRRects'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSFRRects a

class HasVkPSampleLocations a where
        type VkPSampleLocationsMType a :: *

        vkPSampleLocations :: a -> VkPSampleLocationsMType a

        vkPSampleLocationsByteOffset :: a -> Int

        readVkPSampleLocations :: Ptr a -> IO (VkPSampleLocationsMType a)

        writeVkPSampleLocations ::
                                Ptr a -> VkPSampleLocationsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSampleLocations'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSampleLocations a

class HasVkPSampleMask a where
        type VkPSampleMaskMType a :: *

        vkPSampleMask :: a -> VkPSampleMaskMType a

        vkPSampleMaskByteOffset :: a -> Int

        readVkPSampleMask :: Ptr a -> IO (VkPSampleMaskMType a)

        writeVkPSampleMask :: Ptr a -> VkPSampleMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSampleMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSampleMask a

class HasVkPScissors a where
        type VkPScissorsMType a :: *

        vkPScissors :: a -> VkPScissorsMType a

        vkPScissorsByteOffset :: a -> Int

        readVkPScissors :: Ptr a -> IO (VkPScissorsMType a)

        writeVkPScissors :: Ptr a -> VkPScissorsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pScissors'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPScissors a

class HasVkPSetLayouts a where
        type VkPSetLayoutsMType a :: *

        vkPSetLayouts :: a -> VkPSetLayoutsMType a

        vkPSetLayoutsByteOffset :: a -> Int

        readVkPSetLayouts :: Ptr a -> IO (VkPSetLayoutsMType a)

        writeVkPSetLayouts :: Ptr a -> VkPSetLayoutsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSetLayouts'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSetLayouts a

class HasVkPSignalSemaphoreDeviceIndices a where
        type VkPSignalSemaphoreDeviceIndicesMType a :: *

        vkPSignalSemaphoreDeviceIndices ::
                                        a -> VkPSignalSemaphoreDeviceIndicesMType a

        vkPSignalSemaphoreDeviceIndicesByteOffset :: a -> Int

        readVkPSignalSemaphoreDeviceIndices ::
                                            Ptr a -> IO (VkPSignalSemaphoreDeviceIndicesMType a)

        writeVkPSignalSemaphoreDeviceIndices ::
                                             Ptr a ->
                                               VkPSignalSemaphoreDeviceIndicesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'pSignalSemaphoreDeviceIndices'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSignalSemaphoreDeviceIndices a

class HasVkPSignalSemaphoreValues a where
        type VkPSignalSemaphoreValuesMType a :: *

        vkPSignalSemaphoreValues :: a -> VkPSignalSemaphoreValuesMType a

        vkPSignalSemaphoreValuesByteOffset :: a -> Int

        readVkPSignalSemaphoreValues ::
                                     Ptr a -> IO (VkPSignalSemaphoreValuesMType a)

        writeVkPSignalSemaphoreValues ::
                                      Ptr a -> VkPSignalSemaphoreValuesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSignalSemaphoreValues'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSignalSemaphoreValues a

class HasVkPSignalSemaphores a where
        type VkPSignalSemaphoresMType a :: *

        vkPSignalSemaphores :: a -> VkPSignalSemaphoresMType a

        vkPSignalSemaphoresByteOffset :: a -> Int

        readVkPSignalSemaphores :: Ptr a -> IO (VkPSignalSemaphoresMType a)

        writeVkPSignalSemaphores ::
                                 Ptr a -> VkPSignalSemaphoresMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSignalSemaphores'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSignalSemaphores a

class HasVkPSpecializationInfo a where
        type VkPSpecializationInfoMType a :: *

        vkPSpecializationInfo :: a -> VkPSpecializationInfoMType a

        vkPSpecializationInfoByteOffset :: a -> Int

        readVkPSpecializationInfo ::
                                  Ptr a -> IO (VkPSpecializationInfoMType a)

        writeVkPSpecializationInfo ::
                                   Ptr a -> VkPSpecializationInfoMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSpecializationInfo'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSpecializationInfo a

class HasVkPStages a where
        type VkPStagesMType a :: *

        vkPStages :: a -> VkPStagesMType a

        vkPStagesByteOffset :: a -> Int

        readVkPStages :: Ptr a -> IO (VkPStagesMType a)

        writeVkPStages :: Ptr a -> VkPStagesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pStages'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPStages a

class HasVkPSubpasses a where
        type VkPSubpassesMType a :: *

        vkPSubpasses :: a -> VkPSubpassesMType a

        vkPSubpassesByteOffset :: a -> Int

        readVkPSubpasses :: Ptr a -> IO (VkPSubpassesMType a)

        writeVkPSubpasses :: Ptr a -> VkPSubpassesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSubpasses'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSubpasses a

class HasVkPSwapchains a where
        type VkPSwapchainsMType a :: *

        vkPSwapchains :: a -> VkPSwapchainsMType a

        vkPSwapchainsByteOffset :: a -> Int

        readVkPSwapchains :: Ptr a -> IO (VkPSwapchainsMType a)

        writeVkPSwapchains :: Ptr a -> VkPSwapchainsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pSwapchains'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPSwapchains a

class HasVkPTag a where
        type VkPTagMType a :: *

        vkPTag :: a -> VkPTagMType a

        vkPTagByteOffset :: a -> Int

        readVkPTag :: Ptr a -> IO (VkPTagMType a)

        writeVkPTag :: Ptr a -> VkPTagMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pTag'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPTag a

class HasVkPTessellationState a where
        type VkPTessellationStateMType a :: *

        vkPTessellationState :: a -> VkPTessellationStateMType a

        vkPTessellationStateByteOffset :: a -> Int

        readVkPTessellationState ::
                                 Ptr a -> IO (VkPTessellationStateMType a)

        writeVkPTessellationState ::
                                  Ptr a -> VkPTessellationStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pTessellationState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPTessellationState a

class HasVkPTexelBufferView a where
        type VkPTexelBufferViewMType a :: *

        vkPTexelBufferView :: a -> VkPTexelBufferViewMType a

        vkPTexelBufferViewByteOffset :: a -> Int

        readVkPTexelBufferView :: Ptr a -> IO (VkPTexelBufferViewMType a)

        writeVkPTexelBufferView ::
                                Ptr a -> VkPTexelBufferViewMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pTexelBufferView'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPTexelBufferView a

class HasVkPTimes a where
        type VkPTimesMType a :: *

        vkPTimes :: a -> VkPTimesMType a

        vkPTimesByteOffset :: a -> Int

        readVkPTimes :: Ptr a -> IO (VkPTimesMType a)

        writeVkPTimes :: Ptr a -> VkPTimesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pTimes'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPTimes a

class HasVkPTokens a where
        type VkPTokensMType a :: *

        vkPTokens :: a -> VkPTokensMType a

        vkPTokensByteOffset :: a -> Int

        readVkPTokens :: Ptr a -> IO (VkPTokensMType a)

        writeVkPTokens :: Ptr a -> VkPTokensMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pTokens'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPTokens a

class HasVkPUserData a where
        type VkPUserDataMType a :: *

        vkPUserData :: a -> VkPUserDataMType a

        vkPUserDataByteOffset :: a -> Int

        readVkPUserData :: Ptr a -> IO (VkPUserDataMType a)

        writeVkPUserData :: Ptr a -> VkPUserDataMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pUserData'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPUserData a

class HasVkPVertexAttributeDescriptions a where
        type VkPVertexAttributeDescriptionsMType a :: *

        vkPVertexAttributeDescriptions ::
                                       a -> VkPVertexAttributeDescriptionsMType a

        vkPVertexAttributeDescriptionsByteOffset :: a -> Int

        readVkPVertexAttributeDescriptions ::
                                           Ptr a -> IO (VkPVertexAttributeDescriptionsMType a)

        writeVkPVertexAttributeDescriptions ::
                                            Ptr a -> VkPVertexAttributeDescriptionsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'pVertexAttributeDescriptions'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPVertexAttributeDescriptions a

class HasVkPVertexBindingDescriptions a where
        type VkPVertexBindingDescriptionsMType a :: *

        vkPVertexBindingDescriptions ::
                                     a -> VkPVertexBindingDescriptionsMType a

        vkPVertexBindingDescriptionsByteOffset :: a -> Int

        readVkPVertexBindingDescriptions ::
                                         Ptr a -> IO (VkPVertexBindingDescriptionsMType a)

        writeVkPVertexBindingDescriptions ::
                                          Ptr a -> VkPVertexBindingDescriptionsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pVertexBindingDescriptions'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPVertexBindingDescriptions a

class HasVkPVertexInputState a where
        type VkPVertexInputStateMType a :: *

        vkPVertexInputState :: a -> VkPVertexInputStateMType a

        vkPVertexInputStateByteOffset :: a -> Int

        readVkPVertexInputState :: Ptr a -> IO (VkPVertexInputStateMType a)

        writeVkPVertexInputState ::
                                 Ptr a -> VkPVertexInputStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pVertexInputState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPVertexInputState a

class HasVkPView a where
        type VkPViewMType a :: *

        vkPView :: a -> VkPViewMType a

        vkPViewByteOffset :: a -> Int

        readVkPView :: Ptr a -> IO (VkPViewMType a)

        writeVkPView :: Ptr a -> VkPViewMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pView'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPView a

class HasVkPViewFormats a where
        type VkPViewFormatsMType a :: *

        vkPViewFormats :: a -> VkPViewFormatsMType a

        vkPViewFormatsByteOffset :: a -> Int

        readVkPViewFormats :: Ptr a -> IO (VkPViewFormatsMType a)

        writeVkPViewFormats :: Ptr a -> VkPViewFormatsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pViewFormats'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPViewFormats a

class HasVkPViewMasks a where
        type VkPViewMasksMType a :: *

        vkPViewMasks :: a -> VkPViewMasksMType a

        vkPViewMasksByteOffset :: a -> Int

        readVkPViewMasks :: Ptr a -> IO (VkPViewMasksMType a)

        writeVkPViewMasks :: Ptr a -> VkPViewMasksMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pViewMasks'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPViewMasks a

class HasVkPViewOffsets a where
        type VkPViewOffsetsMType a :: *

        vkPViewOffsets :: a -> VkPViewOffsetsMType a

        vkPViewOffsetsByteOffset :: a -> Int

        readVkPViewOffsets :: Ptr a -> IO (VkPViewOffsetsMType a)

        writeVkPViewOffsets :: Ptr a -> VkPViewOffsetsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pViewOffsets'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPViewOffsets a

class HasVkPViewportState a where
        type VkPViewportStateMType a :: *

        vkPViewportState :: a -> VkPViewportStateMType a

        vkPViewportStateByteOffset :: a -> Int

        readVkPViewportState :: Ptr a -> IO (VkPViewportStateMType a)

        writeVkPViewportState :: Ptr a -> VkPViewportStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pViewportState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPViewportState a

class HasVkPViewportSwizzles a where
        type VkPViewportSwizzlesMType a :: *

        vkPViewportSwizzles :: a -> VkPViewportSwizzlesMType a

        vkPViewportSwizzlesByteOffset :: a -> Int

        readVkPViewportSwizzles :: Ptr a -> IO (VkPViewportSwizzlesMType a)

        writeVkPViewportSwizzles ::
                                 Ptr a -> VkPViewportSwizzlesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pViewportSwizzles'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPViewportSwizzles a

class HasVkPViewportWScalings a where
        type VkPViewportWScalingsMType a :: *

        vkPViewportWScalings :: a -> VkPViewportWScalingsMType a

        vkPViewportWScalingsByteOffset :: a -> Int

        readVkPViewportWScalings ::
                                 Ptr a -> IO (VkPViewportWScalingsMType a)

        writeVkPViewportWScalings ::
                                  Ptr a -> VkPViewportWScalingsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pViewportWScalings'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPViewportWScalings a

class HasVkPViewports a where
        type VkPViewportsMType a :: *

        vkPViewports :: a -> VkPViewportsMType a

        vkPViewportsByteOffset :: a -> Int

        readVkPViewports :: Ptr a -> IO (VkPViewportsMType a)

        writeVkPViewports :: Ptr a -> VkPViewportsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pViewports'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPViewports a

class HasVkPWaitDstStageMask a where
        type VkPWaitDstStageMaskMType a :: *

        vkPWaitDstStageMask :: a -> VkPWaitDstStageMaskMType a

        vkPWaitDstStageMaskByteOffset :: a -> Int

        readVkPWaitDstStageMask :: Ptr a -> IO (VkPWaitDstStageMaskMType a)

        writeVkPWaitDstStageMask ::
                                 Ptr a -> VkPWaitDstStageMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pWaitDstStageMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPWaitDstStageMask a

class HasVkPWaitSemaphoreDeviceIndices a where
        type VkPWaitSemaphoreDeviceIndicesMType a :: *

        vkPWaitSemaphoreDeviceIndices ::
                                      a -> VkPWaitSemaphoreDeviceIndicesMType a

        vkPWaitSemaphoreDeviceIndicesByteOffset :: a -> Int

        readVkPWaitSemaphoreDeviceIndices ::
                                          Ptr a -> IO (VkPWaitSemaphoreDeviceIndicesMType a)

        writeVkPWaitSemaphoreDeviceIndices ::
                                           Ptr a -> VkPWaitSemaphoreDeviceIndicesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pWaitSemaphoreDeviceIndices'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPWaitSemaphoreDeviceIndices a

class HasVkPWaitSemaphoreValues a where
        type VkPWaitSemaphoreValuesMType a :: *

        vkPWaitSemaphoreValues :: a -> VkPWaitSemaphoreValuesMType a

        vkPWaitSemaphoreValuesByteOffset :: a -> Int

        readVkPWaitSemaphoreValues ::
                                   Ptr a -> IO (VkPWaitSemaphoreValuesMType a)

        writeVkPWaitSemaphoreValues ::
                                    Ptr a -> VkPWaitSemaphoreValuesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pWaitSemaphoreValues'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPWaitSemaphoreValues a

class HasVkPWaitSemaphores a where
        type VkPWaitSemaphoresMType a :: *

        vkPWaitSemaphores :: a -> VkPWaitSemaphoresMType a

        vkPWaitSemaphoresByteOffset :: a -> Int

        readVkPWaitSemaphores :: Ptr a -> IO (VkPWaitSemaphoresMType a)

        writeVkPWaitSemaphores ::
                               Ptr a -> VkPWaitSemaphoresMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pWaitSemaphores'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPWaitSemaphores a

class HasVkParameters a where
        type VkParametersMType a :: *

        vkParameters :: a -> VkParametersMType a

        vkParametersByteOffset :: a -> Int

        readVkParameters :: Ptr a -> IO (VkParametersMType a)

        writeVkParameters :: Ptr a -> VkParametersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'parameters'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkParameters a

class HasVkPassOp a where
        type VkPassOpMType a :: *

        vkPassOp :: a -> VkPassOpMType a

        vkPassOpByteOffset :: a -> Int

        readVkPassOp :: Ptr a -> IO (VkPassOpMType a)

        writeVkPassOp :: Ptr a -> VkPassOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'passOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPassOp a

class HasVkPatchControlPoints a where
        type VkPatchControlPointsMType a :: *

        vkPatchControlPoints :: a -> VkPatchControlPointsMType a

        vkPatchControlPointsByteOffset :: a -> Int

        readVkPatchControlPoints ::
                                 Ptr a -> IO (VkPatchControlPointsMType a)

        writeVkPatchControlPoints ::
                                  Ptr a -> VkPatchControlPointsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'patchControlPoints'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPatchControlPoints a

class HasVkPerViewPositionAllComponents a where
        type VkPerViewPositionAllComponentsMType a :: *

        vkPerViewPositionAllComponents ::
                                       a -> VkPerViewPositionAllComponentsMType a

        vkPerViewPositionAllComponentsByteOffset :: a -> Int

        readVkPerViewPositionAllComponents ::
                                           Ptr a -> IO (VkPerViewPositionAllComponentsMType a)

        writeVkPerViewPositionAllComponents ::
                                            Ptr a -> VkPerViewPositionAllComponentsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'perViewPositionAllComponents'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPerViewPositionAllComponents a

class HasVkPersistent a where
        type VkPersistentMType a :: *

        vkPersistent :: a -> VkPersistentMType a

        vkPersistentByteOffset :: a -> Int

        readVkPersistent :: Ptr a -> IO (VkPersistentMType a)

        writeVkPersistent :: Ptr a -> VkPersistentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'persistent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPersistent a

class HasVkPersistentContent a where
        type VkPersistentContentMType a :: *

        vkPersistentContent :: a -> VkPersistentContentMType a

        vkPersistentContentByteOffset :: a -> Int

        readVkPersistentContent :: Ptr a -> IO (VkPersistentContentMType a)

        writeVkPersistentContent ::
                                 Ptr a -> VkPersistentContentMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'persistentContent'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPersistentContent a

class HasVkPfnAllocation a where
        type VkPfnAllocationMType a :: *

        vkPfnAllocation :: a -> VkPfnAllocationMType a

        vkPfnAllocationByteOffset :: a -> Int

        readVkPfnAllocation :: Ptr a -> IO (VkPfnAllocationMType a)

        writeVkPfnAllocation :: Ptr a -> VkPfnAllocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pfnAllocation'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPfnAllocation a

class HasVkPfnCallback a where
        type VkPfnCallbackMType a :: *

        vkPfnCallback :: a -> VkPfnCallbackMType a

        vkPfnCallbackByteOffset :: a -> Int

        readVkPfnCallback :: Ptr a -> IO (VkPfnCallbackMType a)

        writeVkPfnCallback :: Ptr a -> VkPfnCallbackMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pfnCallback'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPfnCallback a

class HasVkPfnFree a where
        type VkPfnFreeMType a :: *

        vkPfnFree :: a -> VkPfnFreeMType a

        vkPfnFreeByteOffset :: a -> Int

        readVkPfnFree :: Ptr a -> IO (VkPfnFreeMType a)

        writeVkPfnFree :: Ptr a -> VkPfnFreeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pfnFree'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPfnFree a

class HasVkPfnInternalAllocation a where
        type VkPfnInternalAllocationMType a :: *

        vkPfnInternalAllocation :: a -> VkPfnInternalAllocationMType a

        vkPfnInternalAllocationByteOffset :: a -> Int

        readVkPfnInternalAllocation ::
                                    Ptr a -> IO (VkPfnInternalAllocationMType a)

        writeVkPfnInternalAllocation ::
                                     Ptr a -> VkPfnInternalAllocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pfnInternalAllocation'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPfnInternalAllocation a

class HasVkPfnInternalFree a where
        type VkPfnInternalFreeMType a :: *

        vkPfnInternalFree :: a -> VkPfnInternalFreeMType a

        vkPfnInternalFreeByteOffset :: a -> Int

        readVkPfnInternalFree :: Ptr a -> IO (VkPfnInternalFreeMType a)

        writeVkPfnInternalFree ::
                               Ptr a -> VkPfnInternalFreeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pfnInternalFree'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPfnInternalFree a

class HasVkPfnReallocation a where
        type VkPfnReallocationMType a :: *

        vkPfnReallocation :: a -> VkPfnReallocationMType a

        vkPfnReallocationByteOffset :: a -> Int

        readVkPfnReallocation :: Ptr a -> IO (VkPfnReallocationMType a)

        writeVkPfnReallocation ::
                               Ptr a -> VkPfnReallocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pfnReallocation'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPfnReallocation a

class HasVkPhysicalDeviceCount a where
        type VkPhysicalDeviceCountMType a :: *

        vkPhysicalDeviceCount :: a -> VkPhysicalDeviceCountMType a

        vkPhysicalDeviceCountByteOffset :: a -> Int

        readVkPhysicalDeviceCount ::
                                  Ptr a -> IO (VkPhysicalDeviceCountMType a)

        writeVkPhysicalDeviceCount ::
                                   Ptr a -> VkPhysicalDeviceCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'physicalDeviceCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPhysicalDeviceCount a

class HasVkPhysicalDevicesArray a where
        type VkPhysicalDevicesArrayMType a :: *

        vkPhysicalDevicesArray :: a -> Int -> VkPhysicalDevicesArrayMType a

        vkPhysicalDevicesArrayByteOffset :: a -> Int

        readVkPhysicalDevicesArray ::
                                   Ptr a -> Int -> IO (VkPhysicalDevicesArrayMType a)

        writeVkPhysicalDevicesArray ::
                                    Ptr a -> Int -> VkPhysicalDevicesArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'physicalDevices'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPhysicalDevicesArray a

class HasVkPhysicalDimensions a where
        type VkPhysicalDimensionsMType a :: *

        vkPhysicalDimensions :: a -> VkPhysicalDimensionsMType a

        vkPhysicalDimensionsByteOffset :: a -> Int

        readVkPhysicalDimensions ::
                                 Ptr a -> IO (VkPhysicalDimensionsMType a)

        writeVkPhysicalDimensions ::
                                  Ptr a -> VkPhysicalDimensionsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'physicalDimensions'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPhysicalDimensions a

class HasVkPhysicalResolution a where
        type VkPhysicalResolutionMType a :: *

        vkPhysicalResolution :: a -> VkPhysicalResolutionMType a

        vkPhysicalResolutionByteOffset :: a -> Int

        readVkPhysicalResolution ::
                                 Ptr a -> IO (VkPhysicalResolutionMType a)

        writeVkPhysicalResolution ::
                                  Ptr a -> VkPhysicalResolutionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'physicalResolution'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPhysicalResolution a

class HasVkPipeline a where
        type VkPipelineMType a :: *

        vkPipeline :: a -> VkPipelineMType a

        vkPipelineByteOffset :: a -> Int

        readVkPipeline :: Ptr a -> IO (VkPipelineMType a)

        writeVkPipeline :: Ptr a -> VkPipelineMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pipeline'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPipeline a

class HasVkPipelineBindPoint a where
        type VkPipelineBindPointMType a :: *

        vkPipelineBindPoint :: a -> VkPipelineBindPointMType a

        vkPipelineBindPointByteOffset :: a -> Int

        readVkPipelineBindPoint :: Ptr a -> IO (VkPipelineBindPointMType a)

        writeVkPipelineBindPoint ::
                                 Ptr a -> VkPipelineBindPointMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pipelineBindPoint'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPipelineBindPoint a

class HasVkPipelineCacheUUIDArray a where
        type VkPipelineCacheUUIDArrayMType a :: *

        vkPipelineCacheUUIDArray ::
                                 a -> Int -> VkPipelineCacheUUIDArrayMType a

        vkPipelineCacheUUIDArrayByteOffset :: a -> Int

        readVkPipelineCacheUUIDArray ::
                                     Ptr a -> Int -> IO (VkPipelineCacheUUIDArrayMType a)

        writeVkPipelineCacheUUIDArray ::
                                      Ptr a -> Int -> VkPipelineCacheUUIDArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pipelineCacheUUID'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPipelineCacheUUIDArray a

class HasVkPipelineLayout a where
        type VkPipelineLayoutMType a :: *

        vkPipelineLayout :: a -> VkPipelineLayoutMType a

        vkPipelineLayoutByteOffset :: a -> Int

        readVkPipelineLayout :: Ptr a -> IO (VkPipelineLayoutMType a)

        writeVkPipelineLayout :: Ptr a -> VkPipelineLayoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pipelineLayout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPipelineLayout a

class HasVkPipelineStatistics a where
        type VkPipelineStatisticsMType a :: *

        vkPipelineStatistics :: a -> VkPipelineStatisticsMType a

        vkPipelineStatisticsByteOffset :: a -> Int

        readVkPipelineStatistics ::
                                 Ptr a -> IO (VkPipelineStatisticsMType a)

        writeVkPipelineStatistics ::
                                  Ptr a -> VkPipelineStatisticsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pipelineStatistics'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPipelineStatistics a

class HasVkPipelineStatisticsQuery a where
        type VkPipelineStatisticsQueryMType a :: *

        vkPipelineStatisticsQuery :: a -> VkPipelineStatisticsQueryMType a

        vkPipelineStatisticsQueryByteOffset :: a -> Int

        readVkPipelineStatisticsQuery ::
                                      Ptr a -> IO (VkPipelineStatisticsQueryMType a)

        writeVkPipelineStatisticsQuery ::
                                       Ptr a -> VkPipelineStatisticsQueryMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pipelineStatisticsQuery'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPipelineStatisticsQuery a

class HasVkPlaneAspect a where
        type VkPlaneAspectMType a :: *

        vkPlaneAspect :: a -> VkPlaneAspectMType a

        vkPlaneAspectByteOffset :: a -> Int

        readVkPlaneAspect :: Ptr a -> IO (VkPlaneAspectMType a)

        writeVkPlaneAspect :: Ptr a -> VkPlaneAspectMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'planeAspect'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPlaneAspect a

class HasVkPlaneIndex a where
        type VkPlaneIndexMType a :: *

        vkPlaneIndex :: a -> VkPlaneIndexMType a

        vkPlaneIndexByteOffset :: a -> Int

        readVkPlaneIndex :: Ptr a -> IO (VkPlaneIndexMType a)

        writeVkPlaneIndex :: Ptr a -> VkPlaneIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'planeIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPlaneIndex a

class HasVkPlaneReorderPossible a where
        type VkPlaneReorderPossibleMType a :: *

        vkPlaneReorderPossible :: a -> VkPlaneReorderPossibleMType a

        vkPlaneReorderPossibleByteOffset :: a -> Int

        readVkPlaneReorderPossible ::
                                   Ptr a -> IO (VkPlaneReorderPossibleMType a)

        writeVkPlaneReorderPossible ::
                                    Ptr a -> VkPlaneReorderPossibleMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'planeReorderPossible'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPlaneReorderPossible a

class HasVkPlaneStackIndex a where
        type VkPlaneStackIndexMType a :: *

        vkPlaneStackIndex :: a -> VkPlaneStackIndexMType a

        vkPlaneStackIndexByteOffset :: a -> Int

        readVkPlaneStackIndex :: Ptr a -> IO (VkPlaneStackIndexMType a)

        writeVkPlaneStackIndex ::
                               Ptr a -> VkPlaneStackIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'planeStackIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPlaneStackIndex a

class HasVkPointClippingBehavior a where
        type VkPointClippingBehaviorMType a :: *

        vkPointClippingBehavior :: a -> VkPointClippingBehaviorMType a

        vkPointClippingBehaviorByteOffset :: a -> Int

        readVkPointClippingBehavior ::
                                    Ptr a -> IO (VkPointClippingBehaviorMType a)

        writeVkPointClippingBehavior ::
                                     Ptr a -> VkPointClippingBehaviorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pointClippingBehavior'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPointClippingBehavior a

class HasVkPointSizeGranularity a where
        type VkPointSizeGranularityMType a :: *

        vkPointSizeGranularity :: a -> VkPointSizeGranularityMType a

        vkPointSizeGranularityByteOffset :: a -> Int

        readVkPointSizeGranularity ::
                                   Ptr a -> IO (VkPointSizeGranularityMType a)

        writeVkPointSizeGranularity ::
                                    Ptr a -> VkPointSizeGranularityMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pointSizeGranularity'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPointSizeGranularity a

class HasVkPointSizeRangeArray a where
        type VkPointSizeRangeArrayMType a :: *

        vkPointSizeRangeArray :: a -> Int -> VkPointSizeRangeArrayMType a

        vkPointSizeRangeArrayByteOffset :: a -> Int

        readVkPointSizeRangeArray ::
                                  Ptr a -> Int -> IO (VkPointSizeRangeArrayMType a)

        writeVkPointSizeRangeArray ::
                                   Ptr a -> Int -> VkPointSizeRangeArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pointSizeRange'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPointSizeRangeArray a

class HasVkPolygonMode a where
        type VkPolygonModeMType a :: *

        vkPolygonMode :: a -> VkPolygonModeMType a

        vkPolygonModeByteOffset :: a -> Int

        readVkPolygonMode :: Ptr a -> IO (VkPolygonModeMType a)

        writeVkPolygonMode :: Ptr a -> VkPolygonModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'polygonMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPolygonMode a

class HasVkPoolSizeCount a where
        type VkPoolSizeCountMType a :: *

        vkPoolSizeCount :: a -> VkPoolSizeCountMType a

        vkPoolSizeCountByteOffset :: a -> Int

        readVkPoolSizeCount :: Ptr a -> IO (VkPoolSizeCountMType a)

        writeVkPoolSizeCount :: Ptr a -> VkPoolSizeCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'poolSizeCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPoolSizeCount a

class HasVkPostSubpassSampleLocationsCount a where
        type VkPostSubpassSampleLocationsCountMType a :: *

        vkPostSubpassSampleLocationsCount ::
                                          a -> VkPostSubpassSampleLocationsCountMType a

        vkPostSubpassSampleLocationsCountByteOffset :: a -> Int

        readVkPostSubpassSampleLocationsCount ::
                                              Ptr a -> IO (VkPostSubpassSampleLocationsCountMType a)

        writeVkPostSubpassSampleLocationsCount ::
                                               Ptr a ->
                                                 VkPostSubpassSampleLocationsCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'postSubpassSampleLocationsCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPostSubpassSampleLocationsCount a

class HasVkPowerState a where
        type VkPowerStateMType a :: *

        vkPowerState :: a -> VkPowerStateMType a

        vkPowerStateByteOffset :: a -> Int

        readVkPowerState :: Ptr a -> IO (VkPowerStateMType a)

        writeVkPowerState :: Ptr a -> VkPowerStateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'powerState'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPowerState a

class HasVkPpEnabledExtensionNames a where
        type VkPpEnabledExtensionNamesMType a :: *

        vkPpEnabledExtensionNames :: a -> VkPpEnabledExtensionNamesMType a

        vkPpEnabledExtensionNamesByteOffset :: a -> Int

        readVkPpEnabledExtensionNames ::
                                      Ptr a -> IO (VkPpEnabledExtensionNamesMType a)

        writeVkPpEnabledExtensionNames ::
                                       Ptr a -> VkPpEnabledExtensionNamesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'ppEnabledExtensionNames'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPpEnabledExtensionNames a

class HasVkPpEnabledLayerNames a where
        type VkPpEnabledLayerNamesMType a :: *

        vkPpEnabledLayerNames :: a -> VkPpEnabledLayerNamesMType a

        vkPpEnabledLayerNamesByteOffset :: a -> Int

        readVkPpEnabledLayerNames ::
                                  Ptr a -> IO (VkPpEnabledLayerNamesMType a)

        writeVkPpEnabledLayerNames ::
                                   Ptr a -> VkPpEnabledLayerNamesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'ppEnabledLayerNames'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPpEnabledLayerNames a

class HasVkPreTransform a where
        type VkPreTransformMType a :: *

        vkPreTransform :: a -> VkPreTransformMType a

        vkPreTransformByteOffset :: a -> Int

        readVkPreTransform :: Ptr a -> IO (VkPreTransformMType a)

        writeVkPreTransform :: Ptr a -> VkPreTransformMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'preTransform'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPreTransform a

class HasVkPrefersDedicatedAllocation a where
        type VkPrefersDedicatedAllocationMType a :: *

        vkPrefersDedicatedAllocation ::
                                     a -> VkPrefersDedicatedAllocationMType a

        vkPrefersDedicatedAllocationByteOffset :: a -> Int

        readVkPrefersDedicatedAllocation ::
                                         Ptr a -> IO (VkPrefersDedicatedAllocationMType a)

        writeVkPrefersDedicatedAllocation ::
                                          Ptr a -> VkPrefersDedicatedAllocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'prefersDedicatedAllocation'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPrefersDedicatedAllocation a

class HasVkPresentID a where
        type VkPresentIDMType a :: *

        vkPresentID :: a -> VkPresentIDMType a

        vkPresentIDByteOffset :: a -> Int

        readVkPresentID :: Ptr a -> IO (VkPresentIDMType a)

        writeVkPresentID :: Ptr a -> VkPresentIDMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'presentID'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPresentID a

class HasVkPresentMargin a where
        type VkPresentMarginMType a :: *

        vkPresentMargin :: a -> VkPresentMarginMType a

        vkPresentMarginByteOffset :: a -> Int

        readVkPresentMargin :: Ptr a -> IO (VkPresentMarginMType a)

        writeVkPresentMargin :: Ptr a -> VkPresentMarginMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'presentMargin'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPresentMargin a

class HasVkPresentMaskArray a where
        type VkPresentMaskArrayMType a :: *

        vkPresentMaskArray :: a -> Int -> VkPresentMaskArrayMType a

        vkPresentMaskArrayByteOffset :: a -> Int

        readVkPresentMaskArray ::
                               Ptr a -> Int -> IO (VkPresentMaskArrayMType a)

        writeVkPresentMaskArray ::
                                Ptr a -> Int -> VkPresentMaskArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'presentMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPresentMaskArray a

class HasVkPresentMode a where
        type VkPresentModeMType a :: *

        vkPresentMode :: a -> VkPresentModeMType a

        vkPresentModeByteOffset :: a -> Int

        readVkPresentMode :: Ptr a -> IO (VkPresentModeMType a)

        writeVkPresentMode :: Ptr a -> VkPresentModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'presentMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPresentMode a

class HasVkPreserveAttachmentCount a where
        type VkPreserveAttachmentCountMType a :: *

        vkPreserveAttachmentCount :: a -> VkPreserveAttachmentCountMType a

        vkPreserveAttachmentCountByteOffset :: a -> Int

        readVkPreserveAttachmentCount ::
                                      Ptr a -> IO (VkPreserveAttachmentCountMType a)

        writeVkPreserveAttachmentCount ::
                                       Ptr a -> VkPreserveAttachmentCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'preserveAttachmentCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPreserveAttachmentCount a

class HasVkPrimitiveOverestimationSize a where
        type VkPrimitiveOverestimationSizeMType a :: *

        vkPrimitiveOverestimationSize ::
                                      a -> VkPrimitiveOverestimationSizeMType a

        vkPrimitiveOverestimationSizeByteOffset :: a -> Int

        readVkPrimitiveOverestimationSize ::
                                          Ptr a -> IO (VkPrimitiveOverestimationSizeMType a)

        writeVkPrimitiveOverestimationSize ::
                                           Ptr a -> VkPrimitiveOverestimationSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'primitiveOverestimationSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPrimitiveOverestimationSize a

class HasVkPrimitiveRestartEnable a where
        type VkPrimitiveRestartEnableMType a :: *

        vkPrimitiveRestartEnable :: a -> VkPrimitiveRestartEnableMType a

        vkPrimitiveRestartEnableByteOffset :: a -> Int

        readVkPrimitiveRestartEnable ::
                                     Ptr a -> IO (VkPrimitiveRestartEnableMType a)

        writeVkPrimitiveRestartEnable ::
                                      Ptr a -> VkPrimitiveRestartEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'primitiveRestartEnable'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPrimitiveRestartEnable a

class HasVkPrimitiveUnderestimation a where
        type VkPrimitiveUnderestimationMType a :: *

        vkPrimitiveUnderestimation ::
                                   a -> VkPrimitiveUnderestimationMType a

        vkPrimitiveUnderestimationByteOffset :: a -> Int

        readVkPrimitiveUnderestimation ::
                                       Ptr a -> IO (VkPrimitiveUnderestimationMType a)

        writeVkPrimitiveUnderestimation ::
                                        Ptr a -> VkPrimitiveUnderestimationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'primitiveUnderestimation'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPrimitiveUnderestimation a

class HasVkProperties a where
        type VkPropertiesMType a :: *

        vkProperties :: a -> VkPropertiesMType a

        vkPropertiesByteOffset :: a -> Int

        readVkProperties :: Ptr a -> IO (VkPropertiesMType a)

        writeVkProperties :: Ptr a -> VkPropertiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'properties'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkProperties a

class HasVkPropertyFlags a where
        type VkPropertyFlagsMType a :: *

        vkPropertyFlags :: a -> VkPropertyFlagsMType a

        vkPropertyFlagsByteOffset :: a -> Int

        readVkPropertyFlags :: Ptr a -> IO (VkPropertyFlagsMType a)

        writeVkPropertyFlags :: Ptr a -> VkPropertyFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'propertyFlags'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPropertyFlags a

class HasVkPushConstantRangeCount a where
        type VkPushConstantRangeCountMType a :: *

        vkPushConstantRangeCount :: a -> VkPushConstantRangeCountMType a

        vkPushConstantRangeCountByteOffset :: a -> Int

        readVkPushConstantRangeCount ::
                                     Ptr a -> IO (VkPushConstantRangeCountMType a)

        writeVkPushConstantRangeCount ::
                                      Ptr a -> VkPushConstantRangeCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'pushConstantRangeCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkPushConstantRangeCount a

class HasVkQueryCount a where
        type VkQueryCountMType a :: *

        vkQueryCount :: a -> VkQueryCountMType a

        vkQueryCountByteOffset :: a -> Int

        readVkQueryCount :: Ptr a -> IO (VkQueryCountMType a)

        writeVkQueryCount :: Ptr a -> VkQueryCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queryCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueryCount a

class HasVkQueryFlags a where
        type VkQueryFlagsMType a :: *

        vkQueryFlags :: a -> VkQueryFlagsMType a

        vkQueryFlagsByteOffset :: a -> Int

        readVkQueryFlags :: Ptr a -> IO (VkQueryFlagsMType a)

        writeVkQueryFlags :: Ptr a -> VkQueryFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queryFlags'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueryFlags a

class HasVkQueryType a where
        type VkQueryTypeMType a :: *

        vkQueryType :: a -> VkQueryTypeMType a

        vkQueryTypeByteOffset :: a -> Int

        readVkQueryType :: Ptr a -> IO (VkQueryTypeMType a)

        writeVkQueryType :: Ptr a -> VkQueryTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queryType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueryType a

class HasVkQueueCount a where
        type VkQueueCountMType a :: *

        vkQueueCount :: a -> VkQueueCountMType a

        vkQueueCountByteOffset :: a -> Int

        readVkQueueCount :: Ptr a -> IO (VkQueueCountMType a)

        writeVkQueueCount :: Ptr a -> VkQueueCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queueCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueueCount a

class HasVkQueueCreateInfoCount a where
        type VkQueueCreateInfoCountMType a :: *

        vkQueueCreateInfoCount :: a -> VkQueueCreateInfoCountMType a

        vkQueueCreateInfoCountByteOffset :: a -> Int

        readVkQueueCreateInfoCount ::
                                   Ptr a -> IO (VkQueueCreateInfoCountMType a)

        writeVkQueueCreateInfoCount ::
                                    Ptr a -> VkQueueCreateInfoCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queueCreateInfoCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueueCreateInfoCount a

class HasVkQueueFamilyIndex a where
        type VkQueueFamilyIndexMType a :: *

        vkQueueFamilyIndex :: a -> VkQueueFamilyIndexMType a

        vkQueueFamilyIndexByteOffset :: a -> Int

        readVkQueueFamilyIndex :: Ptr a -> IO (VkQueueFamilyIndexMType a)

        writeVkQueueFamilyIndex ::
                                Ptr a -> VkQueueFamilyIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queueFamilyIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueueFamilyIndex a

class HasVkQueueFamilyIndexCount a where
        type VkQueueFamilyIndexCountMType a :: *

        vkQueueFamilyIndexCount :: a -> VkQueueFamilyIndexCountMType a

        vkQueueFamilyIndexCountByteOffset :: a -> Int

        readVkQueueFamilyIndexCount ::
                                    Ptr a -> IO (VkQueueFamilyIndexCountMType a)

        writeVkQueueFamilyIndexCount ::
                                     Ptr a -> VkQueueFamilyIndexCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queueFamilyIndexCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueueFamilyIndexCount a

class HasVkQueueFamilyProperties a where
        type VkQueueFamilyPropertiesMType a :: *

        vkQueueFamilyProperties :: a -> VkQueueFamilyPropertiesMType a

        vkQueueFamilyPropertiesByteOffset :: a -> Int

        readVkQueueFamilyProperties ::
                                    Ptr a -> IO (VkQueueFamilyPropertiesMType a)

        writeVkQueueFamilyProperties ::
                                     Ptr a -> VkQueueFamilyPropertiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queueFamilyProperties'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueueFamilyProperties a

class HasVkQueueFlags a where
        type VkQueueFlagsMType a :: *

        vkQueueFlags :: a -> VkQueueFlagsMType a

        vkQueueFlagsByteOffset :: a -> Int

        readVkQueueFlags :: Ptr a -> IO (VkQueueFlagsMType a)

        writeVkQueueFlags :: Ptr a -> VkQueueFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'queueFlags'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkQueueFlags a

class HasVkR a where
        type VkRMType a :: *

        vkR :: a -> VkRMType a

        vkRByteOffset :: a -> Int

        readVkR :: Ptr a -> IO (VkRMType a)

        writeVkR :: Ptr a -> VkRMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'r'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkR a

class HasVkRange a where
        type VkRangeMType a :: *

        vkRange :: a -> VkRangeMType a

        vkRangeByteOffset :: a -> Int

        readVkRange :: Ptr a -> IO (VkRangeMType a)

        writeVkRange :: Ptr a -> VkRangeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'range'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRange a

class HasVkRasterizationOrder a where
        type VkRasterizationOrderMType a :: *

        vkRasterizationOrder :: a -> VkRasterizationOrderMType a

        vkRasterizationOrderByteOffset :: a -> Int

        readVkRasterizationOrder ::
                                 Ptr a -> IO (VkRasterizationOrderMType a)

        writeVkRasterizationOrder ::
                                  Ptr a -> VkRasterizationOrderMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'rasterizationOrder'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRasterizationOrder a

class HasVkRasterizationSamples a where
        type VkRasterizationSamplesMType a :: *

        vkRasterizationSamples :: a -> VkRasterizationSamplesMType a

        vkRasterizationSamplesByteOffset :: a -> Int

        readVkRasterizationSamples ::
                                   Ptr a -> IO (VkRasterizationSamplesMType a)

        writeVkRasterizationSamples ::
                                    Ptr a -> VkRasterizationSamplesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'rasterizationSamples'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRasterizationSamples a

class HasVkRasterizerDiscardEnable a where
        type VkRasterizerDiscardEnableMType a :: *

        vkRasterizerDiscardEnable :: a -> VkRasterizerDiscardEnableMType a

        vkRasterizerDiscardEnableByteOffset :: a -> Int

        readVkRasterizerDiscardEnable ::
                                      Ptr a -> IO (VkRasterizerDiscardEnableMType a)

        writeVkRasterizerDiscardEnable ::
                                       Ptr a -> VkRasterizerDiscardEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'rasterizerDiscardEnable'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRasterizerDiscardEnable a

class HasVkRect a where
        type VkRectMType a :: *

        vkRect :: a -> VkRectMType a

        vkRectByteOffset :: a -> Int

        readVkRect :: Ptr a -> IO (VkRectMType a)

        writeVkRect :: Ptr a -> VkRectMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'rect'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRect a

class HasVkRectangleCount a where
        type VkRectangleCountMType a :: *

        vkRectangleCount :: a -> VkRectangleCountMType a

        vkRectangleCountByteOffset :: a -> Int

        readVkRectangleCount :: Ptr a -> IO (VkRectangleCountMType a)

        writeVkRectangleCount :: Ptr a -> VkRectangleCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'rectangleCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRectangleCount a

class HasVkReductionMode a where
        type VkReductionModeMType a :: *

        vkReductionMode :: a -> VkReductionModeMType a

        vkReductionModeByteOffset :: a -> Int

        readVkReductionMode :: Ptr a -> IO (VkReductionModeMType a)

        writeVkReductionMode :: Ptr a -> VkReductionModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'reductionMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkReductionMode a

class HasVkReference a where
        type VkReferenceMType a :: *

        vkReference :: a -> VkReferenceMType a

        vkReferenceByteOffset :: a -> Int

        readVkReference :: Ptr a -> IO (VkReferenceMType a)

        writeVkReference :: Ptr a -> VkReferenceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'reference'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkReference a

class HasVkRefreshDuration a where
        type VkRefreshDurationMType a :: *

        vkRefreshDuration :: a -> VkRefreshDurationMType a

        vkRefreshDurationByteOffset :: a -> Int

        readVkRefreshDuration :: Ptr a -> IO (VkRefreshDurationMType a)

        writeVkRefreshDuration ::
                               Ptr a -> VkRefreshDurationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'refreshDuration'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRefreshDuration a

class HasVkRefreshRate a where
        type VkRefreshRateMType a :: *

        vkRefreshRate :: a -> VkRefreshRateMType a

        vkRefreshRateByteOffset :: a -> Int

        readVkRefreshRate :: Ptr a -> IO (VkRefreshRateMType a)

        writeVkRefreshRate :: Ptr a -> VkRefreshRateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'refreshRate'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRefreshRate a

class HasVkReleaseCount a where
        type VkReleaseCountMType a :: *

        vkReleaseCount :: a -> VkReleaseCountMType a

        vkReleaseCountByteOffset :: a -> Int

        readVkReleaseCount :: Ptr a -> IO (VkReleaseCountMType a)

        writeVkReleaseCount :: Ptr a -> VkReleaseCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'releaseCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkReleaseCount a

class HasVkRenderArea a where
        type VkRenderAreaMType a :: *

        vkRenderArea :: a -> VkRenderAreaMType a

        vkRenderAreaByteOffset :: a -> Int

        readVkRenderArea :: Ptr a -> IO (VkRenderAreaMType a)

        writeVkRenderArea :: Ptr a -> VkRenderAreaMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'renderArea'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRenderArea a

class HasVkRenderPass a where
        type VkRenderPassMType a :: *

        vkRenderPass :: a -> VkRenderPassMType a

        vkRenderPassByteOffset :: a -> Int

        readVkRenderPass :: Ptr a -> IO (VkRenderPassMType a)

        writeVkRenderPass :: Ptr a -> VkRenderPassMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'renderPass'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRenderPass a

class HasVkRequiresDedicatedAllocation a where
        type VkRequiresDedicatedAllocationMType a :: *

        vkRequiresDedicatedAllocation ::
                                      a -> VkRequiresDedicatedAllocationMType a

        vkRequiresDedicatedAllocationByteOffset :: a -> Int

        readVkRequiresDedicatedAllocation ::
                                          Ptr a -> IO (VkRequiresDedicatedAllocationMType a)

        writeVkRequiresDedicatedAllocation ::
                                           Ptr a -> VkRequiresDedicatedAllocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'requiresDedicatedAllocation'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRequiresDedicatedAllocation a

class HasVkResidencyAlignedMipSize a where
        type VkResidencyAlignedMipSizeMType a :: *

        vkResidencyAlignedMipSize :: a -> VkResidencyAlignedMipSizeMType a

        vkResidencyAlignedMipSizeByteOffset :: a -> Int

        readVkResidencyAlignedMipSize ::
                                      Ptr a -> IO (VkResidencyAlignedMipSizeMType a)

        writeVkResidencyAlignedMipSize ::
                                       Ptr a -> VkResidencyAlignedMipSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'residencyAlignedMipSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkResidencyAlignedMipSize a

class HasVkResidencyNonResidentStrict a where
        type VkResidencyNonResidentStrictMType a :: *

        vkResidencyNonResidentStrict ::
                                     a -> VkResidencyNonResidentStrictMType a

        vkResidencyNonResidentStrictByteOffset :: a -> Int

        readVkResidencyNonResidentStrict ::
                                         Ptr a -> IO (VkResidencyNonResidentStrictMType a)

        writeVkResidencyNonResidentStrict ::
                                          Ptr a -> VkResidencyNonResidentStrictMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'residencyNonResidentStrict'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkResidencyNonResidentStrict a

class HasVkResidencyStandard2DBlockShape a where
        type VkResidencyStandard2DBlockShapeMType a :: *

        vkResidencyStandard2DBlockShape ::
                                        a -> VkResidencyStandard2DBlockShapeMType a

        vkResidencyStandard2DBlockShapeByteOffset :: a -> Int

        readVkResidencyStandard2DBlockShape ::
                                            Ptr a -> IO (VkResidencyStandard2DBlockShapeMType a)

        writeVkResidencyStandard2DBlockShape ::
                                             Ptr a ->
                                               VkResidencyStandard2DBlockShapeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'residencyStandard2DBlockShape'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkResidencyStandard2DBlockShape a

class HasVkResidencyStandard2DMultisampleBlockShape a where
        type VkResidencyStandard2DMultisampleBlockShapeMType a :: *

        vkResidencyStandard2DMultisampleBlockShape ::
                                                   a ->
                                                     VkResidencyStandard2DMultisampleBlockShapeMType
                                                       a

        vkResidencyStandard2DMultisampleBlockShapeByteOffset :: a -> Int

        readVkResidencyStandard2DMultisampleBlockShape ::
                                                       Ptr a ->
                                                         IO
                                                           (VkResidencyStandard2DMultisampleBlockShapeMType
                                                              a)

        writeVkResidencyStandard2DMultisampleBlockShape ::
                                                        Ptr a ->
                                                          VkResidencyStandard2DMultisampleBlockShapeMType
                                                            a
                                                            -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'residencyStandard2DMultisampleBlockShape'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkResidencyStandard2DMultisampleBlockShape a

class HasVkResidencyStandard3DBlockShape a where
        type VkResidencyStandard3DBlockShapeMType a :: *

        vkResidencyStandard3DBlockShape ::
                                        a -> VkResidencyStandard3DBlockShapeMType a

        vkResidencyStandard3DBlockShapeByteOffset :: a -> Int

        readVkResidencyStandard3DBlockShape ::
                                            Ptr a -> IO (VkResidencyStandard3DBlockShapeMType a)

        writeVkResidencyStandard3DBlockShape ::
                                             Ptr a ->
                                               VkResidencyStandard3DBlockShapeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'residencyStandard3DBlockShape'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkResidencyStandard3DBlockShape a

class HasVkResourceDeviceIndex a where
        type VkResourceDeviceIndexMType a :: *

        vkResourceDeviceIndex :: a -> VkResourceDeviceIndexMType a

        vkResourceDeviceIndexByteOffset :: a -> Int

        readVkResourceDeviceIndex ::
                                  Ptr a -> IO (VkResourceDeviceIndexMType a)

        writeVkResourceDeviceIndex ::
                                   Ptr a -> VkResourceDeviceIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'resourceDeviceIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkResourceDeviceIndex a

class HasVkResourceOffset a where
        type VkResourceOffsetMType a :: *

        vkResourceOffset :: a -> VkResourceOffsetMType a

        vkResourceOffsetByteOffset :: a -> Int

        readVkResourceOffset :: Ptr a -> IO (VkResourceOffsetMType a)

        writeVkResourceOffset :: Ptr a -> VkResourceOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'resourceOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkResourceOffset a

class HasVkResourceUsage a where
        type VkResourceUsageMType a :: *

        vkResourceUsage :: a -> VkResourceUsageMType a

        vkResourceUsageByteOffset :: a -> Int

        readVkResourceUsage :: Ptr a -> IO (VkResourceUsageMType a)

        writeVkResourceUsage :: Ptr a -> VkResourceUsageMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'resourceUsage'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkResourceUsage a

class HasVkRobustBufferAccess a where
        type VkRobustBufferAccessMType a :: *

        vkRobustBufferAccess :: a -> VkRobustBufferAccessMType a

        vkRobustBufferAccessByteOffset :: a -> Int

        readVkRobustBufferAccess ::
                                 Ptr a -> IO (VkRobustBufferAccessMType a)

        writeVkRobustBufferAccess ::
                                  Ptr a -> VkRobustBufferAccessMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'robustBufferAccess'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRobustBufferAccess a

class HasVkRowPitch a where
        type VkRowPitchMType a :: *

        vkRowPitch :: a -> VkRowPitchMType a

        vkRowPitchByteOffset :: a -> Int

        readVkRowPitch :: Ptr a -> IO (VkRowPitchMType a)

        writeVkRowPitch :: Ptr a -> VkRowPitchMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'rowPitch'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkRowPitch a

class HasVkSFRRectCount a where
        type VkSFRRectCountMType a :: *

        vkSFRRectCount :: a -> VkSFRRectCountMType a

        vkSFRRectCountByteOffset :: a -> Int

        readVkSFRRectCount :: Ptr a -> IO (VkSFRRectCountMType a)

        writeVkSFRRectCount :: Ptr a -> VkSFRRectCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'SFRRectCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSFRRectCount a

class HasVkSType a where
        type VkSTypeMType a :: *

        vkSType :: a -> VkSTypeMType a

        vkSTypeByteOffset :: a -> Int

        readVkSType :: Ptr a -> IO (VkSTypeMType a)

        writeVkSType :: Ptr a -> VkSTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSType a

class HasVkSampleCounts a where
        type VkSampleCountsMType a :: *

        vkSampleCounts :: a -> VkSampleCountsMType a

        vkSampleCountsByteOffset :: a -> Int

        readVkSampleCounts :: Ptr a -> IO (VkSampleCountsMType a)

        writeVkSampleCounts :: Ptr a -> VkSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleCounts'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleCounts a

class HasVkSampleLocationCoordinateRangeArray a where
        type VkSampleLocationCoordinateRangeArrayMType a :: *

        vkSampleLocationCoordinateRangeArray ::
                                             a -> Int -> VkSampleLocationCoordinateRangeArrayMType a

        vkSampleLocationCoordinateRangeArrayByteOffset :: a -> Int

        readVkSampleLocationCoordinateRangeArray ::
                                                 Ptr a ->
                                                   Int ->
                                                     IO
                                                       (VkSampleLocationCoordinateRangeArrayMType a)

        writeVkSampleLocationCoordinateRangeArray ::
                                                  Ptr a ->
                                                    Int ->
                                                      VkSampleLocationCoordinateRangeArrayMType a ->
                                                        IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'sampleLocationCoordinateRange'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleLocationCoordinateRangeArray a

class HasVkSampleLocationGridSize a where
        type VkSampleLocationGridSizeMType a :: *

        vkSampleLocationGridSize :: a -> VkSampleLocationGridSizeMType a

        vkSampleLocationGridSizeByteOffset :: a -> Int

        readVkSampleLocationGridSize ::
                                     Ptr a -> IO (VkSampleLocationGridSizeMType a)

        writeVkSampleLocationGridSize ::
                                      Ptr a -> VkSampleLocationGridSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleLocationGridSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleLocationGridSize a

class HasVkSampleLocationSampleCounts a where
        type VkSampleLocationSampleCountsMType a :: *

        vkSampleLocationSampleCounts ::
                                     a -> VkSampleLocationSampleCountsMType a

        vkSampleLocationSampleCountsByteOffset :: a -> Int

        readVkSampleLocationSampleCounts ::
                                         Ptr a -> IO (VkSampleLocationSampleCountsMType a)

        writeVkSampleLocationSampleCounts ::
                                          Ptr a -> VkSampleLocationSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleLocationSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleLocationSampleCounts a

class HasVkSampleLocationSubPixelBits a where
        type VkSampleLocationSubPixelBitsMType a :: *

        vkSampleLocationSubPixelBits ::
                                     a -> VkSampleLocationSubPixelBitsMType a

        vkSampleLocationSubPixelBitsByteOffset :: a -> Int

        readVkSampleLocationSubPixelBits ::
                                         Ptr a -> IO (VkSampleLocationSubPixelBitsMType a)

        writeVkSampleLocationSubPixelBits ::
                                          Ptr a -> VkSampleLocationSubPixelBitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleLocationSubPixelBits'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleLocationSubPixelBits a

class HasVkSampleLocationsCount a where
        type VkSampleLocationsCountMType a :: *

        vkSampleLocationsCount :: a -> VkSampleLocationsCountMType a

        vkSampleLocationsCountByteOffset :: a -> Int

        readVkSampleLocationsCount ::
                                   Ptr a -> IO (VkSampleLocationsCountMType a)

        writeVkSampleLocationsCount ::
                                    Ptr a -> VkSampleLocationsCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleLocationsCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleLocationsCount a

class HasVkSampleLocationsEnable a where
        type VkSampleLocationsEnableMType a :: *

        vkSampleLocationsEnable :: a -> VkSampleLocationsEnableMType a

        vkSampleLocationsEnableByteOffset :: a -> Int

        readVkSampleLocationsEnable ::
                                    Ptr a -> IO (VkSampleLocationsEnableMType a)

        writeVkSampleLocationsEnable ::
                                     Ptr a -> VkSampleLocationsEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleLocationsEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleLocationsEnable a

class HasVkSampleLocationsInfo a where
        type VkSampleLocationsInfoMType a :: *

        vkSampleLocationsInfo :: a -> VkSampleLocationsInfoMType a

        vkSampleLocationsInfoByteOffset :: a -> Int

        readVkSampleLocationsInfo ::
                                  Ptr a -> IO (VkSampleLocationsInfoMType a)

        writeVkSampleLocationsInfo ::
                                   Ptr a -> VkSampleLocationsInfoMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleLocationsInfo'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleLocationsInfo a

class HasVkSampleLocationsPerPixel a where
        type VkSampleLocationsPerPixelMType a :: *

        vkSampleLocationsPerPixel :: a -> VkSampleLocationsPerPixelMType a

        vkSampleLocationsPerPixelByteOffset :: a -> Int

        readVkSampleLocationsPerPixel ::
                                      Ptr a -> IO (VkSampleLocationsPerPixelMType a)

        writeVkSampleLocationsPerPixel ::
                                       Ptr a -> VkSampleLocationsPerPixelMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleLocationsPerPixel'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleLocationsPerPixel a

class HasVkSampleRateShading a where
        type VkSampleRateShadingMType a :: *

        vkSampleRateShading :: a -> VkSampleRateShadingMType a

        vkSampleRateShadingByteOffset :: a -> Int

        readVkSampleRateShading :: Ptr a -> IO (VkSampleRateShadingMType a)

        writeVkSampleRateShading ::
                                 Ptr a -> VkSampleRateShadingMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleRateShading'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleRateShading a

class HasVkSampleShadingEnable a where
        type VkSampleShadingEnableMType a :: *

        vkSampleShadingEnable :: a -> VkSampleShadingEnableMType a

        vkSampleShadingEnableByteOffset :: a -> Int

        readVkSampleShadingEnable ::
                                  Ptr a -> IO (VkSampleShadingEnableMType a)

        writeVkSampleShadingEnable ::
                                   Ptr a -> VkSampleShadingEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampleShadingEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampleShadingEnable a

class HasVkSampledImageColorSampleCounts a where
        type VkSampledImageColorSampleCountsMType a :: *

        vkSampledImageColorSampleCounts ::
                                        a -> VkSampledImageColorSampleCountsMType a

        vkSampledImageColorSampleCountsByteOffset :: a -> Int

        readVkSampledImageColorSampleCounts ::
                                            Ptr a -> IO (VkSampledImageColorSampleCountsMType a)

        writeVkSampledImageColorSampleCounts ::
                                             Ptr a ->
                                               VkSampledImageColorSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'sampledImageColorSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampledImageColorSampleCounts a

class HasVkSampledImageDepthSampleCounts a where
        type VkSampledImageDepthSampleCountsMType a :: *

        vkSampledImageDepthSampleCounts ::
                                        a -> VkSampledImageDepthSampleCountsMType a

        vkSampledImageDepthSampleCountsByteOffset :: a -> Int

        readVkSampledImageDepthSampleCounts ::
                                            Ptr a -> IO (VkSampledImageDepthSampleCountsMType a)

        writeVkSampledImageDepthSampleCounts ::
                                             Ptr a ->
                                               VkSampledImageDepthSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'sampledImageDepthSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampledImageDepthSampleCounts a

class HasVkSampledImageIntegerSampleCounts a where
        type VkSampledImageIntegerSampleCountsMType a :: *

        vkSampledImageIntegerSampleCounts ::
                                          a -> VkSampledImageIntegerSampleCountsMType a

        vkSampledImageIntegerSampleCountsByteOffset :: a -> Int

        readVkSampledImageIntegerSampleCounts ::
                                              Ptr a -> IO (VkSampledImageIntegerSampleCountsMType a)

        writeVkSampledImageIntegerSampleCounts ::
                                               Ptr a ->
                                                 VkSampledImageIntegerSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'sampledImageIntegerSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampledImageIntegerSampleCounts a

class HasVkSampledImageStencilSampleCounts a where
        type VkSampledImageStencilSampleCountsMType a :: *

        vkSampledImageStencilSampleCounts ::
                                          a -> VkSampledImageStencilSampleCountsMType a

        vkSampledImageStencilSampleCountsByteOffset :: a -> Int

        readVkSampledImageStencilSampleCounts ::
                                              Ptr a -> IO (VkSampledImageStencilSampleCountsMType a)

        writeVkSampledImageStencilSampleCounts ::
                                               Ptr a ->
                                                 VkSampledImageStencilSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'sampledImageStencilSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampledImageStencilSampleCounts a

class HasVkSampler a where
        type VkSamplerMType a :: *

        vkSampler :: a -> VkSamplerMType a

        vkSamplerByteOffset :: a -> Int

        readVkSampler :: Ptr a -> IO (VkSamplerMType a)

        writeVkSampler :: Ptr a -> VkSamplerMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sampler'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSampler a

class HasVkSamplerAnisotropy a where
        type VkSamplerAnisotropyMType a :: *

        vkSamplerAnisotropy :: a -> VkSamplerAnisotropyMType a

        vkSamplerAnisotropyByteOffset :: a -> Int

        readVkSamplerAnisotropy :: Ptr a -> IO (VkSamplerAnisotropyMType a)

        writeVkSamplerAnisotropy ::
                                 Ptr a -> VkSamplerAnisotropyMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'samplerAnisotropy'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSamplerAnisotropy a

class HasVkSamplerYcbcrConversion a where
        type VkSamplerYcbcrConversionMType a :: *

        vkSamplerYcbcrConversion :: a -> VkSamplerYcbcrConversionMType a

        vkSamplerYcbcrConversionByteOffset :: a -> Int

        readVkSamplerYcbcrConversion ::
                                     Ptr a -> IO (VkSamplerYcbcrConversionMType a)

        writeVkSamplerYcbcrConversion ::
                                      Ptr a -> VkSamplerYcbcrConversionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'samplerYcbcrConversion'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSamplerYcbcrConversion a

class HasVkSamples a where
        type VkSamplesMType a :: *

        vkSamples :: a -> VkSamplesMType a

        vkSamplesByteOffset :: a -> Int

        readVkSamples :: Ptr a -> IO (VkSamplesMType a)

        writeVkSamples :: Ptr a -> VkSamplesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'samples'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSamples a

class HasVkScissorCount a where
        type VkScissorCountMType a :: *

        vkScissorCount :: a -> VkScissorCountMType a

        vkScissorCountByteOffset :: a -> Int

        readVkScissorCount :: Ptr a -> IO (VkScissorCountMType a)

        writeVkScissorCount :: Ptr a -> VkScissorCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'scissorCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkScissorCount a

class HasVkScratchMemUsageInBytes a where
        type VkScratchMemUsageInBytesMType a :: *

        vkScratchMemUsageInBytes :: a -> VkScratchMemUsageInBytesMType a

        vkScratchMemUsageInBytesByteOffset :: a -> Int

        readVkScratchMemUsageInBytes ::
                                     Ptr a -> IO (VkScratchMemUsageInBytesMType a)

        writeVkScratchMemUsageInBytes ::
                                      Ptr a -> VkScratchMemUsageInBytesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'scratchMemUsageInBytes'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkScratchMemUsageInBytes a

class HasVkSemaphore a where
        type VkSemaphoreMType a :: *

        vkSemaphore :: a -> VkSemaphoreMType a

        vkSemaphoreByteOffset :: a -> Int

        readVkSemaphore :: Ptr a -> IO (VkSemaphoreMType a)

        writeVkSemaphore :: Ptr a -> VkSemaphoreMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'semaphore'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSemaphore a

class HasVkSequencesCountBuffer a where
        type VkSequencesCountBufferMType a :: *

        vkSequencesCountBuffer :: a -> VkSequencesCountBufferMType a

        vkSequencesCountBufferByteOffset :: a -> Int

        readVkSequencesCountBuffer ::
                                   Ptr a -> IO (VkSequencesCountBufferMType a)

        writeVkSequencesCountBuffer ::
                                    Ptr a -> VkSequencesCountBufferMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sequencesCountBuffer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSequencesCountBuffer a

class HasVkSequencesCountOffset a where
        type VkSequencesCountOffsetMType a :: *

        vkSequencesCountOffset :: a -> VkSequencesCountOffsetMType a

        vkSequencesCountOffsetByteOffset :: a -> Int

        readVkSequencesCountOffset ::
                                   Ptr a -> IO (VkSequencesCountOffsetMType a)

        writeVkSequencesCountOffset ::
                                    Ptr a -> VkSequencesCountOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sequencesCountOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSequencesCountOffset a

class HasVkSequencesIndexBuffer a where
        type VkSequencesIndexBufferMType a :: *

        vkSequencesIndexBuffer :: a -> VkSequencesIndexBufferMType a

        vkSequencesIndexBufferByteOffset :: a -> Int

        readVkSequencesIndexBuffer ::
                                   Ptr a -> IO (VkSequencesIndexBufferMType a)

        writeVkSequencesIndexBuffer ::
                                    Ptr a -> VkSequencesIndexBufferMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sequencesIndexBuffer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSequencesIndexBuffer a

class HasVkSequencesIndexOffset a where
        type VkSequencesIndexOffsetMType a :: *

        vkSequencesIndexOffset :: a -> VkSequencesIndexOffsetMType a

        vkSequencesIndexOffsetByteOffset :: a -> Int

        readVkSequencesIndexOffset ::
                                   Ptr a -> IO (VkSequencesIndexOffsetMType a)

        writeVkSequencesIndexOffset ::
                                    Ptr a -> VkSequencesIndexOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sequencesIndexOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSequencesIndexOffset a

class HasVkSet a where
        type VkSetMType a :: *

        vkSet :: a -> VkSetMType a

        vkSetByteOffset :: a -> Int

        readVkSet :: Ptr a -> IO (VkSetMType a)

        writeVkSet :: Ptr a -> VkSetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'set'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSet a

class HasVkSetLayoutCount a where
        type VkSetLayoutCountMType a :: *

        vkSetLayoutCount :: a -> VkSetLayoutCountMType a

        vkSetLayoutCountByteOffset :: a -> Int

        readVkSetLayoutCount :: Ptr a -> IO (VkSetLayoutCountMType a)

        writeVkSetLayoutCount :: Ptr a -> VkSetLayoutCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'setLayoutCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSetLayoutCount a

class HasVkShaderClipDistance a where
        type VkShaderClipDistanceMType a :: *

        vkShaderClipDistance :: a -> VkShaderClipDistanceMType a

        vkShaderClipDistanceByteOffset :: a -> Int

        readVkShaderClipDistance ::
                                 Ptr a -> IO (VkShaderClipDistanceMType a)

        writeVkShaderClipDistance ::
                                  Ptr a -> VkShaderClipDistanceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderClipDistance'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderClipDistance a

class HasVkShaderCullDistance a where
        type VkShaderCullDistanceMType a :: *

        vkShaderCullDistance :: a -> VkShaderCullDistanceMType a

        vkShaderCullDistanceByteOffset :: a -> Int

        readVkShaderCullDistance ::
                                 Ptr a -> IO (VkShaderCullDistanceMType a)

        writeVkShaderCullDistance ::
                                  Ptr a -> VkShaderCullDistanceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderCullDistance'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderCullDistance a

class HasVkShaderFloat64 a where
        type VkShaderFloat64MType a :: *

        vkShaderFloat64 :: a -> VkShaderFloat64MType a

        vkShaderFloat64ByteOffset :: a -> Int

        readVkShaderFloat64 :: Ptr a -> IO (VkShaderFloat64MType a)

        writeVkShaderFloat64 :: Ptr a -> VkShaderFloat64MType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderFloat64'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderFloat64 a

class HasVkShaderImageGatherExtended a where
        type VkShaderImageGatherExtendedMType a :: *

        vkShaderImageGatherExtended ::
                                    a -> VkShaderImageGatherExtendedMType a

        vkShaderImageGatherExtendedByteOffset :: a -> Int

        readVkShaderImageGatherExtended ::
                                        Ptr a -> IO (VkShaderImageGatherExtendedMType a)

        writeVkShaderImageGatherExtended ::
                                         Ptr a -> VkShaderImageGatherExtendedMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderImageGatherExtended'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderImageGatherExtended a

class HasVkShaderInt16 a where
        type VkShaderInt16MType a :: *

        vkShaderInt16 :: a -> VkShaderInt16MType a

        vkShaderInt16ByteOffset :: a -> Int

        readVkShaderInt16 :: Ptr a -> IO (VkShaderInt16MType a)

        writeVkShaderInt16 :: Ptr a -> VkShaderInt16MType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderInt16'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderInt16 a

class HasVkShaderInt64 a where
        type VkShaderInt64MType a :: *

        vkShaderInt64 :: a -> VkShaderInt64MType a

        vkShaderInt64ByteOffset :: a -> Int

        readVkShaderInt64 :: Ptr a -> IO (VkShaderInt64MType a)

        writeVkShaderInt64 :: Ptr a -> VkShaderInt64MType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderInt64'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderInt64 a

class HasVkShaderResourceMinLod a where
        type VkShaderResourceMinLodMType a :: *

        vkShaderResourceMinLod :: a -> VkShaderResourceMinLodMType a

        vkShaderResourceMinLodByteOffset :: a -> Int

        readVkShaderResourceMinLod ::
                                   Ptr a -> IO (VkShaderResourceMinLodMType a)

        writeVkShaderResourceMinLod ::
                                    Ptr a -> VkShaderResourceMinLodMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderResourceMinLod'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderResourceMinLod a

class HasVkShaderResourceResidency a where
        type VkShaderResourceResidencyMType a :: *

        vkShaderResourceResidency :: a -> VkShaderResourceResidencyMType a

        vkShaderResourceResidencyByteOffset :: a -> Int

        readVkShaderResourceResidency ::
                                      Ptr a -> IO (VkShaderResourceResidencyMType a)

        writeVkShaderResourceResidency ::
                                       Ptr a -> VkShaderResourceResidencyMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderResourceResidency'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderResourceResidency a

class HasVkShaderSampledImageArrayDynamicIndexing a where
        type VkShaderSampledImageArrayDynamicIndexingMType a :: *

        vkShaderSampledImageArrayDynamicIndexing ::
                                                 a ->
                                                   VkShaderSampledImageArrayDynamicIndexingMType a

        vkShaderSampledImageArrayDynamicIndexingByteOffset :: a -> Int

        readVkShaderSampledImageArrayDynamicIndexing ::
                                                     Ptr a ->
                                                       IO
                                                         (VkShaderSampledImageArrayDynamicIndexingMType
                                                            a)

        writeVkShaderSampledImageArrayDynamicIndexing ::
                                                      Ptr a ->
                                                        VkShaderSampledImageArrayDynamicIndexingMType
                                                          a
                                                          -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderSampledImageArrayDynamicIndexing'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderSampledImageArrayDynamicIndexing a

class HasVkShaderStageMask a where
        type VkShaderStageMaskMType a :: *

        vkShaderStageMask :: a -> VkShaderStageMaskMType a

        vkShaderStageMaskByteOffset :: a -> Int

        readVkShaderStageMask :: Ptr a -> IO (VkShaderStageMaskMType a)

        writeVkShaderStageMask ::
                               Ptr a -> VkShaderStageMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'shaderStageMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderStageMask a

class HasVkShaderStorageBufferArrayDynamicIndexing a where
        type VkShaderStorageBufferArrayDynamicIndexingMType a :: *

        vkShaderStorageBufferArrayDynamicIndexing ::
                                                  a ->
                                                    VkShaderStorageBufferArrayDynamicIndexingMType a

        vkShaderStorageBufferArrayDynamicIndexingByteOffset :: a -> Int

        readVkShaderStorageBufferArrayDynamicIndexing ::
                                                      Ptr a ->
                                                        IO
                                                          (VkShaderStorageBufferArrayDynamicIndexingMType
                                                             a)

        writeVkShaderStorageBufferArrayDynamicIndexing ::
                                                       Ptr a ->
                                                         VkShaderStorageBufferArrayDynamicIndexingMType
                                                           a
                                                           -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderStorageBufferArrayDynamicIndexing'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderStorageBufferArrayDynamicIndexing a

class HasVkShaderStorageImageArrayDynamicIndexing a where
        type VkShaderStorageImageArrayDynamicIndexingMType a :: *

        vkShaderStorageImageArrayDynamicIndexing ::
                                                 a ->
                                                   VkShaderStorageImageArrayDynamicIndexingMType a

        vkShaderStorageImageArrayDynamicIndexingByteOffset :: a -> Int

        readVkShaderStorageImageArrayDynamicIndexing ::
                                                     Ptr a ->
                                                       IO
                                                         (VkShaderStorageImageArrayDynamicIndexingMType
                                                            a)

        writeVkShaderStorageImageArrayDynamicIndexing ::
                                                      Ptr a ->
                                                        VkShaderStorageImageArrayDynamicIndexingMType
                                                          a
                                                          -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderStorageImageArrayDynamicIndexing'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderStorageImageArrayDynamicIndexing a

class HasVkShaderStorageImageExtendedFormats a where
        type VkShaderStorageImageExtendedFormatsMType a :: *

        vkShaderStorageImageExtendedFormats ::
                                            a -> VkShaderStorageImageExtendedFormatsMType a

        vkShaderStorageImageExtendedFormatsByteOffset :: a -> Int

        readVkShaderStorageImageExtendedFormats ::
                                                Ptr a ->
                                                  IO (VkShaderStorageImageExtendedFormatsMType a)

        writeVkShaderStorageImageExtendedFormats ::
                                                 Ptr a ->
                                                   VkShaderStorageImageExtendedFormatsMType a ->
                                                     IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderStorageImageExtendedFormats'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderStorageImageExtendedFormats a

class HasVkShaderStorageImageMultisample a where
        type VkShaderStorageImageMultisampleMType a :: *

        vkShaderStorageImageMultisample ::
                                        a -> VkShaderStorageImageMultisampleMType a

        vkShaderStorageImageMultisampleByteOffset :: a -> Int

        readVkShaderStorageImageMultisample ::
                                            Ptr a -> IO (VkShaderStorageImageMultisampleMType a)

        writeVkShaderStorageImageMultisample ::
                                             Ptr a ->
                                               VkShaderStorageImageMultisampleMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderStorageImageMultisample'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderStorageImageMultisample a

class HasVkShaderStorageImageReadWithoutFormat a where
        type VkShaderStorageImageReadWithoutFormatMType a :: *

        vkShaderStorageImageReadWithoutFormat ::
                                              a -> VkShaderStorageImageReadWithoutFormatMType a

        vkShaderStorageImageReadWithoutFormatByteOffset :: a -> Int

        readVkShaderStorageImageReadWithoutFormat ::
                                                  Ptr a ->
                                                    IO
                                                      (VkShaderStorageImageReadWithoutFormatMType a)

        writeVkShaderStorageImageReadWithoutFormat ::
                                                   Ptr a ->
                                                     VkShaderStorageImageReadWithoutFormatMType a ->
                                                       IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderStorageImageReadWithoutFormat'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderStorageImageReadWithoutFormat a

class HasVkShaderStorageImageWriteWithoutFormat a where
        type VkShaderStorageImageWriteWithoutFormatMType a :: *

        vkShaderStorageImageWriteWithoutFormat ::
                                               a -> VkShaderStorageImageWriteWithoutFormatMType a

        vkShaderStorageImageWriteWithoutFormatByteOffset :: a -> Int

        readVkShaderStorageImageWriteWithoutFormat ::
                                                   Ptr a ->
                                                     IO
                                                       (VkShaderStorageImageWriteWithoutFormatMType
                                                          a)

        writeVkShaderStorageImageWriteWithoutFormat ::
                                                    Ptr a ->
                                                      VkShaderStorageImageWriteWithoutFormatMType a
                                                        -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderStorageImageWriteWithoutFormat'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderStorageImageWriteWithoutFormat a

class HasVkShaderTessellationAndGeometryPointSize a where
        type VkShaderTessellationAndGeometryPointSizeMType a :: *

        vkShaderTessellationAndGeometryPointSize ::
                                                 a ->
                                                   VkShaderTessellationAndGeometryPointSizeMType a

        vkShaderTessellationAndGeometryPointSizeByteOffset :: a -> Int

        readVkShaderTessellationAndGeometryPointSize ::
                                                     Ptr a ->
                                                       IO
                                                         (VkShaderTessellationAndGeometryPointSizeMType
                                                            a)

        writeVkShaderTessellationAndGeometryPointSize ::
                                                      Ptr a ->
                                                        VkShaderTessellationAndGeometryPointSizeMType
                                                          a
                                                          -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderTessellationAndGeometryPointSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderTessellationAndGeometryPointSize a

class HasVkShaderUniformBufferArrayDynamicIndexing a where
        type VkShaderUniformBufferArrayDynamicIndexingMType a :: *

        vkShaderUniformBufferArrayDynamicIndexing ::
                                                  a ->
                                                    VkShaderUniformBufferArrayDynamicIndexingMType a

        vkShaderUniformBufferArrayDynamicIndexingByteOffset :: a -> Int

        readVkShaderUniformBufferArrayDynamicIndexing ::
                                                      Ptr a ->
                                                        IO
                                                          (VkShaderUniformBufferArrayDynamicIndexingMType
                                                             a)

        writeVkShaderUniformBufferArrayDynamicIndexing ::
                                                       Ptr a ->
                                                         VkShaderUniformBufferArrayDynamicIndexingMType
                                                           a
                                                           -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'shaderUniformBufferArrayDynamicIndexing'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkShaderUniformBufferArrayDynamicIndexing a

class HasVkSharedPresentSupportedUsageFlags a where
        type VkSharedPresentSupportedUsageFlagsMType a :: *

        vkSharedPresentSupportedUsageFlags ::
                                           a -> VkSharedPresentSupportedUsageFlagsMType a

        vkSharedPresentSupportedUsageFlagsByteOffset :: a -> Int

        readVkSharedPresentSupportedUsageFlags ::
                                               Ptr a ->
                                                 IO (VkSharedPresentSupportedUsageFlagsMType a)

        writeVkSharedPresentSupportedUsageFlags ::
                                                Ptr a ->
                                                  VkSharedPresentSupportedUsageFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'sharedPresentSupportedUsageFlags'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSharedPresentSupportedUsageFlags a

class HasVkSharingMode a where
        type VkSharingModeMType a :: *

        vkSharingMode :: a -> VkSharingModeMType a

        vkSharingModeByteOffset :: a -> Int

        readVkSharingMode :: Ptr a -> IO (VkSharingModeMType a)

        writeVkSharingMode :: Ptr a -> VkSharingModeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sharingMode'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSharingMode a

class HasVkSignalSemaphoreCount a where
        type VkSignalSemaphoreCountMType a :: *

        vkSignalSemaphoreCount :: a -> VkSignalSemaphoreCountMType a

        vkSignalSemaphoreCountByteOffset :: a -> Int

        readVkSignalSemaphoreCount ::
                                   Ptr a -> IO (VkSignalSemaphoreCountMType a)

        writeVkSignalSemaphoreCount ::
                                    Ptr a -> VkSignalSemaphoreCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'signalSemaphoreCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSignalSemaphoreCount a

class HasVkSignalSemaphoreValuesCount a where
        type VkSignalSemaphoreValuesCountMType a :: *

        vkSignalSemaphoreValuesCount ::
                                     a -> VkSignalSemaphoreValuesCountMType a

        vkSignalSemaphoreValuesCountByteOffset :: a -> Int

        readVkSignalSemaphoreValuesCount ::
                                         Ptr a -> IO (VkSignalSemaphoreValuesCountMType a)

        writeVkSignalSemaphoreValuesCount ::
                                          Ptr a -> VkSignalSemaphoreValuesCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'signalSemaphoreValuesCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSignalSemaphoreValuesCount a

class HasVkSize a where
        type VkSizeMType a :: *

        vkSize :: a -> VkSizeMType a

        vkSizeByteOffset :: a -> Int

        readVkSize :: Ptr a -> IO (VkSizeMType a)

        writeVkSize :: Ptr a -> VkSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'size'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSize a

class HasVkSparseAddressSpaceSize a where
        type VkSparseAddressSpaceSizeMType a :: *

        vkSparseAddressSpaceSize :: a -> VkSparseAddressSpaceSizeMType a

        vkSparseAddressSpaceSizeByteOffset :: a -> Int

        readVkSparseAddressSpaceSize ::
                                     Ptr a -> IO (VkSparseAddressSpaceSizeMType a)

        writeVkSparseAddressSpaceSize ::
                                      Ptr a -> VkSparseAddressSpaceSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseAddressSpaceSize'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseAddressSpaceSize a

class HasVkSparseBinding a where
        type VkSparseBindingMType a :: *

        vkSparseBinding :: a -> VkSparseBindingMType a

        vkSparseBindingByteOffset :: a -> Int

        readVkSparseBinding :: Ptr a -> IO (VkSparseBindingMType a)

        writeVkSparseBinding :: Ptr a -> VkSparseBindingMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseBinding'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseBinding a

class HasVkSparseProperties a where
        type VkSparsePropertiesMType a :: *

        vkSparseProperties :: a -> VkSparsePropertiesMType a

        vkSparsePropertiesByteOffset :: a -> Int

        readVkSparseProperties :: Ptr a -> IO (VkSparsePropertiesMType a)

        writeVkSparseProperties ::
                                Ptr a -> VkSparsePropertiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseProperties'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseProperties a

class HasVkSparseResidency16Samples a where
        type VkSparseResidency16SamplesMType a :: *

        vkSparseResidency16Samples ::
                                   a -> VkSparseResidency16SamplesMType a

        vkSparseResidency16SamplesByteOffset :: a -> Int

        readVkSparseResidency16Samples ::
                                       Ptr a -> IO (VkSparseResidency16SamplesMType a)

        writeVkSparseResidency16Samples ::
                                        Ptr a -> VkSparseResidency16SamplesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseResidency16Samples'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseResidency16Samples a

class HasVkSparseResidency2Samples a where
        type VkSparseResidency2SamplesMType a :: *

        vkSparseResidency2Samples :: a -> VkSparseResidency2SamplesMType a

        vkSparseResidency2SamplesByteOffset :: a -> Int

        readVkSparseResidency2Samples ::
                                      Ptr a -> IO (VkSparseResidency2SamplesMType a)

        writeVkSparseResidency2Samples ::
                                       Ptr a -> VkSparseResidency2SamplesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseResidency2Samples'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseResidency2Samples a

class HasVkSparseResidency4Samples a where
        type VkSparseResidency4SamplesMType a :: *

        vkSparseResidency4Samples :: a -> VkSparseResidency4SamplesMType a

        vkSparseResidency4SamplesByteOffset :: a -> Int

        readVkSparseResidency4Samples ::
                                      Ptr a -> IO (VkSparseResidency4SamplesMType a)

        writeVkSparseResidency4Samples ::
                                       Ptr a -> VkSparseResidency4SamplesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseResidency4Samples'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseResidency4Samples a

class HasVkSparseResidency8Samples a where
        type VkSparseResidency8SamplesMType a :: *

        vkSparseResidency8Samples :: a -> VkSparseResidency8SamplesMType a

        vkSparseResidency8SamplesByteOffset :: a -> Int

        readVkSparseResidency8Samples ::
                                      Ptr a -> IO (VkSparseResidency8SamplesMType a)

        writeVkSparseResidency8Samples ::
                                       Ptr a -> VkSparseResidency8SamplesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseResidency8Samples'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseResidency8Samples a

class HasVkSparseResidencyAliased a where
        type VkSparseResidencyAliasedMType a :: *

        vkSparseResidencyAliased :: a -> VkSparseResidencyAliasedMType a

        vkSparseResidencyAliasedByteOffset :: a -> Int

        readVkSparseResidencyAliased ::
                                     Ptr a -> IO (VkSparseResidencyAliasedMType a)

        writeVkSparseResidencyAliased ::
                                      Ptr a -> VkSparseResidencyAliasedMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseResidencyAliased'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseResidencyAliased a

class HasVkSparseResidencyBuffer a where
        type VkSparseResidencyBufferMType a :: *

        vkSparseResidencyBuffer :: a -> VkSparseResidencyBufferMType a

        vkSparseResidencyBufferByteOffset :: a -> Int

        readVkSparseResidencyBuffer ::
                                    Ptr a -> IO (VkSparseResidencyBufferMType a)

        writeVkSparseResidencyBuffer ::
                                     Ptr a -> VkSparseResidencyBufferMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseResidencyBuffer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseResidencyBuffer a

class HasVkSparseResidencyImage2D a where
        type VkSparseResidencyImage2DMType a :: *

        vkSparseResidencyImage2D :: a -> VkSparseResidencyImage2DMType a

        vkSparseResidencyImage2DByteOffset :: a -> Int

        readVkSparseResidencyImage2D ::
                                     Ptr a -> IO (VkSparseResidencyImage2DMType a)

        writeVkSparseResidencyImage2D ::
                                      Ptr a -> VkSparseResidencyImage2DMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseResidencyImage2D'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseResidencyImage2D a

class HasVkSparseResidencyImage3D a where
        type VkSparseResidencyImage3DMType a :: *

        vkSparseResidencyImage3D :: a -> VkSparseResidencyImage3DMType a

        vkSparseResidencyImage3DByteOffset :: a -> Int

        readVkSparseResidencyImage3D ::
                                     Ptr a -> IO (VkSparseResidencyImage3DMType a)

        writeVkSparseResidencyImage3D ::
                                      Ptr a -> VkSparseResidencyImage3DMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'sparseResidencyImage3D'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSparseResidencyImage3D a

class HasVkSpecVersion a where
        type VkSpecVersionMType a :: *

        vkSpecVersion :: a -> VkSpecVersionMType a

        vkSpecVersionByteOffset :: a -> Int

        readVkSpecVersion :: Ptr a -> IO (VkSpecVersionMType a)

        writeVkSpecVersion :: Ptr a -> VkSpecVersionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'specVersion'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSpecVersion a

class HasVkSrcAccessMask a where
        type VkSrcAccessMaskMType a :: *

        vkSrcAccessMask :: a -> VkSrcAccessMaskMType a

        vkSrcAccessMaskByteOffset :: a -> Int

        readVkSrcAccessMask :: Ptr a -> IO (VkSrcAccessMaskMType a)

        writeVkSrcAccessMask :: Ptr a -> VkSrcAccessMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcAccessMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcAccessMask a

class HasVkSrcAlphaBlendFactor a where
        type VkSrcAlphaBlendFactorMType a :: *

        vkSrcAlphaBlendFactor :: a -> VkSrcAlphaBlendFactorMType a

        vkSrcAlphaBlendFactorByteOffset :: a -> Int

        readVkSrcAlphaBlendFactor ::
                                  Ptr a -> IO (VkSrcAlphaBlendFactorMType a)

        writeVkSrcAlphaBlendFactor ::
                                   Ptr a -> VkSrcAlphaBlendFactorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcAlphaBlendFactor'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcAlphaBlendFactor a

class HasVkSrcArrayElement a where
        type VkSrcArrayElementMType a :: *

        vkSrcArrayElement :: a -> VkSrcArrayElementMType a

        vkSrcArrayElementByteOffset :: a -> Int

        readVkSrcArrayElement :: Ptr a -> IO (VkSrcArrayElementMType a)

        writeVkSrcArrayElement ::
                               Ptr a -> VkSrcArrayElementMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcArrayElement'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcArrayElement a

class HasVkSrcBinding a where
        type VkSrcBindingMType a :: *

        vkSrcBinding :: a -> VkSrcBindingMType a

        vkSrcBindingByteOffset :: a -> Int

        readVkSrcBinding :: Ptr a -> IO (VkSrcBindingMType a)

        writeVkSrcBinding :: Ptr a -> VkSrcBindingMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcBinding'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcBinding a

class HasVkSrcColorBlendFactor a where
        type VkSrcColorBlendFactorMType a :: *

        vkSrcColorBlendFactor :: a -> VkSrcColorBlendFactorMType a

        vkSrcColorBlendFactorByteOffset :: a -> Int

        readVkSrcColorBlendFactor ::
                                  Ptr a -> IO (VkSrcColorBlendFactorMType a)

        writeVkSrcColorBlendFactor ::
                                   Ptr a -> VkSrcColorBlendFactorMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcColorBlendFactor'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcColorBlendFactor a

class HasVkSrcOffset a where
        type VkSrcOffsetMType a :: *

        vkSrcOffset :: a -> VkSrcOffsetMType a

        vkSrcOffsetByteOffset :: a -> Int

        readVkSrcOffset :: Ptr a -> IO (VkSrcOffsetMType a)

        writeVkSrcOffset :: Ptr a -> VkSrcOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcOffset a

class HasVkSrcOffsetsArray a where
        type VkSrcOffsetsArrayMType a :: *

        vkSrcOffsetsArray :: a -> Int -> VkSrcOffsetsArrayMType a

        vkSrcOffsetsArrayByteOffset :: a -> Int

        readVkSrcOffsetsArray ::
                              Ptr a -> Int -> IO (VkSrcOffsetsArrayMType a)

        writeVkSrcOffsetsArray ::
                               Ptr a -> Int -> VkSrcOffsetsArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcOffsets'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcOffsetsArray a

class HasVkSrcPremultiplied a where
        type VkSrcPremultipliedMType a :: *

        vkSrcPremultiplied :: a -> VkSrcPremultipliedMType a

        vkSrcPremultipliedByteOffset :: a -> Int

        readVkSrcPremultiplied :: Ptr a -> IO (VkSrcPremultipliedMType a)

        writeVkSrcPremultiplied ::
                                Ptr a -> VkSrcPremultipliedMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcPremultiplied'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcPremultiplied a

class HasVkSrcQueueFamilyIndex a where
        type VkSrcQueueFamilyIndexMType a :: *

        vkSrcQueueFamilyIndex :: a -> VkSrcQueueFamilyIndexMType a

        vkSrcQueueFamilyIndexByteOffset :: a -> Int

        readVkSrcQueueFamilyIndex ::
                                  Ptr a -> IO (VkSrcQueueFamilyIndexMType a)

        writeVkSrcQueueFamilyIndex ::
                                   Ptr a -> VkSrcQueueFamilyIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcQueueFamilyIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcQueueFamilyIndex a

class HasVkSrcRect a where
        type VkSrcRectMType a :: *

        vkSrcRect :: a -> VkSrcRectMType a

        vkSrcRectByteOffset :: a -> Int

        readVkSrcRect :: Ptr a -> IO (VkSrcRectMType a)

        writeVkSrcRect :: Ptr a -> VkSrcRectMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcRect'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcRect a

class HasVkSrcSet a where
        type VkSrcSetMType a :: *

        vkSrcSet :: a -> VkSrcSetMType a

        vkSrcSetByteOffset :: a -> Int

        readVkSrcSet :: Ptr a -> IO (VkSrcSetMType a)

        writeVkSrcSet :: Ptr a -> VkSrcSetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcSet'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcSet a

class HasVkSrcStageMask a where
        type VkSrcStageMaskMType a :: *

        vkSrcStageMask :: a -> VkSrcStageMaskMType a

        vkSrcStageMaskByteOffset :: a -> Int

        readVkSrcStageMask :: Ptr a -> IO (VkSrcStageMaskMType a)

        writeVkSrcStageMask :: Ptr a -> VkSrcStageMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcStageMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcStageMask a

class HasVkSrcSubpass a where
        type VkSrcSubpassMType a :: *

        vkSrcSubpass :: a -> VkSrcSubpassMType a

        vkSrcSubpassByteOffset :: a -> Int

        readVkSrcSubpass :: Ptr a -> IO (VkSrcSubpassMType a)

        writeVkSrcSubpass :: Ptr a -> VkSrcSubpassMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcSubpass'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcSubpass a

class HasVkSrcSubresource a where
        type VkSrcSubresourceMType a :: *

        vkSrcSubresource :: a -> VkSrcSubresourceMType a

        vkSrcSubresourceByteOffset :: a -> Int

        readVkSrcSubresource :: Ptr a -> IO (VkSrcSubresourceMType a)

        writeVkSrcSubresource :: Ptr a -> VkSrcSubresourceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'srcSubresource'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSrcSubresource a

class HasVkStage a where
        type VkStageMType a :: *

        vkStage :: a -> VkStageMType a

        vkStageByteOffset :: a -> Int

        readVkStage :: Ptr a -> IO (VkStageMType a)

        writeVkStage :: Ptr a -> VkStageMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'stage'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStage a

class HasVkStageCount a where
        type VkStageCountMType a :: *

        vkStageCount :: a -> VkStageCountMType a

        vkStageCountByteOffset :: a -> Int

        readVkStageCount :: Ptr a -> IO (VkStageCountMType a)

        writeVkStageCount :: Ptr a -> VkStageCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'stageCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStageCount a

class HasVkStageFlags a where
        type VkStageFlagsMType a :: *

        vkStageFlags :: a -> VkStageFlagsMType a

        vkStageFlagsByteOffset :: a -> Int

        readVkStageFlags :: Ptr a -> IO (VkStageFlagsMType a)

        writeVkStageFlags :: Ptr a -> VkStageFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'stageFlags'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStageFlags a

class HasVkStandardSampleLocations a where
        type VkStandardSampleLocationsMType a :: *

        vkStandardSampleLocations :: a -> VkStandardSampleLocationsMType a

        vkStandardSampleLocationsByteOffset :: a -> Int

        readVkStandardSampleLocations ::
                                      Ptr a -> IO (VkStandardSampleLocationsMType a)

        writeVkStandardSampleLocations ::
                                       Ptr a -> VkStandardSampleLocationsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'standardSampleLocations'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStandardSampleLocations a

class HasVkStencil a where
        type VkStencilMType a :: *

        vkStencil :: a -> VkStencilMType a

        vkStencilByteOffset :: a -> Int

        readVkStencil :: Ptr a -> IO (VkStencilMType a)

        writeVkStencil :: Ptr a -> VkStencilMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'stencil'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStencil a

class HasVkStencilLoadOp a where
        type VkStencilLoadOpMType a :: *

        vkStencilLoadOp :: a -> VkStencilLoadOpMType a

        vkStencilLoadOpByteOffset :: a -> Int

        readVkStencilLoadOp :: Ptr a -> IO (VkStencilLoadOpMType a)

        writeVkStencilLoadOp :: Ptr a -> VkStencilLoadOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'stencilLoadOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStencilLoadOp a

class HasVkStencilStoreOp a where
        type VkStencilStoreOpMType a :: *

        vkStencilStoreOp :: a -> VkStencilStoreOpMType a

        vkStencilStoreOpByteOffset :: a -> Int

        readVkStencilStoreOp :: Ptr a -> IO (VkStencilStoreOpMType a)

        writeVkStencilStoreOp :: Ptr a -> VkStencilStoreOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'stencilStoreOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStencilStoreOp a

class HasVkStencilTestEnable a where
        type VkStencilTestEnableMType a :: *

        vkStencilTestEnable :: a -> VkStencilTestEnableMType a

        vkStencilTestEnableByteOffset :: a -> Int

        readVkStencilTestEnable :: Ptr a -> IO (VkStencilTestEnableMType a)

        writeVkStencilTestEnable ::
                                 Ptr a -> VkStencilTestEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'stencilTestEnable'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStencilTestEnable a

class HasVkStorageBuffer16BitAccess a where
        type VkStorageBuffer16BitAccessMType a :: *

        vkStorageBuffer16BitAccess ::
                                   a -> VkStorageBuffer16BitAccessMType a

        vkStorageBuffer16BitAccessByteOffset :: a -> Int

        readVkStorageBuffer16BitAccess ::
                                       Ptr a -> IO (VkStorageBuffer16BitAccessMType a)

        writeVkStorageBuffer16BitAccess ::
                                        Ptr a -> VkStorageBuffer16BitAccessMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'storageBuffer16BitAccess'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStorageBuffer16BitAccess a

class HasVkStorageImageSampleCounts a where
        type VkStorageImageSampleCountsMType a :: *

        vkStorageImageSampleCounts ::
                                   a -> VkStorageImageSampleCountsMType a

        vkStorageImageSampleCountsByteOffset :: a -> Int

        readVkStorageImageSampleCounts ::
                                       Ptr a -> IO (VkStorageImageSampleCountsMType a)

        writeVkStorageImageSampleCounts ::
                                        Ptr a -> VkStorageImageSampleCountsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'storageImageSampleCounts'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStorageImageSampleCounts a

class HasVkStorageInputOutput16 a where
        type VkStorageInputOutput16MType a :: *

        vkStorageInputOutput16 :: a -> VkStorageInputOutput16MType a

        vkStorageInputOutput16ByteOffset :: a -> Int

        readVkStorageInputOutput16 ::
                                   Ptr a -> IO (VkStorageInputOutput16MType a)

        writeVkStorageInputOutput16 ::
                                    Ptr a -> VkStorageInputOutput16MType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'storageInputOutput16'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStorageInputOutput16 a

class HasVkStoragePushConstant16 a where
        type VkStoragePushConstant16MType a :: *

        vkStoragePushConstant16 :: a -> VkStoragePushConstant16MType a

        vkStoragePushConstant16ByteOffset :: a -> Int

        readVkStoragePushConstant16 ::
                                    Ptr a -> IO (VkStoragePushConstant16MType a)

        writeVkStoragePushConstant16 ::
                                     Ptr a -> VkStoragePushConstant16MType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'storagePushConstant16'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStoragePushConstant16 a

class HasVkStoreOp a where
        type VkStoreOpMType a :: *

        vkStoreOp :: a -> VkStoreOpMType a

        vkStoreOpByteOffset :: a -> Int

        readVkStoreOp :: Ptr a -> IO (VkStoreOpMType a)

        writeVkStoreOp :: Ptr a -> VkStoreOpMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'storeOp'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStoreOp a

class HasVkStrictLines a where
        type VkStrictLinesMType a :: *

        vkStrictLines :: a -> VkStrictLinesMType a

        vkStrictLinesByteOffset :: a -> Int

        readVkStrictLines :: Ptr a -> IO (VkStrictLinesMType a)

        writeVkStrictLines :: Ptr a -> VkStrictLinesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'strictLines'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStrictLines a

class HasVkStride a where
        type VkStrideMType a :: *

        vkStride :: a -> VkStrideMType a

        vkStrideByteOffset :: a -> Int

        readVkStride :: Ptr a -> IO (VkStrideMType a)

        writeVkStride :: Ptr a -> VkStrideMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'stride'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkStride a

class HasVkSubPixelInterpolationOffsetBits a where
        type VkSubPixelInterpolationOffsetBitsMType a :: *

        vkSubPixelInterpolationOffsetBits ::
                                          a -> VkSubPixelInterpolationOffsetBitsMType a

        vkSubPixelInterpolationOffsetBitsByteOffset :: a -> Int

        readVkSubPixelInterpolationOffsetBits ::
                                              Ptr a -> IO (VkSubPixelInterpolationOffsetBitsMType a)

        writeVkSubPixelInterpolationOffsetBits ::
                                               Ptr a ->
                                                 VkSubPixelInterpolationOffsetBitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'subPixelInterpolationOffsetBits'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubPixelInterpolationOffsetBits a

class HasVkSubPixelPrecisionBits a where
        type VkSubPixelPrecisionBitsMType a :: *

        vkSubPixelPrecisionBits :: a -> VkSubPixelPrecisionBitsMType a

        vkSubPixelPrecisionBitsByteOffset :: a -> Int

        readVkSubPixelPrecisionBits ::
                                    Ptr a -> IO (VkSubPixelPrecisionBitsMType a)

        writeVkSubPixelPrecisionBits ::
                                     Ptr a -> VkSubPixelPrecisionBitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'subPixelPrecisionBits'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubPixelPrecisionBits a

class HasVkSubTexelPrecisionBits a where
        type VkSubTexelPrecisionBitsMType a :: *

        vkSubTexelPrecisionBits :: a -> VkSubTexelPrecisionBitsMType a

        vkSubTexelPrecisionBitsByteOffset :: a -> Int

        readVkSubTexelPrecisionBits ::
                                    Ptr a -> IO (VkSubTexelPrecisionBitsMType a)

        writeVkSubTexelPrecisionBits ::
                                     Ptr a -> VkSubTexelPrecisionBitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'subTexelPrecisionBits'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubTexelPrecisionBits a

class HasVkSubpass a where
        type VkSubpassMType a :: *

        vkSubpass :: a -> VkSubpassMType a

        vkSubpassByteOffset :: a -> Int

        readVkSubpass :: Ptr a -> IO (VkSubpassMType a)

        writeVkSubpass :: Ptr a -> VkSubpassMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'subpass'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubpass a

class HasVkSubpassCount a where
        type VkSubpassCountMType a :: *

        vkSubpassCount :: a -> VkSubpassCountMType a

        vkSubpassCountByteOffset :: a -> Int

        readVkSubpassCount :: Ptr a -> IO (VkSubpassCountMType a)

        writeVkSubpassCount :: Ptr a -> VkSubpassCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'subpassCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubpassCount a

class HasVkSubpassIndex a where
        type VkSubpassIndexMType a :: *

        vkSubpassIndex :: a -> VkSubpassIndexMType a

        vkSubpassIndexByteOffset :: a -> Int

        readVkSubpassIndex :: Ptr a -> IO (VkSubpassIndexMType a)

        writeVkSubpassIndex :: Ptr a -> VkSubpassIndexMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'subpassIndex'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubpassIndex a

class HasVkSubresource a where
        type VkSubresourceMType a :: *

        vkSubresource :: a -> VkSubresourceMType a

        vkSubresourceByteOffset :: a -> Int

        readVkSubresource :: Ptr a -> IO (VkSubresourceMType a)

        writeVkSubresource :: Ptr a -> VkSubresourceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'subresource'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubresource a

class HasVkSubresourceRange a where
        type VkSubresourceRangeMType a :: *

        vkSubresourceRange :: a -> VkSubresourceRangeMType a

        vkSubresourceRangeByteOffset :: a -> Int

        readVkSubresourceRange :: Ptr a -> IO (VkSubresourceRangeMType a)

        writeVkSubresourceRange ::
                                Ptr a -> VkSubresourceRangeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'subresourceRange'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubresourceRange a

class HasVkSubsetAllocation a where
        type VkSubsetAllocationMType a :: *

        vkSubsetAllocation :: a -> VkSubsetAllocationMType a

        vkSubsetAllocationByteOffset :: a -> Int

        readVkSubsetAllocation :: Ptr a -> IO (VkSubsetAllocationMType a)

        writeVkSubsetAllocation ::
                                Ptr a -> VkSubsetAllocationMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'subsetAllocation'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSubsetAllocation a

class HasVkSupportedAlpha a where
        type VkSupportedAlphaMType a :: *

        vkSupportedAlpha :: a -> VkSupportedAlphaMType a

        vkSupportedAlphaByteOffset :: a -> Int

        readVkSupportedAlpha :: Ptr a -> IO (VkSupportedAlphaMType a)

        writeVkSupportedAlpha :: Ptr a -> VkSupportedAlphaMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'supportedAlpha'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSupportedAlpha a

class HasVkSupportedCompositeAlpha a where
        type VkSupportedCompositeAlphaMType a :: *

        vkSupportedCompositeAlpha :: a -> VkSupportedCompositeAlphaMType a

        vkSupportedCompositeAlphaByteOffset :: a -> Int

        readVkSupportedCompositeAlpha ::
                                      Ptr a -> IO (VkSupportedCompositeAlphaMType a)

        writeVkSupportedCompositeAlpha ::
                                       Ptr a -> VkSupportedCompositeAlphaMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'supportedCompositeAlpha'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSupportedCompositeAlpha a

class HasVkSupportedSurfaceCounters a where
        type VkSupportedSurfaceCountersMType a :: *

        vkSupportedSurfaceCounters ::
                                   a -> VkSupportedSurfaceCountersMType a

        vkSupportedSurfaceCountersByteOffset :: a -> Int

        readVkSupportedSurfaceCounters ::
                                       Ptr a -> IO (VkSupportedSurfaceCountersMType a)

        writeVkSupportedSurfaceCounters ::
                                        Ptr a -> VkSupportedSurfaceCountersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'supportedSurfaceCounters'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSupportedSurfaceCounters a

class HasVkSupportedTransforms a where
        type VkSupportedTransformsMType a :: *

        vkSupportedTransforms :: a -> VkSupportedTransformsMType a

        vkSupportedTransformsByteOffset :: a -> Int

        readVkSupportedTransforms ::
                                  Ptr a -> IO (VkSupportedTransformsMType a)

        writeVkSupportedTransforms ::
                                   Ptr a -> VkSupportedTransformsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'supportedTransforms'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSupportedTransforms a

class HasVkSupportedUsageFlags a where
        type VkSupportedUsageFlagsMType a :: *

        vkSupportedUsageFlags :: a -> VkSupportedUsageFlagsMType a

        vkSupportedUsageFlagsByteOffset :: a -> Int

        readVkSupportedUsageFlags ::
                                  Ptr a -> IO (VkSupportedUsageFlagsMType a)

        writeVkSupportedUsageFlags ::
                                   Ptr a -> VkSupportedUsageFlagsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'supportedUsageFlags'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSupportedUsageFlags a

class HasVkSupportsTextureGatherLODBiasAMD a where
        type VkSupportsTextureGatherLODBiasAMDMType a :: *

        vkSupportsTextureGatherLODBiasAMD ::
                                          a -> VkSupportsTextureGatherLODBiasAMDMType a

        vkSupportsTextureGatherLODBiasAMDByteOffset :: a -> Int

        readVkSupportsTextureGatherLODBiasAMD ::
                                              Ptr a -> IO (VkSupportsTextureGatherLODBiasAMDMType a)

        writeVkSupportsTextureGatherLODBiasAMD ::
                                               Ptr a ->
                                                 VkSupportsTextureGatherLODBiasAMDMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'supportsTextureGatherLODBiasAMD'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSupportsTextureGatherLODBiasAMD a

class HasVkSurface a where
        type VkSurfaceMType a :: *

        vkSurface :: a -> VkSurfaceMType a

        vkSurfaceByteOffset :: a -> Int

        readVkSurface :: Ptr a -> IO (VkSurfaceMType a)

        writeVkSurface :: Ptr a -> VkSurfaceMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'surface'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSurface a

class HasVkSurfaceCapabilities a where
        type VkSurfaceCapabilitiesMType a :: *

        vkSurfaceCapabilities :: a -> VkSurfaceCapabilitiesMType a

        vkSurfaceCapabilitiesByteOffset :: a -> Int

        readVkSurfaceCapabilities ::
                                  Ptr a -> IO (VkSurfaceCapabilitiesMType a)

        writeVkSurfaceCapabilities ::
                                   Ptr a -> VkSurfaceCapabilitiesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'surfaceCapabilities'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSurfaceCapabilities a

class HasVkSurfaceCounters a where
        type VkSurfaceCountersMType a :: *

        vkSurfaceCounters :: a -> VkSurfaceCountersMType a

        vkSurfaceCountersByteOffset :: a -> Int

        readVkSurfaceCounters :: Ptr a -> IO (VkSurfaceCountersMType a)

        writeVkSurfaceCounters ::
                               Ptr a -> VkSurfaceCountersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'surfaceCounters'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSurfaceCounters a

class HasVkSurfaceFormat a where
        type VkSurfaceFormatMType a :: *

        vkSurfaceFormat :: a -> VkSurfaceFormatMType a

        vkSurfaceFormatByteOffset :: a -> Int

        readVkSurfaceFormat :: Ptr a -> IO (VkSurfaceFormatMType a)

        writeVkSurfaceFormat :: Ptr a -> VkSurfaceFormatMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'surfaceFormat'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSurfaceFormat a

class HasVkSwapchain a where
        type VkSwapchainMType a :: *

        vkSwapchain :: a -> VkSwapchainMType a

        vkSwapchainByteOffset :: a -> Int

        readVkSwapchain :: Ptr a -> IO (VkSwapchainMType a)

        writeVkSwapchain :: Ptr a -> VkSwapchainMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'swapchain'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSwapchain a

class HasVkSwapchainCount a where
        type VkSwapchainCountMType a :: *

        vkSwapchainCount :: a -> VkSwapchainCountMType a

        vkSwapchainCountByteOffset :: a -> Int

        readVkSwapchainCount :: Ptr a -> IO (VkSwapchainCountMType a)

        writeVkSwapchainCount :: Ptr a -> VkSwapchainCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'swapchainCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkSwapchainCount a

class HasVkTagName a where
        type VkTagNameMType a :: *

        vkTagName :: a -> VkTagNameMType a

        vkTagNameByteOffset :: a -> Int

        readVkTagName :: Ptr a -> IO (VkTagNameMType a)

        writeVkTagName :: Ptr a -> VkTagNameMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'tagName'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTagName a

class HasVkTagSize a where
        type VkTagSizeMType a :: *

        vkTagSize :: a -> VkTagSizeMType a

        vkTagSizeByteOffset :: a -> Int

        readVkTagSize :: Ptr a -> IO (VkTagSizeMType a)

        writeVkTagSize :: Ptr a -> VkTagSizeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'tagSize'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTagSize a

class HasVkTargetCommandBuffer a where
        type VkTargetCommandBufferMType a :: *

        vkTargetCommandBuffer :: a -> VkTargetCommandBufferMType a

        vkTargetCommandBufferByteOffset :: a -> Int

        readVkTargetCommandBuffer ::
                                  Ptr a -> IO (VkTargetCommandBufferMType a)

        writeVkTargetCommandBuffer ::
                                   Ptr a -> VkTargetCommandBufferMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'targetCommandBuffer'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTargetCommandBuffer a

class HasVkTemplateType a where
        type VkTemplateTypeMType a :: *

        vkTemplateType :: a -> VkTemplateTypeMType a

        vkTemplateTypeByteOffset :: a -> Int

        readVkTemplateType :: Ptr a -> IO (VkTemplateTypeMType a)

        writeVkTemplateType :: Ptr a -> VkTemplateTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'templateType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTemplateType a

class HasVkTessellationShader a where
        type VkTessellationShaderMType a :: *

        vkTessellationShader :: a -> VkTessellationShaderMType a

        vkTessellationShaderByteOffset :: a -> Int

        readVkTessellationShader ::
                                 Ptr a -> IO (VkTessellationShaderMType a)

        writeVkTessellationShader ::
                                  Ptr a -> VkTessellationShaderMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'tessellationShader'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTessellationShader a

class HasVkTextureCompressionASTC_LDR a where
        type VkTextureCompressionASTC_LDRMType a :: *

        vkTextureCompressionASTC_LDR ::
                                     a -> VkTextureCompressionASTC_LDRMType a

        vkTextureCompressionASTC_LDRByteOffset :: a -> Int

        readVkTextureCompressionASTC_LDR ::
                                         Ptr a -> IO (VkTextureCompressionASTC_LDRMType a)

        writeVkTextureCompressionASTC_LDR ::
                                          Ptr a -> VkTextureCompressionASTC_LDRMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'textureCompressionASTC_LDR'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTextureCompressionASTC_LDR a

class HasVkTextureCompressionBC a where
        type VkTextureCompressionBCMType a :: *

        vkTextureCompressionBC :: a -> VkTextureCompressionBCMType a

        vkTextureCompressionBCByteOffset :: a -> Int

        readVkTextureCompressionBC ::
                                   Ptr a -> IO (VkTextureCompressionBCMType a)

        writeVkTextureCompressionBC ::
                                    Ptr a -> VkTextureCompressionBCMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'textureCompressionBC'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTextureCompressionBC a

class HasVkTextureCompressionETC2 a where
        type VkTextureCompressionETC2MType a :: *

        vkTextureCompressionETC2 :: a -> VkTextureCompressionETC2MType a

        vkTextureCompressionETC2ByteOffset :: a -> Int

        readVkTextureCompressionETC2 ::
                                     Ptr a -> IO (VkTextureCompressionETC2MType a)

        writeVkTextureCompressionETC2 ::
                                      Ptr a -> VkTextureCompressionETC2MType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'textureCompressionETC2'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTextureCompressionETC2 a

class HasVkTiling a where
        type VkTilingMType a :: *

        vkTiling :: a -> VkTilingMType a

        vkTilingByteOffset :: a -> Int

        readVkTiling :: Ptr a -> IO (VkTilingMType a)

        writeVkTiling :: Ptr a -> VkTilingMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'tiling'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTiling a

class HasVkTimeout a where
        type VkTimeoutMType a :: *

        vkTimeout :: a -> VkTimeoutMType a

        vkTimeoutByteOffset :: a -> Int

        readVkTimeout :: Ptr a -> IO (VkTimeoutMType a)

        writeVkTimeout :: Ptr a -> VkTimeoutMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'timeout'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTimeout a

class HasVkTimestampComputeAndGraphics a where
        type VkTimestampComputeAndGraphicsMType a :: *

        vkTimestampComputeAndGraphics ::
                                      a -> VkTimestampComputeAndGraphicsMType a

        vkTimestampComputeAndGraphicsByteOffset :: a -> Int

        readVkTimestampComputeAndGraphics ::
                                          Ptr a -> IO (VkTimestampComputeAndGraphicsMType a)

        writeVkTimestampComputeAndGraphics ::
                                           Ptr a -> VkTimestampComputeAndGraphicsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'timestampComputeAndGraphics'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTimestampComputeAndGraphics a

class HasVkTimestampPeriod a where
        type VkTimestampPeriodMType a :: *

        vkTimestampPeriod :: a -> VkTimestampPeriodMType a

        vkTimestampPeriodByteOffset :: a -> Int

        readVkTimestampPeriod :: Ptr a -> IO (VkTimestampPeriodMType a)

        writeVkTimestampPeriod ::
                               Ptr a -> VkTimestampPeriodMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'timestampPeriod'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTimestampPeriod a

class HasVkTimestampValidBits a where
        type VkTimestampValidBitsMType a :: *

        vkTimestampValidBits :: a -> VkTimestampValidBitsMType a

        vkTimestampValidBitsByteOffset :: a -> Int

        readVkTimestampValidBits ::
                                 Ptr a -> IO (VkTimestampValidBitsMType a)

        writeVkTimestampValidBits ::
                                  Ptr a -> VkTimestampValidBitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'timestampValidBits'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTimestampValidBits a

class HasVkTokenCount a where
        type VkTokenCountMType a :: *

        vkTokenCount :: a -> VkTokenCountMType a

        vkTokenCountByteOffset :: a -> Int

        readVkTokenCount :: Ptr a -> IO (VkTokenCountMType a)

        writeVkTokenCount :: Ptr a -> VkTokenCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'tokenCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTokenCount a

class HasVkTokenType a where
        type VkTokenTypeMType a :: *

        vkTokenType :: a -> VkTokenTypeMType a

        vkTokenTypeByteOffset :: a -> Int

        readVkTokenType :: Ptr a -> IO (VkTokenTypeMType a)

        writeVkTokenType :: Ptr a -> VkTokenTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'tokenType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTokenType a

class HasVkTopology a where
        type VkTopologyMType a :: *

        vkTopology :: a -> VkTopologyMType a

        vkTopologyByteOffset :: a -> Int

        readVkTopology :: Ptr a -> IO (VkTopologyMType a)

        writeVkTopology :: Ptr a -> VkTopologyMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'topology'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTopology a

class HasVkTransform a where
        type VkTransformMType a :: *

        vkTransform :: a -> VkTransformMType a

        vkTransformByteOffset :: a -> Int

        readVkTransform :: Ptr a -> IO (VkTransformMType a)

        writeVkTransform :: Ptr a -> VkTransformMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'transform'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkTransform a

class HasVkType a where
        type VkTypeMType a :: *

        vkType :: a -> VkTypeMType a

        vkTypeByteOffset :: a -> Int

        readVkType :: Ptr a -> IO (VkTypeMType a)

        writeVkType :: Ptr a -> VkTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'type'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkType a

class HasVkUint32Array a where
        type VkUint32ArrayMType a :: *

        vkUint32Array :: a -> Int -> VkUint32ArrayMType a

        vkUint32ArrayByteOffset :: a -> Int

        readVkUint32Array :: Ptr a -> Int -> IO (VkUint32ArrayMType a)

        writeVkUint32Array :: Ptr a -> Int -> VkUint32ArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'uint32'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkUint32Array a

class HasVkUniformAndStorageBuffer16BitAccess a where
        type VkUniformAndStorageBuffer16BitAccessMType a :: *

        vkUniformAndStorageBuffer16BitAccess ::
                                             a -> VkUniformAndStorageBuffer16BitAccessMType a

        vkUniformAndStorageBuffer16BitAccessByteOffset :: a -> Int

        readVkUniformAndStorageBuffer16BitAccess ::
                                                 Ptr a ->
                                                   IO (VkUniformAndStorageBuffer16BitAccessMType a)

        writeVkUniformAndStorageBuffer16BitAccess ::
                                                  Ptr a ->
                                                    VkUniformAndStorageBuffer16BitAccessMType a ->
                                                      IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'uniformAndStorageBuffer16BitAccess'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkUniformAndStorageBuffer16BitAccess a

class HasVkUnnormalizedCoordinates a where
        type VkUnnormalizedCoordinatesMType a :: *

        vkUnnormalizedCoordinates :: a -> VkUnnormalizedCoordinatesMType a

        vkUnnormalizedCoordinatesByteOffset :: a -> Int

        readVkUnnormalizedCoordinates ::
                                      Ptr a -> IO (VkUnnormalizedCoordinatesMType a)

        writeVkUnnormalizedCoordinates ::
                                       Ptr a -> VkUnnormalizedCoordinatesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'unnormalizedCoordinates'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkUnnormalizedCoordinates a

class HasVkUsage a where
        type VkUsageMType a :: *

        vkUsage :: a -> VkUsageMType a

        vkUsageByteOffset :: a -> Int

        readVkUsage :: Ptr a -> IO (VkUsageMType a)

        writeVkUsage :: Ptr a -> VkUsageMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'usage'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkUsage a

class HasVkValidationCache a where
        type VkValidationCacheMType a :: *

        vkValidationCache :: a -> VkValidationCacheMType a

        vkValidationCacheByteOffset :: a -> Int

        readVkValidationCache :: Ptr a -> IO (VkValidationCacheMType a)

        writeVkValidationCache ::
                               Ptr a -> VkValidationCacheMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'validationCache'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkValidationCache a

class HasVkVariableMultisampleRate a where
        type VkVariableMultisampleRateMType a :: *

        vkVariableMultisampleRate :: a -> VkVariableMultisampleRateMType a

        vkVariableMultisampleRateByteOffset :: a -> Int

        readVkVariableMultisampleRate ::
                                      Ptr a -> IO (VkVariableMultisampleRateMType a)

        writeVkVariableMultisampleRate ::
                                       Ptr a -> VkVariableMultisampleRateMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'variableMultisampleRate'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVariableMultisampleRate a

class HasVkVariablePointers a where
        type VkVariablePointersMType a :: *

        vkVariablePointers :: a -> VkVariablePointersMType a

        vkVariablePointersByteOffset :: a -> Int

        readVkVariablePointers :: Ptr a -> IO (VkVariablePointersMType a)

        writeVkVariablePointers ::
                                Ptr a -> VkVariablePointersMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'variablePointers'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVariablePointers a

class HasVkVariablePointersStorageBuffer a where
        type VkVariablePointersStorageBufferMType a :: *

        vkVariablePointersStorageBuffer ::
                                        a -> VkVariablePointersStorageBufferMType a

        vkVariablePointersStorageBufferByteOffset :: a -> Int

        readVkVariablePointersStorageBuffer ::
                                            Ptr a -> IO (VkVariablePointersStorageBufferMType a)

        writeVkVariablePointersStorageBuffer ::
                                             Ptr a ->
                                               VkVariablePointersStorageBufferMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'variablePointersStorageBuffer'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVariablePointersStorageBuffer a

class HasVkVariableSampleLocations a where
        type VkVariableSampleLocationsMType a :: *

        vkVariableSampleLocations :: a -> VkVariableSampleLocationsMType a

        vkVariableSampleLocationsByteOffset :: a -> Int

        readVkVariableSampleLocations ::
                                      Ptr a -> IO (VkVariableSampleLocationsMType a)

        writeVkVariableSampleLocations ::
                                       Ptr a -> VkVariableSampleLocationsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'variableSampleLocations'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVariableSampleLocations a

class HasVkVendorID a where
        type VkVendorIDMType a :: *

        vkVendorID :: a -> VkVendorIDMType a

        vkVendorIDByteOffset :: a -> Int

        readVkVendorID :: Ptr a -> IO (VkVendorIDMType a)

        writeVkVendorID :: Ptr a -> VkVendorIDMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'vendorID'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVendorID a

class HasVkVertexAttributeDescriptionCount a where
        type VkVertexAttributeDescriptionCountMType a :: *

        vkVertexAttributeDescriptionCount ::
                                          a -> VkVertexAttributeDescriptionCountMType a

        vkVertexAttributeDescriptionCountByteOffset :: a -> Int

        readVkVertexAttributeDescriptionCount ::
                                              Ptr a -> IO (VkVertexAttributeDescriptionCountMType a)

        writeVkVertexAttributeDescriptionCount ::
                                               Ptr a ->
                                                 VkVertexAttributeDescriptionCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'vertexAttributeDescriptionCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVertexAttributeDescriptionCount a

class HasVkVertexBindingDescriptionCount a where
        type VkVertexBindingDescriptionCountMType a :: *

        vkVertexBindingDescriptionCount ::
                                        a -> VkVertexBindingDescriptionCountMType a

        vkVertexBindingDescriptionCountByteOffset :: a -> Int

        readVkVertexBindingDescriptionCount ::
                                            Ptr a -> IO (VkVertexBindingDescriptionCountMType a)

        writeVkVertexBindingDescriptionCount ::
                                             Ptr a ->
                                               VkVertexBindingDescriptionCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'vertexBindingDescriptionCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVertexBindingDescriptionCount a

class HasVkVertexCount a where
        type VkVertexCountMType a :: *

        vkVertexCount :: a -> VkVertexCountMType a

        vkVertexCountByteOffset :: a -> Int

        readVkVertexCount :: Ptr a -> IO (VkVertexCountMType a)

        writeVkVertexCount :: Ptr a -> VkVertexCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'vertexCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVertexCount a

class HasVkVertexOffset a where
        type VkVertexOffsetMType a :: *

        vkVertexOffset :: a -> VkVertexOffsetMType a

        vkVertexOffsetByteOffset :: a -> Int

        readVkVertexOffset :: Ptr a -> IO (VkVertexOffsetMType a)

        writeVkVertexOffset :: Ptr a -> VkVertexOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'vertexOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVertexOffset a

class HasVkVertexPipelineStoresAndAtomics a where
        type VkVertexPipelineStoresAndAtomicsMType a :: *

        vkVertexPipelineStoresAndAtomics ::
                                         a -> VkVertexPipelineStoresAndAtomicsMType a

        vkVertexPipelineStoresAndAtomicsByteOffset :: a -> Int

        readVkVertexPipelineStoresAndAtomics ::
                                             Ptr a -> IO (VkVertexPipelineStoresAndAtomicsMType a)

        writeVkVertexPipelineStoresAndAtomics ::
                                              Ptr a ->
                                                VkVertexPipelineStoresAndAtomicsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text
                " does not seem to have field 'vertexPipelineStoresAndAtomics'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVertexPipelineStoresAndAtomics a

class HasVkViewFormatCount a where
        type VkViewFormatCountMType a :: *

        vkViewFormatCount :: a -> VkViewFormatCountMType a

        vkViewFormatCountByteOffset :: a -> Int

        readVkViewFormatCount :: Ptr a -> IO (VkViewFormatCountMType a)

        writeVkViewFormatCount ::
                               Ptr a -> VkViewFormatCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'viewFormatCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkViewFormatCount a

class HasVkViewType a where
        type VkViewTypeMType a :: *

        vkViewType :: a -> VkViewTypeMType a

        vkViewTypeByteOffset :: a -> Int

        readVkViewType :: Ptr a -> IO (VkViewTypeMType a)

        writeVkViewType :: Ptr a -> VkViewTypeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'viewType'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkViewType a

class HasVkViewportBoundsRangeArray a where
        type VkViewportBoundsRangeArrayMType a :: *

        vkViewportBoundsRangeArray ::
                                   a -> Int -> VkViewportBoundsRangeArrayMType a

        vkViewportBoundsRangeArrayByteOffset :: a -> Int

        readVkViewportBoundsRangeArray ::
                                       Ptr a -> Int -> IO (VkViewportBoundsRangeArrayMType a)

        writeVkViewportBoundsRangeArray ::
                                        Ptr a -> Int -> VkViewportBoundsRangeArrayMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'viewportBoundsRange'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkViewportBoundsRangeArray a

class HasVkViewportCount a where
        type VkViewportCountMType a :: *

        vkViewportCount :: a -> VkViewportCountMType a

        vkViewportCountByteOffset :: a -> Int

        readVkViewportCount :: Ptr a -> IO (VkViewportCountMType a)

        writeVkViewportCount :: Ptr a -> VkViewportCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'viewportCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkViewportCount a

class HasVkViewportSubPixelBits a where
        type VkViewportSubPixelBitsMType a :: *

        vkViewportSubPixelBits :: a -> VkViewportSubPixelBitsMType a

        vkViewportSubPixelBitsByteOffset :: a -> Int

        readVkViewportSubPixelBits ::
                                   Ptr a -> IO (VkViewportSubPixelBitsMType a)

        writeVkViewportSubPixelBits ::
                                    Ptr a -> VkViewportSubPixelBitsMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'viewportSubPixelBits'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkViewportSubPixelBits a

class HasVkViewportWScalingEnable a where
        type VkViewportWScalingEnableMType a :: *

        vkViewportWScalingEnable :: a -> VkViewportWScalingEnableMType a

        vkViewportWScalingEnableByteOffset :: a -> Int

        readVkViewportWScalingEnable ::
                                     Ptr a -> IO (VkViewportWScalingEnableMType a)

        writeVkViewportWScalingEnable ::
                                      Ptr a -> VkViewportWScalingEnableMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'viewportWScalingEnable'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkViewportWScalingEnable a

class HasVkVisibleRegion a where
        type VkVisibleRegionMType a :: *

        vkVisibleRegion :: a -> VkVisibleRegionMType a

        vkVisibleRegionByteOffset :: a -> Int

        readVkVisibleRegion :: Ptr a -> IO (VkVisibleRegionMType a)

        writeVkVisibleRegion :: Ptr a -> VkVisibleRegionMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'visibleRegion'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkVisibleRegion a

class HasVkW a where
        type VkWMType a :: *

        vkW :: a -> VkWMType a

        vkWByteOffset :: a -> Int

        readVkW :: Ptr a -> IO (VkWMType a)

        writeVkW :: Ptr a -> VkWMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'w'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkW a

class HasVkWaitSemaphoreCount a where
        type VkWaitSemaphoreCountMType a :: *

        vkWaitSemaphoreCount :: a -> VkWaitSemaphoreCountMType a

        vkWaitSemaphoreCountByteOffset :: a -> Int

        readVkWaitSemaphoreCount ::
                                 Ptr a -> IO (VkWaitSemaphoreCountMType a)

        writeVkWaitSemaphoreCount ::
                                  Ptr a -> VkWaitSemaphoreCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'waitSemaphoreCount'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkWaitSemaphoreCount a

class HasVkWaitSemaphoreValuesCount a where
        type VkWaitSemaphoreValuesCountMType a :: *

        vkWaitSemaphoreValuesCount ::
                                   a -> VkWaitSemaphoreValuesCountMType a

        vkWaitSemaphoreValuesCountByteOffset :: a -> Int

        readVkWaitSemaphoreValuesCount ::
                                       Ptr a -> IO (VkWaitSemaphoreValuesCountMType a)

        writeVkWaitSemaphoreValuesCount ::
                                        Ptr a -> VkWaitSemaphoreValuesCountMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'waitSemaphoreValuesCount'."
                ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkWaitSemaphoreValuesCount a

class HasVkWhitePoint a where
        type VkWhitePointMType a :: *

        vkWhitePoint :: a -> VkWhitePointMType a

        vkWhitePointByteOffset :: a -> Int

        readVkWhitePoint :: Ptr a -> IO (VkWhitePointMType a)

        writeVkWhitePoint :: Ptr a -> VkWhitePointMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'whitePoint'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkWhitePoint a

class HasVkWideLines a where
        type VkWideLinesMType a :: *

        vkWideLines :: a -> VkWideLinesMType a

        vkWideLinesByteOffset :: a -> Int

        readVkWideLines :: Ptr a -> IO (VkWideLinesMType a)

        writeVkWideLines :: Ptr a -> VkWideLinesMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'wideLines'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkWideLines a

class HasVkWidth a where
        type VkWidthMType a :: *

        vkWidth :: a -> VkWidthMType a

        vkWidthByteOffset :: a -> Int

        readVkWidth :: Ptr a -> IO (VkWidthMType a)

        writeVkWidth :: Ptr a -> VkWidthMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'width'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkWidth a

class HasVkWindow a where
        type VkWindowMType a :: *

        vkWindow :: a -> VkWindowMType a

        vkWindowByteOffset :: a -> Int

        readVkWindow :: Ptr a -> IO (VkWindowMType a)

        writeVkWindow :: Ptr a -> VkWindowMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'window'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkWindow a

class HasVkWriteMask a where
        type VkWriteMaskMType a :: *

        vkWriteMask :: a -> VkWriteMaskMType a

        vkWriteMaskByteOffset :: a -> Int

        readVkWriteMask :: Ptr a -> IO (VkWriteMaskMType a)

        writeVkWriteMask :: Ptr a -> VkWriteMaskMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'writeMask'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkWriteMask a

class HasVkX a where
        type VkXMType a :: *

        vkX :: a -> VkXMType a

        vkXByteOffset :: a -> Int

        readVkX :: Ptr a -> IO (VkXMType a)

        writeVkX :: Ptr a -> VkXMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'x'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkX a

class HasVkXChromaOffset a where
        type VkXChromaOffsetMType a :: *

        vkXChromaOffset :: a -> VkXChromaOffsetMType a

        vkXChromaOffsetByteOffset :: a -> Int

        readVkXChromaOffset :: Ptr a -> IO (VkXChromaOffsetMType a)

        writeVkXChromaOffset :: Ptr a -> VkXChromaOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'xChromaOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkXChromaOffset a

class HasVkXcoeff a where
        type VkXcoeffMType a :: *

        vkXcoeff :: a -> VkXcoeffMType a

        vkXcoeffByteOffset :: a -> Int

        readVkXcoeff :: Ptr a -> IO (VkXcoeffMType a)

        writeVkXcoeff :: Ptr a -> VkXcoeffMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'xcoeff'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkXcoeff a

class HasVkY a where
        type VkYMType a :: *

        vkY :: a -> VkYMType a

        vkYByteOffset :: a -> Int

        readVkY :: Ptr a -> IO (VkYMType a)

        writeVkY :: Ptr a -> VkYMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'y'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkY a

class HasVkYChromaOffset a where
        type VkYChromaOffsetMType a :: *

        vkYChromaOffset :: a -> VkYChromaOffsetMType a

        vkYChromaOffsetByteOffset :: a -> Int

        readVkYChromaOffset :: Ptr a -> IO (VkYChromaOffsetMType a)

        writeVkYChromaOffset :: Ptr a -> VkYChromaOffsetMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'yChromaOffset'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkYChromaOffset a

class HasVkYcbcrModel a where
        type VkYcbcrModelMType a :: *

        vkYcbcrModel :: a -> VkYcbcrModelMType a

        vkYcbcrModelByteOffset :: a -> Int

        readVkYcbcrModel :: Ptr a -> IO (VkYcbcrModelMType a)

        writeVkYcbcrModel :: Ptr a -> VkYcbcrModelMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'ycbcrModel'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkYcbcrModel a

class HasVkYcbcrRange a where
        type VkYcbcrRangeMType a :: *

        vkYcbcrRange :: a -> VkYcbcrRangeMType a

        vkYcbcrRangeByteOffset :: a -> Int

        readVkYcbcrRange :: Ptr a -> IO (VkYcbcrRangeMType a)

        writeVkYcbcrRange :: Ptr a -> VkYcbcrRangeMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'ycbcrRange'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkYcbcrRange a

class HasVkYcoeff a where
        type VkYcoeffMType a :: *

        vkYcoeff :: a -> VkYcoeffMType a

        vkYcoeffByteOffset :: a -> Int

        readVkYcoeff :: Ptr a -> IO (VkYcoeffMType a)

        writeVkYcoeff :: Ptr a -> VkYcoeffMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'ycoeff'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkYcoeff a

class HasVkZ a where
        type VkZMType a :: *

        vkZ :: a -> VkZMType a

        vkZByteOffset :: a -> Int

        readVkZ :: Ptr a -> IO (VkZMType a)

        writeVkZ :: Ptr a -> VkZMType a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError
           ('ShowType a ':<>:
              'Text " does not seem to have field 'z'." ':$$:
                'Text
                  "Check Vulkan documentation for available fields of this type.") =>
         HasVkZ a
