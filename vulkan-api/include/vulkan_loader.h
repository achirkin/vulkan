#ifndef VULKAN_LOADER_H_
#define VULKAN_LOADER_H_ 1
#ifdef VK_NO_PROTOTYPES
#include "vulkan/vulkan.h"

VKAPI_ATTR PFN_vkVoidFunction VKAPI_CALL vkGetInstanceProcAddr(
    VkInstance                                  instance,
    const char*                                 pName);

VKAPI_ATTR PFN_vkVoidFunction VKAPI_CALL vkGetDeviceProcAddr(
    VkDevice                                    device,
    const char*                                 pName);


#endif
#endif
