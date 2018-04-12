#ifdef VK_NO_PROTOTYPES
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#include "vulkan_loader.h"


VKAPI_ATTR PFN_vkVoidFunction VKAPI_CALL vkGetInstanceProcAddr(
    VkInstance                                  instance,
    const char*                                 pName)
{
  void *handle;
  PFN_vkGetInstanceProcAddr getProcAddr = NULL;
  PFN_vkVoidFunction rez = NULL;
  char *error;
  handle = dlopen ("libvulkan.so", RTLD_LAZY);
  if (!handle) {
        fputs (dlerror(), stderr);
        exit(1);
  }

  getProcAddr = (PFN_vkGetInstanceProcAddr)dlsym(handle, "vkGetInstanceProcAddr");

  if ((error = dlerror()) != NULL)  {
      fputs(error, stderr);
      exit(1);
  }
  rez = getProcAddr(instance, pName);
  dlclose(handle);
  return rez;
}


VKAPI_ATTR PFN_vkVoidFunction VKAPI_CALL vkGetDeviceProcAddr(
    VkDevice                                    device,
    const char*                                 pName)
{
  void *handle;
  PFN_vkGetDeviceProcAddr getProcAddr = NULL;
  PFN_vkVoidFunction rez = NULL;
  char *error;
  handle = dlopen ("libvulkan.so", RTLD_LAZY);
  if (!handle) {
        fputs (dlerror(), stderr);
        exit(1);
  }

  getProcAddr = (PFN_vkGetDeviceProcAddr)dlsym(handle, "vkGetDeviceProcAddr");

  if ((error = dlerror()) != NULL)  {
      fputs(error, stderr);
      exit(1);
  }
  rez = getProcAddr(device, pName);
  dlclose(handle);
  return rez;
}


#endif
