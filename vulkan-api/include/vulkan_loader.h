#ifndef VULKAN_LOADER_H_
#define VULKAN_LOADER_H_ 1

#if mingw32_HOST_OS
  #include <WinBase.h>
  #define _vk_dlopen() LoadLibraryA("vulkan-1.dll")
  #define _vk_dlclose(handle) FreeLibrary((HMODULE) handle)
  #define _vk_dlsym(handle, name) GetProcAddress((HMODULE) handle, name)
#else
  #include <dlfcn.h>
  #ifdef darwin_HOST_OS
    #define _vk_dlopen() dlopen("libMoltenVK.dylib", RTLD_LAZY | RTLD_LOCAL)
  #else
    #define _vk_dlopen() dlopen("libvulkan.so", RTLD_LAZY | RTLD_LOCAL)
  #endif
  #define _vk_dlclose(handle) dlclose(handle)
  #define _vk_dlsym(handle, name) dlsym(handle, name)
#endif


void *_vkdll_dlinit(char **errorMsg);
void  _vkdll_dlclose(void *handle);
void *_vkdll_dlsym(void *handle, const char *symbol, char **errorMsg);

#endif
