#include "vulkan_loader.h"

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)
  #include <windows.h>
  #define _vk_dlopen() LoadLibraryA("vulkan-1.dll")
  #define _vk_dlclose(handle) FreeLibrary((HMODULE) handle)
  #define _vk_dlsym(handle, name) GetProcAddress((HMODULE) handle, name)
  #define _vk_dlerror(msg) *msg = errMsgBuf; FormatMessage( \
        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, \
        NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) errMsgBuf , 255, NULL )
  static TCHAR errMsgBuf[255] = { 0 };
#else
  #include <dlfcn.h>
  #if defined(__APPLE__) || defined(darwin_HOST_OS)
    #define _vk_dlopen() dlopen("libvulkan.1.dylib", RTLD_LAZY | RTLD_LOCAL)
  #else
    #define _vk_dlopen() dlopen("libvulkan.so", RTLD_LAZY | RTLD_LOCAL)
  #endif
  #define _vk_dlclose(handle) dlclose(handle)
  #define _vk_dlsym(handle, name) dlsym(handle, name)
  #define _vk_dlerror(msg) *msg = dlerror()
#endif


void *_vkdll_dlinit(char **errorMsg){
  void *handle = (void*)_vk_dlopen();
  _vk_dlerror(errorMsg);
  return handle;
}

void _vkdll_dlclose(void *handle){
  _vk_dlclose(handle);
}

void *_vkdll_dlsym(void *handle, const char *symbol, char **errorMsg){
  void *rez = (void*)_vk_dlsym(handle, symbol);
  _vk_dlerror(errorMsg);
  return rez;
}
