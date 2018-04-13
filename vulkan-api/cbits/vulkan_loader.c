#include "vulkan_loader.h"


void *_vkdll_dlinit(char **errorMsg){
  void *handle = (void*)_vk_dlopen();
  *errorMsg = dlerror();
  return handle;
}

void _vkdll_dlclose(void *handle){
  _vk_dlclose(handle);
}

void *_vkdll_dlsym(void *handle, const char *symbol, char **errorMsg){
  void *rez = (void*)_vk_dlsym(handle, symbol);
  *errorMsg = dlerror();
  return rez;
}
