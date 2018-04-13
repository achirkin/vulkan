#ifndef VULKAN_LOADER_H_
#define VULKAN_LOADER_H_ 1

void *_vkdll_dlinit(char **errorMsg);
void  _vkdll_dlclose(void *handle);
void *_vkdll_dlsym(void *handle, const char *symbol, char **errorMsg);

#endif
