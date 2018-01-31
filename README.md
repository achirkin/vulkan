# genvulkan

Generate haskell vulkan sources using vk.xml file.

# vulkan-api

Generated haskell bindings for vulkan api.

# bulkan-examples

Examples of programs using vulkan-api bindings.


## Building

Note, you need modern drivers to support vulkan api, e.g. nvidia drivers 367+.
If your drivers are ok, install vulkan libs and everything should be fine.
On ubuntu, these can be installed as follows::
```bash
sudo apt-get install libvulkan-dev
```

## TODO

 * [ ] Check if it is safe to do all foreign calls unsafe,
       mark those calls that have to be safe (all calls are unsafe currently).
 * [ ] Check if it is possible to hide `VkPtr` constructor.
