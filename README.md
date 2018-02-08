The aim of this project is to provide low-level low-overhead haskell bindings to vulkan api.
Features of the bindings:

  * Keep as close as possible to vulkan naming conventions unless they violate
    haskell syntax. This involves heavy usage of `PatternSynonyms` extension,
    and occasional violation of camel case.
  * Provide as much as possible information at type level, but allow avoiding
    any overheads related to it.
    Compile-time constants are duplicated at type level, but it is not necessary
    to use them.
  * Do not introduce type marshalling overheads.
    All vulkan structures have `ByteArray#` runtime representation,
    allowing zero-copy conversion to and from pointers.
    Moreover, it is not necessary to convert them at all, if one prefers to
    manage corresponding memory manually.

# vulkan-api

Generated haskell bindings for vulkan api.

# genvulkan

Generate haskell vulkan sources using vk.xml file.


# bulkan-examples

Examples of programs using vulkan-api bindings.


## Building


Note, you need modern drivers to support ulkan api, e.g. nvidia drivers 367+.
If your drivers are ok, install vulkan libs and everything should be fine.

Basically, ghc should be able to see vulkan dynamic library
and to discover "vulkan/vulkan.h" header file.

On ubuntu, these can be installed as follows:
```bash
sudo apt-get install libvulkan-dev
```


## TODO

 * [ ] Check if it is safe to do all foreign calls unsafe,
       mark those calls that have to be safe (all calls are unsafe currently).
 * [ ] Check if it is possible to hide `VkPtr` constructor.
 * [ ] Find the best way to represent pairs of `Vk**Flags` and `Vk**FlagBits` types.
