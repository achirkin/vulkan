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
  * Document the generated code as much as possible with references to vulkan registry.
  * Use no dependencies except `base` and `ghc-prim`.

# vulkan-api

Generated haskell bindings for vulkan api.

# genvulkan

Generate haskell vulkan sources using vk.xml file.


# vulkan-examples

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
       One way to go:

       1. Find all pairs of `Vk**Flags` and `Vk**FlagBits` types.
       2. Define type parameters `data VkFlagType = Bit | Mask`.
       3. Define a common `newtype Vk**FB (a :: VkFlagType) = Vk**FB VkFlags`
       3. Make all patterns parameter-polymorphic.
       4. Optionally, make converting functions.

 * [ ] `VkXml.Sections.Commands`: parse command parameters more robustly,
       maybe use `language-c` package for that.
       Make parsing more compliant with the registry spec.
 * [ ] `VkXml.Sections.Types` `parseVkTypeData` needs a cleaner rewrite.
      Especially, check if type and member names are parsed correctly.
