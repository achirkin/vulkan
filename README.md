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
To update the api bindings, run `genvulkan` using stack with this project folder:
```bash
cd genvulkan
stack build
stack exec genvulkan
```

# vulkan-examples

Examples of programs using vulkan-api bindings.
Consists of several executables implementing steps of vulkan-tutorial.com.
This is the easiest way to familiarize yourself with the library.

Prerequisites
  * For validation layers to work, you need to have Vulkan SDK installed,
    get it on [vulkan.lunarg.com](https://vulkan.lunarg.com/).
  * Some examples compile shaders using `glslangValidator` via TH, so the tool must be in your `PATH`
    (it is included in Vulkan SDK).
  * Windowing is done via [GLFW](http://www.glfw.org/), so you may need to have it on your system,
    version 3.2 or newer.


# vulkan-triangles

A more haskell-style example of a vulkan program.
This is a combined result of programs in `vulkan-examples` with a little cleaner code.

## Building


Note, you need modern drivers to support ulkan api, e.g. nvidia drivers 367+.
If your drivers are ok, install vulkan libs and everything should be fine.

Basically, ghc should be able to see vulkan dynamic library and to discover header files.

On ubuntu, these can be installed as follows:
```bash
sudo apt-get install libvulkan-dev
```


## TODO

##### vulkan-api

 * [ ] Try to build it on various platforms, check if specifying foreign code calling
       convention is necessary.
 * [ ] Remove unsafe FFI call to functions that could break at runtime.
       Currently we have both safe and unsafe versions for every function.
 * [ ] Figure out if it is necessary to have `extra-libraries: vulkan` on various platforms
       (or, maybe, `extra-ghci-libraries` is enough?)
 * [ ] Make `Graphics.Vulkan.Marshal.Create` fill `sType` fields automatically,
       together with optional fields
 * [x] Make `Graphics.Vulkan.Marshal.Create` provide more meaningful error
       messages when types of fields mismatch.
 * [ ] Check whether we can disallow writing `returnedonly` fields.
 * [ ] Investigate the need to use the extension loader (`vulkan-docs/src/ext_loader`).

##### genvulkan

 * [ ] `VkXml.Sections.Commands`: parse command parameters more robustly,
       maybe use `language-c` package for that.
       Make parsing more compliant with the registry spec.
 * [ ] `VkXml.Sections.Types` `parseVkTypeData` needs a cleaner rewrite.
      Especially, check if type and member names are parsed correctly.
