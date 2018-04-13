[![Build Status](https://secure.travis-ci.org/achirkin/vulkan.svg)](http://travis-ci.org/achirkin/vulkan)

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
  * Use no dependencies except `base`.

# vulkan-api [![Hackage](https://img.shields.io/hackage/v/vulkan-api.svg)](https://hackage.haskell.org/package/vulkan-api)

Generated haskell bindings for vulkan api.


  * The generated library is rather big; consider using `-split-objs` or `-split-sections`
    to reduce the size of a project.
    Note, enabling one of these options can make the library compiling painfully
    long time (take some coffee... or watch a movie).

  * By default, the library loads vulkan symbols explicitly dynamically at runtime.
    Therefore, it does not even link to the vulkan loader library at compile time.

  * The library provides `useNativeFFI-x-y` flags that enable haskell FFI functions
    for vulkan core `x.y` symbols.
    Turning on any of these flags enables compile-time linking to the vulkan loader library.

  * All available extension functions can be found at runtime using simple lookup
    functions in `Graphics.Vulkan.Marshal.Proc` module.


Tested using `stack` on:

  * Windows 10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)
  * Mac OS High Sierra 10.13.4 with [MoltenVK](https://github.com/KhronosGroup/MoltenVK)
  * Ubuntu 17.10 x64 with [LunarG Vulkan SDK](https://www.lunarg.com/vulkan-sdk/)


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




## TODO

##### vulkan-api

 * [x] Try to build it on various platforms, check if specifying foreign code calling
       convention is necessary.
 * [ ] Remove unsafe FFI call to functions that could break at runtime.
       Currently we have both safe and unsafe versions for every function.
 * [x] Figure out if it is necessary to have `extra-libraries: vulkan` on various platforms
       (or, maybe, `extra-ghci-libraries` is enough?).
       An alternative would be to make a C stub to get all functions via `vk***ProcAddr`,
       which seems not the best option due to performance considerations of doing
       dynamic wrapping FFI.
 * [ ] Make `Graphics.Vulkan.Marshal.Create` fill `sType` fields automatically,
       together with optional fields
 * [x] Make `Graphics.Vulkan.Marshal.Create` provide more meaningful error
       messages when types of fields mismatch.
 * [ ] Check whether we can disallow writing `returnedonly` fields.
 * [x] Investigate the need to use the extension loader (`vulkan-docs/src/ext_loader`).
       `Graphics.Vulkan.Marshal.Proc` seems to be good enough for this low-level binding.
 * [ ] `vkGetProc` and `vkLookupProc` currently lookup functions in a shared library,
       even if vulkan is linked statically. This can be dangerous! Need to check it.

##### genvulkan

 * [ ] `VkXml.Sections.Commands`: parse command parameters more robustly,
       maybe use `language-c` package for that.
       Make parsing more compliant with the registry spec.
 * [ ] `VkXml.Sections.Types` `parseVkTypeData` needs a cleaner rewrite.
      Especially, check if type and member names are parsed correctly.
