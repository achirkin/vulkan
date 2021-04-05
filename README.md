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

  * Windows 10 x64 with [LunarG Vulkan SDK](https://vulkan.lunarg.com/sdk/home#windows)
  * Mac OS High Sierra 10.13.4 with
     [LunarG Vulkan SDK](https://vulkan.lunarg.com/sdk/home#mac)
     and
     [MoltenVK](https://github.com/KhronosGroup/MoltenVK).
    See `README-macOS.md` for the Mac OS setup tutorial.
  * Ubuntu 17.10 x64 with [LunarG Vulkan SDK](https://vulkan.lunarg.com/sdk/home#linux)

## Status update vulan-api-1.4 (2021.04.05)

Vulkan-Docs changed between version 1.1 and 1.2 a lot, which made adapting genvulkan
rather hard.
At this point, I decided to modify the generated code manually until I come up with a better way
to generate haskell code fully automatically (I expect this would require a rather large refactoring).

The current semi-generated code matches v. 1.2.174 of Vulkan-Docs vk.xml.
Here are some manual adjustments I've had to make:

  - `VkAccelerationStructureInstanceKHR` has bitfields and not processed by hsc2hs and does not fit `VulkanMarshal.StructRep`;
    the manual class instance workarounds this (rather inconveniently).
  - A few new cyclic module dependencies must have been fixed with manual .hs-boot
  - `Graphics.Vulkan.Ext.VK_NV_ray_tracing` and some related structs are hidden behind `enableBetaExtensions` flag
    (seems to compile with the flag enabled though)

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


## Why another Haskell bindings?

The generated bindings [`vulkan-api`](https://hackage.haskell.org/package/vulkan-api)
are not the only Haskell bindings for Vulkan API.
There is another package, called [`vulkan`](https://hackage.haskell.org/package/vulkan)
that started in 2016.
The main reason for me to write this new package two years later was that `vulkan`
package was abandoned for a while and required significant efforts to be compiled
at the time this project started in January 2018
(as of April 2018 things seem to have changed and that package is great again :) ).
However, the are a few design decisions that render `vulkan` and `vulkan-api` quite different.
The main difference is that `vulkan` uses regular Haskell data types plus `DuplicateRecordFields` to manipulate Vulkan objects,
whereas `vulkan-api` uses wrapped pinned byte arrays plus type classes and `TypeApplications`;
as a result:

  * Creating and composing data types in `vulkan` is very close to normal haskell way
    of doing that (modulo the need to manually allocate pointers).
    Creating and composing data types in `vulkan-api` is done via
    [`VulkanMarshal`](https://github.com/achirkin/vulkan/blob/master/vulkan-api/src/Graphics/Vulkan/Marshal.hs#L87)
    class.
    There are helpers for managing memory in `Graphics.Vulkan.Marshal.Create` module,
    you can find some examples in the [repository](https://github.com/achirkin/vulkan/blob/master/vulkan-triangles/src/Lib/Vulkan/Drawing.hs#L81).

  * Duplicate field names in `vulkan` structure, such as `sType` use `DuplicateRecordFields`
    and often require you writing a lot of type signatures explicitly,
    which can be very annoying.
    Things will become better with record type inference and `OverloadedRecordFields` extension;
    but this is not implemented even in GHC 8.4 yet.

  * Writing structure fields in `vulkan-api` is done via type classes (and heavy inlining);
    thus, overloading with custom data types is extremely easy
    (e.g. writing vectors or bytearrays directly into vulkan structures).
    That comes at the cost of a not particularly novice-friendly interface.

  * Low overheads: `vulkan-api` structures can be converted to and from C pointers for FFI
    doing zero copying.
    There is no need to `peek` all fields of a structure to read one of them.

There is a number of smaller things:

  * `vulkan-api` has different `vkGetXxxProc` machinery for loading Vulkan symbols dynamically,
    check out [`Graphics.Vulkan.Marshal.Proc`](https://github.com/achirkin/vulkan/blob/master/vulkan-api/src/Graphics/Vulkan/Marshal/Proc.hs)
    for that.

  * `vulkan-api` keeps all Vulkan extension names in `Ptr CString` bi-directional patterns,
    which eliminates the need to `alloca` when feeding them to Vulkan functions.

  * Most of the constants in `vulkan-api` are duplicated at type level using `Nat` and `Symbol`,
    which should allow more type-level programming and fancy high-level wrapppers.
