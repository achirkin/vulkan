# Getting started with Vulkan on macOS using Haskell

This is for the [vulkan-api](https://github.com/achirkin/vulkan) Haskell library.
There is another vulkan library for Haskell, see the README of vulkan-api for differences.
Instructions tested with cabal v2-commands, not yet with stack.

## Installing Vulkan SDK

Regarding SDK documentation, go by
[Getting Started with the Vulkan SDK (mac)](https://vulkan.lunarg.com/doc/sdk/latest/mac/getting_started.html) rather than
[MoltenVK Runtime User Guide](https://github.com/KhronosGroup/MoltenVK/blob/master/Docs/MoltenVK_Runtime_UserGuide.md).
The latter one describes linking against MoltenVK directly (libMoltenVK.dylib) instead of the Vulkan loader (libvulkan.1.dylib),
which doesn't yield a full Vulkan implementation.
Additional Information can be found in
[Architecture of the Vulkan Loader Interfaces](https://vulkan.lunarg.com/doc/view/latest/windows/loader_and_layer_interface.html).

While the SDK documentation claims it can't be installed, I recommend the following steps to make development easier:

### Option 1, tested

    cd ~
    tar xzf Downloads/vulkan-sdk.tar.gz
    cd vulkansdk-macos-1.1.101.0/macOS
    mkdir ~/.local/lib
    cp -a lib/*.dylib ~/.local/lib
    mkdir ~/.local/share/vulkan
    cp -a etc/vulkan/* ~/.local/share/vulkan
    cp -a bin/* ~/bin

Make sure $HOME/bin is on your path, or use another location.

The Vulkan loader searches for ICDs (installable client drivers) and layers in certain OS-specific locations, including
~/.local/share/vulkan/icd.d and ~/.local/share/vulkan/explicit_layer.d respectively.
The layer search paths are described
[here](https://vulkan.lunarg.com/doc/view/latest/mac/loader_and_layer_interface.html#user-content-macos-layer-discovery),
ICD search paths
[here](https://vulkan.lunarg.com/doc/view/latest/mac/loader_and_layer_interface.html#user-content-icd-discovery-on-macos).

MoltenVK is an ICD and is mandatory on macOS and iOS. It implements Vulkan via the Metal API, because Apple doesn't support Vulkan,
so make sure the Vulkan loader can find MoltenVK.

### Option 2, untested

    cd ~
    tar xzf Downloads/vulkan-sdk.tar.gz

When developing:

    VULKAN_SDK="${HOME}/vulkansdk-macos-1.1.101.0/macOS"
    export VK_LAYER_PATH="${VULKAN_SDK}/etc/vulkan/explicit_layer.d"
    export VK_ICD_FILENAMES="${VULKAN_SDK}/etc/vulkan/icd.d/MoltenVK_icd.json"
    export PATH="${VULKAN_SDK}/bin:${PATH}"
    export DYLD_LIBRARY_PATH="${VULKAN_SDK}/lib"

### Workaround for compile-time linking vulkan-api

The cabal-install-option `--constraint="vulkan-api +useNativeFFI-1-1"` enables compile-time linking of the Vulkan loader in vulkan-api.
This is not static linking, but linking via ld as opposed to dlopen at runtime.
You probably also need `--extra-lib-dirs $VULKAN_SDK/lib` for cabal to find the dylib file when doing this.

As of cabal version 2.4.1.0, cabal doesn't seem to respect DYLD_LIBRARY_PATH or `extra-libraries: vulkan` when linking the executables generated
via hsc2hs that are executed during compile time. The install name of `libvulkan.1.dylib` is `@rpath/libvulkan.1.dylib`, this is
in the metadata of `libvulkan.1.dylib`. Because of that, the executables search for the library in `@rpath` only, which doesn't work.
The executables don't have any @rpath entries, and even if they had, @rpath is relative to the executable path, and the executables
are generated in various build directories.
It can be worked around by changing the install name of the file to an absolute path (assuming $VULKAN_SDK is absolute as above):

    install_name_tool -id "${VULKAN_SDK}/lib/libvulkan.1.dylib" "${VULKAN_SDK}/lib/libvulkan.1.dylib"

You don't need to do this if you use vulkan-api without the useNativeFFI-1-1 or useNativeFFI-1-0 flag.

## Installing the needed Haskell libraries

GLFW supports Vulkan on macOS only since version 3.3, which isn't released yet at the time of writing. You need to use bindings-GLFW and
GLFW-b with a version >= 3.3 and vulkan-api with a version >= 1.1.3.2.

Check if the right versions have been released yet first.
Where to get the right versions depends on if the following pull requests have been merged:
- https://github.com/bsl/bindings-GLFW/pull/65
- https://github.com/bsl/GLFW-b/pull/83
- https://github.com/achirkin/vulkan/pull/27  (vulkan-examples and vulkan-triangles is there as well)

You can use a cabal.project file to get them compiled together with your code, i.e. in vulkan/vulkan-examples the content might be

    packages: ., ../../bindings-GLFW, ../../GLFW-b, ../vulkan-api

When launching Haskell programs that use GLFW-b (via bindings-GLFW), you need to:

    export DYLD_LIBRARY_PATH="~/.local/lib"

You can also prefix your `cabal v2-run/v2-repl` commands with `DYLD_LIBRARY_PATH="~/.local/lib"` instead.

This is because bindings-GLFW currently doesn't do compile time linking against the Vulkan loader (libvulkan.1.dylib).
It will dlopen the lib at runtime regardless of how vulkan-api is linked.

## Pitfalls

Be aware that GLFW and vulkan-api both need a dynamic library that implements Vulkan.
The recommended versions all use `libvulkan.1.dylib`. Older versions might use `libMoltenVK.dylib` or even do static linking.
Make sure that GLFW and vulkan-api use the same library file, otherwise
[bad things](https://github.com/achirkin/vulkan/issues/24) will happen.
