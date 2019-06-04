{ stdenv, fetchurl, localVulkanSdktargz}:

stdenv.mkDerivation rec {
  name = "vulkan-darwin";
  version = "1.0.0";

  src = localVulkanSdktargz;
#   unpackPhase = "true"; this will skip unpack
  unpackCmd = '' tar xzf $curSrc '';
  installPhase = ''
    mkdir -p $out
    cd macos
    cp -a . $out
    
  '';

}
# need vulkan loader (in vulkan-api) to find ICDs.  currently can only do this with env vars
# export VK_LAYER_PATH="${VULKAN_SDK}/etc/vulkan/explicit_layer.d"
# export VK_ICD_FILENAMES="${VULKAN_SDK}/etc/vulkan/icd.d/MoltenVK_icd.json"
# export PATH="${VULKAN_SDK}/bin:${PATH}"
# export DYLD_LIBRARY_PATH="${VULKAN_SDK}/lib"

# yeah, that was deegguing against the actual url
# o1lo01ol1o
# but I'll do a local file
# clever
# some websites force you to agree to a license
# clever
# and if you didnt, it gives html asking you to do so
# clever
# https://github.com/NixOS/nixpkgs/blob/5f106e8aae930c7f48a50cf20c1c92d80ca733c8/pkgs/os-specific/darwin/xcode/default.nix#L18-L33
# clever
# o1lo01ol1o: requireFile can deal with that issue
# clever
# you give a url, sha256, and a msg to the user
# clever
# and then the user must download the file, and run nix-store --add-fixed on it

# if the binaries are in $out/bin/, and you then add it to buildInputs, it will be added to PATH at build time

# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/generic-builder.nix#L37