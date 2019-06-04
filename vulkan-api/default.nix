{ mkDerivation, base, stdenv, vulkan }:
mkDerivation {
  pname = "vulkan-api";
  version = "1.1.4.0";
  src = ./.;
  librarySystemDepends = [ vulkan ];
  libraryHaskellDepends = [ base ];
  executableSystemDepends = [ vulkan ];
  # VK_LAYER_PATH="${vulkan}/etc/vulkan/explicit_layer.d";
  # VK_ICD_FILENAMES="${vulkan}/etc/vulkan/icd.d/MoltenVK_icd.json";
  # DYLD_LIBRARY_PATH="${vulkan}/lib";
  # shellHook = ''
  #   export VK_LAYER_PATH="${vulkan}/etc/vulkan/explicit_layer.d"
  #   export VK_ICD_FILENAMES="${vulkan}/etc/vulkan/icd.d/MoltenVK_icd.json"
  #   export DYLD_LIBRARY_PATH="${vulkan}/lib"
  # '';
  configureFlags = [ "--extra-lib-dirs=${vulkan}" ]; # probably can drop this since nix seems to pipe vulkan through well enough on its own
  homepage = "https://github.com/achirkin/vulkan#readme";
  description = "Low-level low-overhead vulkan api bindings";
  license = stdenv.lib.licenses.bsd3;
}
