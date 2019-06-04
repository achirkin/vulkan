{ mkDerivation, base, containers, directory, filepath, GLFW-b
, process, stdenv, template-haskell, vulkan-api, vulkan, glfwFrameworks
}:
mkDerivation {
  pname = "vulkan-examples";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableSystemDepends = [ vulkan ] ++ glfwFrameworks;
  # VK_LAYER_PATH="${vulkan}/etc/vulkan/explicit_layer.d";
  # VK_ICD_FILENAMES="${vulkan}/etc/vulkan/icd.d/MoltenVK_icd.json";
  # DYLD_LIBRARY_PATH="${vulkan}/lib";
  # shellHook = ''
  #   export VK_LAYER_PATH="${vulkan}/etc/vulkan/explicit_layer.d"
  #   export VK_ICD_FILENAMES="${vulkan}/etc/vulkan/icd.d/MoltenVK_icd.json"
  #   export DYLD_LIBRARY_PATH="${vulkan}/lib"
  # '';
  executableHaskellDepends = [
    base containers directory filepath GLFW-b process template-haskell
    vulkan-api
  ];
  homepage = "https://github.com/achirkin/genvulkan#readme";
  description = "Use cases for vulkan-api";
  license = stdenv.lib.licenses.bsd3;
}
