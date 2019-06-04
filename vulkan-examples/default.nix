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
  executableHaskellDepends = [
    base containers directory filepath GLFW-b process template-haskell
    vulkan-api
  ];
  homepage = "https://github.com/achirkin/genvulkan#readme";
  description = "Use cases for vulkan-api";
  license = stdenv.lib.licenses.bsd3;
}
