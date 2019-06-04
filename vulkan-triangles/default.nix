{ mkDerivation, base, containers, dimensions, directory, easytensor
, easytensor-vulkan, filepath, GLFW-b, monad-logger, mtl, process
, stdenv, template-haskell, time, transformers, vulkan-api, vulkan, glfwFrameworks
}:
mkDerivation {
  pname = "vulkan-triangles";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  librarySystemDepends = [ vulkan ] ++ glfwFrameworks;
  executableSystemDepends = [ vulkan ] ++ glfwFrameworks;
  libraryHaskellDepends = [
    base containers dimensions directory easytensor easytensor-vulkan
    filepath GLFW-b monad-logger mtl process template-haskell time
    transformers vulkan-api
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/achirkin/genvulkan#readme";
  description = "Haskell-vulkan program rendering triangles";
  license = stdenv.lib.licenses.bsd3;
}
