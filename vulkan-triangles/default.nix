{ mkDerivation, base, containers, dimensions, directory, easytensor
, easytensor-vulkan, filepath, GLFW-b, glfwFrameworks, JuicyPixels, monad-logger
, mtl, process, stdenv, template-haskell, time, transformers
, vector, vulkan-api, wavefront, vulkan
}:
mkDerivation {
  pname = "vulkan-triangles";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers dimensions directory easytensor glfwFrameworks easytensor-vulkan
    filepath GLFW-b JuicyPixels monad-logger mtl process vulkan
    template-haskell time transformers vector vulkan-api wavefront
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/achirkin/genvulkan#readme";
  description = "Haskell-vulkan program rendering triangles";
  license = stdenv.lib.licenses.bsd3;
}
