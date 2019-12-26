{ mkDerivation, base, stdenv, vulkan }:
mkDerivation {
  pname = "vulkan-api";
  version = "1.3.0.0";
  src = ./.;
  librarySystemDepends = [ vulkan ];
  libraryHaskellDepends = [ base ];
  executableSystemDepends = [ vulkan ];
  configureFlags = [ "--extra-lib-dirs=${vulkan}" ]; # probably can drop this since nix seems to pipe vulkan through well enough on its own
  homepage = "https://github.com/achirkin/vulkan#readme";
  description = "Low-level low-overhead vulkan api bindings";
  license = stdenv.lib.licenses.bsd3;
}
