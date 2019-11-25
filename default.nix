{pkgs ? import <nixpkgs> {}, compiler ? "ghc865"}:
let 
    # TODO: vulkan needs to be installed and passed to all the subpackages with the platform appropriate frameworks.
    default = {
            vulkan-examples = pkgs.haskellPackages.callCabal2nix "vulkan-examples" ./vulkan-examples {};
            vulkan-triangles = pkgs.haskellPackages.callCabal2nix "vulkan-triangles" ../vulkan-triangles {};
            vulkan-api = pkgs.haskellPackages.callCabal2nix "vulkan-api" ../vulkan-api {};
    };
    darwin = import ./nix/default-darwin.nix {inherit compiler;};
in
    if pkgs.stdenv.isDarwin 
    then darwin
    else default
