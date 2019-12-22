let
  nixOverride = ./nixpkgs.json;
  pkgs = let spec = builtins.fromJSON (builtins.readFile nixOverride);
  in (builtins.fetchTarball {
    url = "${spec.url}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  });
in { pkgs' ? pkgs, compiler ? "ghc865" }:
let

  pkgs = import pkgs' { };
  # TODO: vulkan needs to be installed and passed to all the subpackages with the platform appropriate frameworks.
  default = {
    vulkan-examples =
      pkgs.haskellPackages.callCabal2nix "vulkan-examples" ./vulkan-examples
      { };
    vulkan-triangles =
      pkgs.haskellPackages.callCabal2nix "vulkan-triangles" ../vulkan-triangles
      { };
    vulkan-api =
      pkgs.haskellPackages.callCabal2nix "vulkan-api" ../vulkan-api { };
  };
  darwin = import ./nix/default-darwin.nix { inherit pkgs' compiler; };
in if pkgs.stdenv.isDarwin then darwin else default
