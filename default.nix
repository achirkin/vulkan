let
  nixOverride = ./nixpkgs.json;
  pkgs = let spec = builtins.fromJSON (builtins.readFile nixOverride);
  in (builtins.fetchTarball {
    url = "${spec.url}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  });
in { pkgs' ? pkgs, compiler ? "ghc883" }:
let
  pkgs = import pkgs' { };
  # TODO: On a linux box, vulkan needs to be installed and passed to all the subpackages with the platform appropriate frameworks.
  # and an expression exposing the same api as the below `darwin` attribute should be implemented.
  darwin = import ./nix/default-darwin.nix { inherit pkgs' compiler; };
in if pkgs.stdenv.isDarwin then
  darwin
else
  throw "Only darwin is currently supported!"
