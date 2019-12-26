{ pkgs', compiler ? "ghc864" }:
let
  # MoltenVK
  # vulkan = import (pkgs.fetchgit {
  #   url = "https://github.com/o1lo01ol1o/MoltenVK-nix.git";
  #   rev = "524b0699cc3785f219e8cc597f996e7c48232ea2";
  #   sha256 = "1azp3jihxsxgmv5mwd9yw2zznvjmzl0p7s0dpjvm48xl3fvb5ks6";
  # }) { };

  # vulkan = import /Users/timpierson/arity/MoltenVK/default.nix {};
  easytensor-src = builtins.fetchGit {
    url = "https://github.com/achirkin/easytensor.git";
    rev = "3ef88ab3bfc095d131365a588fe3282a2be5d0c1";
  };
  mkVulkan = pkgs:
    pkgs.callPackage ./vulkan.nix {
      localVulkanSdktargz = (pkgs.callPackage ./lunarSDK.nix { }).vulkan-darwin;
    };

  moltenOverrides = vulkan: drv: {
    VK_LAYER_PATH = "${vulkan}/etc/vulkan/explicit_layer.d";
    VK_ICD_FILENAMES = "${vulkan}/etc/vulkan/icd.d/MoltenVK_icd.json";
    DYLD_LIBRARY_PATH = "${vulkan}/lib";
  };

  mkGLFWFrameworks = pkgs:
    let frameworks = pkgs.darwin.apple_sdk.frameworks;
    in [
      frameworks.Foundation
      frameworks.OpenGL
      frameworks.Cocoa
      frameworks.CoreVideo
    ];

  overlayShared = pkgsSelf: pkgsSuper: {
    vulkan = (mkVulkan pkgsSuper);
    haskell = pkgsSuper.haskell // {
      packages = pkgsSuper.haskell.packages // {
        "${compiler}" = pkgsSuper.haskell.packages."${compiler}".override
          (old: {
            overrides = let
              mkMoltenExtension = haskellPackagesSelf: haskellPackagesSuper:
                let
                  frameworks = pkgsSuper.darwin.apple_sdk.frameworks;
                  glfwFrameworks = mkGLFWFrameworks pkgsSuper;
                  vulkan-api = pkgsSuper.lib.overrideDerivation
                    (pkgsSuper.haskell.lib.addBuildDepend
                      (haskellPackagesSelf.callCabal2nix "vulkan-api"
                        ../vulkan-api { }) (mkVulkan pkgsSuper))
                    (moltenOverrides (mkVulkan pkgsSuper));
                in {
                  inherit vulkan-api;

                  monad-par = pkgsSuper.haskell.lib.dontCheck
                    haskellPackagesSuper.monad-par;

                  # until bindings-GLFW and GLFW-b are updated to support GLFW 3.3 we override
                  bindings-GLFW = pkgsSuper.lib.overrideDerivation
                    (haskellPackagesSuper.callCabal2nix "bindings-GLFW"
                      (pkgsSuper.fetchgit {
                        url = "https://github.com/bsl/bindings-GLFW.git";
                        rev = "3b300787078ec12d0ef6ac10ced031ecb2b11e6d";
                        sha256 =
                          "07sixxmhcnzxi9xc4j5bg9qwac27jdnx0sl239si5hi7isyja6wm";
                      }) { }) (drv: {

                        buildInputs = (drv.buildInputs ++ [
                          frameworks.Cocoa
                          # frameworks.Security
                          frameworks.CoreFoundation
                          frameworks.CoreServices
                          frameworks.AGL
                          frameworks.CoreVideo
                          frameworks.CoreGraphics
                          frameworks.IOKit
                          frameworks.CoreMediaIO
                          frameworks.OpenGL
                          frameworks.IOSurface
                          frameworks.Metal
                          frameworks.Carbon
                          frameworks.GLUT
                          frameworks.GameController
                          frameworks.IOSurface
                          frameworks.Kernel # think just need Kernel and Foundation; and Cocoa, CoreVideo, Graphics, IOKit, OpenGL
                        ]);
                      });

                  # tests fail to return expected version
                  GLFW-b = pkgsSuper.lib.overrideDerivation
                    (pkgsSuper.haskell.lib.dontCheck
                      (haskellPackagesSuper.callCabal2nix "GLFW-b"
                        (pkgsSuper.fetchgit {
                          url = "https://github.com/bsl/GLFW-b.git";
                          rev = "96ae257f7b6b6058d5ff41809757d69fd5a03287";
                          sha256 =
                            "1daazpnc7y6k3aky9833d2qhn7ba769hbwmir0dvag7l3hkbwa94";
                        }) { })) (drv: {
                          buildInputs = (drv.buildInputs ++ glfwFrameworks);
                        });
                  vulkan = mkVulkan pkgsSuper;
                  easytensor-vulkan =
                    haskellPackagesSelf.callCabal2nix "easytensor-vulkan"
                    (easytensor-src + /easytensor-vulkan) {
                    };
                  vulkan-triangles = pkgsSuper.lib.overrideDerivation
                    (haskellPackagesSuper.callPackage ../vulkan-triangles {
                      inherit glfwFrameworks vulkan-api;
                    }) (moltenOverrides (mkVulkan pkgsSuper));
                };
            in pkgsSelf.lib.fold pkgsSelf.lib.composeExtensions
            (old.overrides or (_: _: { })) [ mkMoltenExtension ];
          });
      };
    };
  };

  config = {
    allowBroken = true; # easytensor is marked as broken.
    allowUnfree = true;
  };

  nixpkgs = import pkgs' {
    inherit config;
    overlays = [ overlayShared ];
  };

in let
  glfwFrameworks = mkGLFWFrameworks nixpkgs;
  base-compiler = nixpkgs.haskell.packages."${compiler}";
in {
  inherit moltenOverrides glfwFrameworks overlayShared base-compiler;
  inherit (base-compiler) vulkan-api vulkan vulkan-triangles;
}
# test with:
# nix-build . -A vulkan-triangles && cd vulkan-trianges && nix-shell .. -A vulkan-triangles --command '../result/bin/vulkan-triangles'
