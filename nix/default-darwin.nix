{ pkgs', compiler ? "ghc883" }:
let

  mkVulkan = pkgs:
    pkgs.callPackage ./vulkan.nix {
      localVulkanSdktargz = (pkgs.callPackage ./lunarSDK.nix { }).vulkan-darwin;
    };

  moltenHooks = vulkan: drv: {
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
                    (moltenHooks (mkVulkan pkgsSuper));
                in {
                  inherit vulkan-api;

                  monad-par = pkgsSuper.haskell.lib.dontCheck
                    haskellPackagesSuper.monad-par;

                  # Need GLFW 3.3 
                  bindings-GLFW = pkgsSuper.lib.overrideDerivation
                    (haskellPackagesSuper.callCabal2nix "bindings-GLFW"
                      (pkgsSuper.fetchgit {
                        url = "https://github.com/bsl/bindings-GLFW.git";
                        rev = "e3a625f81ff490fef6e867abde20db50e7f26fa0";
                        sha256 =
                          "006fzr1jkya5dwrl5vgm2in3arpg7bxzv6flilpkr1vk6q94h8pc";
                      }) { }) (drv: {

                        buildInputs = (drv.buildInputs ++ [
                          frameworks.Cocoa
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
                          frameworks.Kernel # TODO: think just need Kernel and Foundation; and Cocoa, CoreVideo, Graphics, IOKit, OpenGL
                        ]);
                      });

                  GLFW-b = pkgsSuper.lib.overrideDerivation
                    (haskellPackagesSuper.callHackage "GLFW-b" "3.3.0.0" { }) (drv: {
                        buildInputs = (drv.buildInputs ++ glfwFrameworks);
                      });
                  vulkan = mkVulkan pkgsSuper;

                  wavefront = (haskellPackagesSuper.callCabal2nix "wavefront"
                      (pkgsSuper.fetchgit {
                        url = "https://github.com/phaazon/wavefront.git";
                        rev = "74dc61e8e298dce19c1c50ebf21d1c6fe841458c";
                        sha256 =
                          "0ihbhkwqmw6s3s1jlq3xiyxfvpdasg3qq80y021yq858hd087m8k";
                      }) { });

                  vulkan-triangles = pkgsSuper.lib.overrideDerivation
                    (haskellPackagesSuper.callPackage ../vulkan-triangles {
                      inherit glfwFrameworks vulkan-api;
                    }) (moltenHooks (mkVulkan pkgsSuper));
                  vulkan-examples = pkgsSuper.lib.overrideDerivation
                    (haskellPackagesSuper.callPackage ../vulkan-examples {
                      inherit glfwFrameworks vulkan-api;
                    }) (moltenHooks (mkVulkan pkgsSuper));
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
  inherit glfwFrameworks overlayShared base-compiler;
  inherit (base-compiler) vulkan-api vulkan vulkan-triangles vulkan-examples;
  moltenHooks = moltenHooks nixpkgs.vulkan;
}
# test with:
# nix-build . -A vulkan-triangles && cd vulkan-triangles && nix-shell .. -A vulkan-triangles --command '../result/bin/vulkan-triangles'
