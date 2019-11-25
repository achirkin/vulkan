{ compiler ? "ghc865"}:
let
  vulkan = import ./vulkan.nix {
    stdenv = pkgs.stdenv;
    fetchurl = pkgs.fetchurl;
    localVulkanSdktargz = (pkgs.callPackage ./vulkan-darwin.nix {}).vulkan-darwin;
  };

  frameworks = pkgs.darwin.apple_sdk.frameworks;
  glfwFrameworks = [
    frameworks.Foundation
    frameworks.OpenGL
    frameworks.Cocoa
    frameworks.CoreVideo
  ];
  config = {
    allowBroken = true; # easytensor is marked as broken.
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.${compiler}.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {

          monad-par = pkgs.haskell.lib.dontCheck haskellPackagesOld.monad-par;

          # until bindings-GLFW and GLFW-b are updated to support GLFW 3.3 we override
          bindings-GLFW = pkgs.lib.overrideDerivation
          (haskellPackagesNew.callCabal2nix "bindings-GLFW" (pkgs.fetchgit {
            url = "https://github.com/bsl/bindings-GLFW.git";
            rev = "3b300787078ec12d0ef6ac10ced031ecb2b11e6d";
            sha256 = "07sixxmhcnzxi9xc4j5bg9qwac27jdnx0sl239si5hi7isyja6wm";
          }) { }) (drv: {

            buildInputs = (drv.buildInputs ++ [
              frameworks.Cocoa
              frameworks.Security
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
          GLFW-b = pkgs.lib.overrideDerivation (pkgs.haskell.lib.dontCheck
          (haskellPackagesNew.callCabal2nix "GLFW-b" (pkgs.fetchgit {
            url = "https://github.com/bsl/GLFW-b.git";
            rev = "96ae257f7b6b6058d5ff41809757d69fd5a03287";
            sha256 = "1daazpnc7y6k3aky9833d2qhn7ba769hbwmir0dvag7l3hkbwa94";
          }) { }))
          (drv: { buildInputs = (drv.buildInputs ++ glfwFrameworks); });

        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  # helper to set env variables
  vulkanEnvHooks = { vulkan, drv }: {
    VK_LAYER_PATH = "${vulkan}/etc/vulkan/explicit_layer.d";
    VK_ICD_FILENAMES = "${vulkan}/etc/vulkan/icd.d/MoltenVK_icd.json";
    DYLD_LIBRARY_PATH = "${vulkan}/lib";
  };

in let
  vulkan-api = pkgs.lib.overrideDerivation
  (pkgs.haskellPackages.callPackage ../vulkan-api { vulkan = vulkan; }) (drv:
  vulkanEnvHooks {
    vulkan = vulkan;
    drv = drv;
  });
in {
  vulkan-examples = pkgs.lib.overrideDerivation
  (pkgs.haskellPackages.callPackage ../vulkan-examples {
    glfwFrameworks = glfwFrameworks;
    vulkan = vulkan;
    vulkan-api = vulkan-api;
  }) (drv:
  vulkanEnvHooks {
    vulkan = vulkan;
    drv = drv;
  });
  vulkan-triangles = pkgs.lib.overrideDerivation
  (pkgs.haskellPackages.callPackage ../vulkan-triangles {
    glfwFrameworks = glfwFrameworks;
    vulkan = vulkan;
    vulkan-api = vulkan-api;
  }) (drv:
  vulkanEnvHooks {
    vulkan = vulkan;
    drv = drv;
  });
  # re-exports
  glfwFrameworks = glfwFrameworks;
  vulkan = vulkan;
  vulkan-api = vulkan-api;
  vulkanEnvHooks = drv: vulkanEnvHooks {vulkan = vulkan; drv = drv; };
}
# test with: nix-shell -A vulkan-triangles --command 'result-4/bin/vulkan-triangles'