{ stdenv, requireFile, lib }:

let
  requireVulkanMac = version: sha256:
    let
      tar = "vulkansdk-macos-" + version + ".tar.gz";
      app = requireFile rec {
        name = tar;
        url = "https://vulkan.lunarg.com/sdk/home";
        #   hashMode = "recursive";
        inherit sha256;
        message = ''
          Unfortunately, we cannot download ${name} automatically.
          Please go to ${url}
          to download it yourself, and add it to the Nix store by running the following commands.
          nix-store --add-fixed sha256 ${name}
          rm -rf ${name}
        '';
      };
      meta = with stdenv.lib; {
        homepage = "https://vulkan.lunarg.com/sdk/home";
        description = "Vulkan SDK";
        license = licenses.unfree;
        platforms = platforms.darwin;
      };

    in app.overrideAttrs (oldAttrs: oldAttrs // { inherit meta; });

in lib.makeExtensible (self: {
  vulkan-darwin_1_1_130_0 = requireVulkanMac "1.1.130.0"
    "d6d80ab96e3b4363be969f9d256772e9cfb8f583db130076a9a9618d2551c726";
  vulkan-darwin_1_2_131_1 = requireVulkanMac "1.2.131.1"
    "fbc01a433d61d303dd3ceef19a12a99ba3431498de2d761679073f12b05a7870";
  vulkan-darwin_1_2_131_2 = requireVulkanMac "1.2.131.2"
    "e28363ae0bdb3d881ebf93cdd7a721d052f6a2e5686d0fb3447e6edd585bb53f";
  vulkan-darwin_1_2_135_0 = requireVulkanMac "1.2.135.0"
    "81da27908836f6f5f41ed7962ff1b4be56ded3b447d4802a98b253d492f985cf";
  # vulkan-darwin = requireVulkanMac "1.2.135.0" #TODO: check the changelogs for file structure; this version
  # isn't picked up by GLFW
  #   "81da27908836f6f5f41ed7962ff1b4be56ded3b447d4802a98b253d492f985cf";
  vulkan-darwin = requireVulkanMac "1.2.131.2"
    "e28363ae0bdb3d881ebf93cdd7a721d052f6a2e5686d0fb3447e6edd585bb53f";
})
