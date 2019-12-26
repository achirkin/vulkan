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
  vulkan-darwin_1_1_126_0 = requireVulkanMac "1.1.126.0"
    "1ae62cc33227cbb32eff50951011405b0afa7a220765df06a975d6334cc45db4";
  vulkan-darwin_1_1_130_0 = requireVulkanMac "1.1.130.0"
    "d6d80ab96e3b4363be969f9d256772e9cfb8f583db130076a9a9618d2551c726";
  vulkan-darwin = requireVulkanMac "1.1.130.0"
    "d6d80ab96e3b4363be969f9d256772e9cfb8f583db130076a9a9618d2551c726";
})
