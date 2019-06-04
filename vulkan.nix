{ stdenv, fetchurl, localVulkanSdktargz}:

stdenv.mkDerivation rec {
  name = "vulkan-darwin";
  version = "1.0.0";

  src = localVulkanSdktargz;
  unpackCmd = '' tar xzf $curSrc '';
  installPhase = ''
    mkdir -p $out
    cd macos
    cp -a . $out
    
  '';

}
