{ mkDerivation, aeson, base, conduit, conduit-extra, containers
, deepseq, exceptions, ghc-prim, haskell-src-exts
, haskell-src-exts-sc, hfmt, lens, mmorph, mtl, neat-interpolation
, path, path-io, pretty, primitive, regex, resourcet, stdenv
, template-haskell, text, transformers, xml-conduit, xml-types
}:
mkDerivation {
  pname = "genvulkan";
  version = "1.1.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base conduit conduit-extra containers deepseq exceptions
    ghc-prim haskell-src-exts haskell-src-exts-sc hfmt lens mmorph mtl
    neat-interpolation path path-io pretty primitive regex resourcet
    template-haskell text transformers xml-conduit xml-types
  ];
  executableHaskellDepends = [ base path path-io ];
  homepage = "https://github.com/achirkin/genvulkan#readme";
  description = "Generate vulkan-api haskell bindings";
  license = stdenv.lib.licenses.bsd3;
}
