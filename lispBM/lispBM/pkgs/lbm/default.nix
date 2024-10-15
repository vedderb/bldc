{ self
, supportedSystems
, nix-filter

, callPackage
, pkgsi686Linux
}:

rec {
  # TODO: add support for building the LBM static library.
  repl64 = callPackage ./repl { inherit self supportedSystems nix-filter; };
  repl = repl64.override {
    build32 = true;
    readline = pkgsi686Linux.readline;
    libpng = pkgsi686Linux.libpng;
  };
  doc = callPackage ./doc { inherit self supportedSystems nix-filter; };
  c-doc = callPackage ./c-doc { inherit self supportedSystems nix-filter; };
}