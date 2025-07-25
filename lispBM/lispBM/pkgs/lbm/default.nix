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
    SDL2 = pkgsi686Linux.SDL2;
    SDL2_image = pkgsi686Linux.SDL2_image;
  };
  doc = callPackage ./doc { inherit self supportedSystems nix-filter; };
  c-doc = callPackage ./c-doc { inherit self supportedSystems nix-filter; };
}
