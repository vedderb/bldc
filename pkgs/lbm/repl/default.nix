{ build32 ? false
, supportedSystems

, self
, nix-filter

, multiStdenv
, readline
, libpng
, gcc_multi
}:

let
  makeTarget = if build32 then "all" else "all64";
  name = if build32 then "lbm" else "lbm64";
in multiStdenv.mkDerivation {
  pname = name;
  # IDK what pattern should be used to get ahold of the version number...
  version = self.shortRev or self.rev or self.dirtyShortRev or "unknown";
  
  meta = {
    description = "LispBM repl";
    platforms = supportedSystems;
  };
  
  src = nix-filter {
    root = self;
    include = with nix-filter.lib; [
      "src"
      (and
        "repl"
        (or_ isDirectory (or_ (matchExt "c") (matchExt "h")))
      )
      "include"
      "platform"
      "repl/Makefile"
      "lispbm.mk"
    ];
  };
  
  buildPhase = ''
    cd repl/
    
    make ${makeTarget}
  '';
  installPhase = ''
    mkdir -p $out/bin
    
    cp repl $out/bin/${name}
  '';
  
  buildInputs = [
    readline
    libpng
  ];
  
  nativeBuildInputs = [
    gcc_multi
  ];
}