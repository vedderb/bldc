{ supportedSystems

, self
, nix-filter

, stdenv
, lbm
, graphviz
}:

stdenv.mkDerivation rec {
  pname = "lbm-doc";
  version = self.shortRev or self.rev or self.dirtyShortRev or "unknown";
  
  meta = {
    description = "LispBM's Markdown documentation";
    platforms = supportedSystems;
  };
  
  src = nix-filter {
    root = self;
    include = with nix-filter.lib; [
      "doc"
    ];
    exclude = with nix-filter.lib; [
      "doc/c_doc"
      (matchExt "dot")
      (matchExt "md")
    ];
  };
  
  s = builtins.break true;
  
  buildPhase = ''
    cd doc
    
    make all
  '';
  installPhase = ''
    mkdir -p $out $out/images
    
    cp lbmref.md $out
    cp displayref.md $out
    cp runtimeref.md $out
    cp images/*.png $out/images
  '';
  
  nativeBuildInputs = [
    lbm.repl
    graphviz
  ];
}