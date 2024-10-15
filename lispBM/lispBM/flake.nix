{
  description = "Flake to build LispBM's repl and documentation";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, nix-filter, ... }: let
    supportedSystems = with flake-utils.lib.system; [
      x86_64-linux
      x86_64-darwin
    ];
  in {
    overlays.default = import ./pkgs { inherit self inputs supportedSystems; };
    
    # For debugging
    inherit inputs;
  } // flake-utils.lib.eachSystem supportedSystems (system: let
    pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
  in {
    packages = with pkgs.lbm; {
      inherit repl;
      inherit repl64;
      inherit doc;
      inherit c-doc;
      default = repl;
    };    
  });
}
