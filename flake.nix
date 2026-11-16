{
  description = "Packages VESC firmware into a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        bldc-fw = pkgs.callPackage ./pkgs/bldc.nix {
          src = self;
        };
      in
      {
        packages = {
          inherit bldc-fw;
          default = bldc-fw;
        };
      }
    )
    // {
      overlays.default = final: _prev: {
        bldc-fw = final.callPackage ./pkgs/bldc.nix {
          src = self;
        };
      };
    };
}
