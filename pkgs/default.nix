{ self,
  inputs,
  supportedSystems
}:

final: prev: {
  lbm = prev.callPackage ./lbm { inherit self supportedSystems; inherit (inputs) nix-filter; };
}