# Builds res_fw.qrc for the specified firmware versions.
{
  src,
  # List of bldc board names to be built, i.e. a list of bldc "fw_*" Makefile
  # targets without the "fw_" prefix.
  # Can also be the string "all", which builds all standard board firmwares.
  fwBoards ? [ ],

  gcc-arm-embedded-14,
  git,
  python3,
  stdenv,
}:

let
  fwTargets =
    if fwBoards == "all" then
      [ "all_fw" ]
    # VESC Tool doesn't build if the provided res_fw.qrc file is empty for some
    # reason. Therefore always include "general purpose" firmware.
    else
      builtins.map (board: "fw_${board}") (fwBoards ++ [ "gp" ]);
in
stdenv.mkDerivation rec {
  pname = "bldc-fw";
  version = src.shortRev or src.dirtyShortRev or "unknown";

  BLDC_FW_SHORT_SHA = src.shortRev or "unknown";

  inherit src;

  dontPatch = true;
  dontFixup = true;

  buildPhase = ''
    ${
      if builtins.length fwTargets != 0 then
        "make -j $NIX_BUILD_CORES ${builtins.concatStringsSep " " fwTargets}"
      else
        ""
    }

    python package_firmware.py
  '';
  installPhase = ''
    mkdir -p $out

    cp -r ./package/* $out/
  '';

  nativeBuildInputs = [
    gcc-arm-embedded-14
    python3
    git
  ];
}
