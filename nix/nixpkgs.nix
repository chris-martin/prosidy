#
# Repo:    https://github.com/NixOS/nixpkgs-channels
# Branch:  nixos-unstable
# Updated: 2019-01-25
#

args:

let
  static     = builtins.getEnv "NIX_STATIC" != "";
  url        = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
  commit     = "e59dcf8ccec439f54dc52ffd5832610fc3f6c9c2";
  sha256     = "1ky79vczvf2sv6gk234i2m4vsxz1wpkh69gm8c7s1qwyawar9vjq";
  nixpkgs    = import tarball args;
  tarball    = builtins.fetchTarball { inherit url sha256; };
in
  if static then nixpkgs.pkgsMusl else nixpkgs
