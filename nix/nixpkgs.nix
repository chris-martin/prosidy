#
# Repo:    https://github.com/NixOS/nixpkgs-channels
# Branch:  nixos-unstable
# Updated: 2019-01-25
#

args:

let
  static        = builtins.getEnv "NIX_STATIC" != "";
  dynUrl        = "https://github.com/NixOS/nixpkgs/archive/${dynCommit}.tar.gz";
  dynCommit     = "e59dcf8ccec439f54dc52ffd5832610fc3f6c9c2";
  dynSha256     = "1ky79vczvf2sv6gk234i2m4vsxz1wpkh69gm8c7s1qwyawar9vjq";

  staticUrl     = "https://github.com/nh2/nixpkgs/archive/${staticCommit}.tar.gz";
  staticCommit  = "0c960262d159d3a884dadc3d4e4b131557dad116";
  staticSha256  = "0d7ms4dxbxvd6f8zrgymr6njvka54fppph1mrjjlcan7y0dhi5rb";

  nixpkgs       = import tarball args;
  tarball       = 
    if static
    then builtins.fetchTarball { url = staticUrl; sha256 = staticSha256; }
    else builtins.fetchTarball { url = dynUrl; sha256 = dynSha256; };
in
  if static then nixpkgs.pkgsMusl else nixpkgs
