{ system ? builtins.currentSystem, compiler ? null }:
let
  pkgs = import ./nix { inherit system compiler; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.nano-parsec.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.nano-parsec.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
