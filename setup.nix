# Execute `nix-shell setup.nix` to get an augmented environment
# to avoid polluting the global install
{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [ pkgs.docker pkgs.shellcheck ];
}
