# Execute `nix-shell` (picks up shell.nix by default) to get an augmented environment
# to avoid polluting the global install
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    shellcheck sqlitebrowser
  ];
}
