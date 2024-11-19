{
  description = "A flake for the parsing-lib Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc966;
        
        commitHook = pkgs.writeShellScript "commit-msg" (builtins.readFile ./scripts/hook-commit.sh);

        setupGitHooks = pkgs.writeShellScript "setup-git-hooks" ''
          ln -sf ${commitHook} .git/hooks/commit-msg
          chmod +x .git/hooks/commit-msg
        '';
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.hpack
            haskellPackages.stack
            haskellPackages.haskell-language-server
          ];
          shellHook = ''
            ${setupGitHooks}
          '';
        };

        packages.default = haskellPackages.callCabal2nix "glados" ./. {};
      });
}