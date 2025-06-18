{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
        };

        erlpkgs = pkgs.beam.packages.erlang_26;
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with erlpkgs; [
            pkgs.act
            pkgs.just
            rebar3
            erlang
          ];

          shellHook = ''
            git config --local core.hooksPath .github/hooks
          '';
        };
      }
    );
}


