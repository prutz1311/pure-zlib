{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs@{ self, nixpkgs, flake-utils }:
  flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
    let
      pkgs = import nixpkgs { inherit system; };
      pz = pkgs.haskellPackages.callCabal2nix "pure-zlib" ./. {};
    in
    {
      packages.pure-zlib = pz;
      packages.default = pz;
      devShells.pure-zlib = pkgs.mkShell {
        buildInputs = [ pkgs.zlib ];  # The C libary for benchmarks
        nativeBuildInputs = with pkgs.haskellPackages; [ cabal-install ];
      };
    }
  );
}
