{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    swipl-nix.url = "github:matko/swipl-nix";
  };

  outputs = { self, nixpkgs, utils, swipl-nix }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ swipl-nix.overlays.default ];
      };
    in
    {
      devShell = pkgs.mkShell {
        packages = [
          pkgs.swipl-nix."latest"
        ];

        env = {
          SWI_HOME_DIR = "${pkgs.swipl-nix."latest"}/share/swi-prolog";
        };
      };
    }
  );
}
