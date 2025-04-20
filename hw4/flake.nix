{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    swipl-nix.url = "github:matko/swipl-nix";
  };

  outputs = { self, nixpkgs, swipl-nix, ... }:
    let
      system = "x86_64-linux";  # Change for other systems
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ swipl-nix.overlays.default ];
      };
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          pkgs.swipl-nix."latest"
          pkgs.pcre
          
          pkgs.git
          pkgs.gnumake
        ];

        SWI_HOME_DIR = "${pkgs.swipl-nix."latest"}/share/swi-prolog";
      };
    };
}
