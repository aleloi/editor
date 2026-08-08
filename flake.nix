{
  inputs = {
    nixpkgs.url  = "github:NixOS/nixpkgs";
  };

  outputs = {self, nixpkgs, ... }:
  let
    systems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
    nixpkgsFor = system: import nixpkgs { inherit system; };
  in
    {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor system;
        in
          {
            default = pkgs.mkShell {
              nativeBuildInputs = [
                pkgs.zig_0_16
                pkgs.zls
                pkgs.tracy
                pkgs.seer
                pkgs.tmux
              ];
            };
          }
      );
    };
}
