{
  inputs = {
    nixpkgs.url  = "github:NixOS/nixpkgs";
    zig-overlay = {
      url = "github:mitchellh/zig-overlay";
    };
  };

  outputs = {self, zig-overlay, nixpkgs, ... }:
  let
    pkgs = import nixpkgs {
      overlays = [zig-overlay.overlays.default ];
      system = "x86_64-linux";
    };
  in
    {
      devShell.x86_64-linux = pkgs.mkShell {
        nativeBuildInputs = [
          zig-overlay.packages."x86_64-linux"."0.13.0"
          pkgs.tracy
          pkgs.seer
          # pkgs
        ];
      };
    };
}
