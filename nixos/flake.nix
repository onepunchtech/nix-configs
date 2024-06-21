{
  description = "My NixOS flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs, ... }@inputs: {
    nixosConfigurations = {
      mises = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./mises.nix
        ];
      };
      bob = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./bob.nix
        ];
      };
      tom = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./tom.nix
        ];
      };
      sowell = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./sowell.nix
        ];
      };
    };
  };
}
