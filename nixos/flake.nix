{
  description = "My NixOS flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      ...
    }@inputs:
    {
      nixosConfigurations = {
        mises = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          modules = [
            {
              nixpkgs.overlays = [
                (final: prev: {
                  unstable = import nixpkgs-unstable {
                    inherit system;
                    config.allowUnfree = true;
                  };
                })
              ];
            }
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
        router = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./router.nix
          ];
        };
        bigtux = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./bigtux.nix
          ];
        };
      };
    };
}
