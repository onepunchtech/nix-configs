{
  description = "Home Manager configuration of whitehead";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = (import nixpkgs) {
        inherit system;
      };
    in
    {
      homeConfigurations."whitehead" = home-manager.lib.homeManagerConfiguration {
        pkgs = pkgs;

        modules = [
          ./home.nix
        ];
      };
    };
}
