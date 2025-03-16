{
  description = "Home Manager configuration of whitehead";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=8072da51328d54e5d5469bd0efd85a59cd288db3";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";
    hyprpanel.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      hyprpanel,
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
        extraSpecialArgs = {
          extra = { inherit hyprpanel; };
        };
      };
    };
}
