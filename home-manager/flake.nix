{
  description = "Home Manager configuration of whitehead";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
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
        overlays = [
          hyprpanel.overlay
        ];
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
