{
  description = "Home Manager configuration of whitehead";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";
    hyprpanel.inputs.nixpkgs.follows = "nixpkgs";
    vale-nix.url = "github:icewind1991/vale-nix";
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      hyprpanel,
      vale-nix,
      ...
    }:
    let
      system = "x86_64-linux";
      overlays = [
        vale-nix.overlays.default
      ];
      pkgs = (import nixpkgs) {
        inherit system overlays;
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
