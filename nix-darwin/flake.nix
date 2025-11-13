{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
    }:
    let
      configuration =
        { pkgs, ... }:
        {
          imports = [
            ../modules/base.nix
          ];
          # List packages installed in system profile. To search by name, run:
          # $ nix-env -qaP | grep wget
          environment.systemPackages = with pkgs; [
            neovim
            home-manager
            ghostty-bin
          ];

          # Necessary for using flakes on this system.
          nix.settings.experimental-features = "nix-command flakes";

          # Enable alternative shell support in nix-darwin.
          # programs.fish.enable = true;

          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null;

          # Used for backwards compatibility, please read the changelog before changing.
          # $ darwin-rebuild changelog
          system.stateVersion = 6;

          # The platform the configuration will be used on.
          nixpkgs.hostPlatform = "aarch64-darwin";

          system.primaryUser = "whitehead";
          system.defaults.dock = {
            static-only = true;
            autohide = true;
          };

          services.yabai = {
            enable = true;

            config = {
              focus_follows_mouse = "autoraise";
              mouse_follows_focus = "off";
              window_placement = "second_child";
              window_opacity = "off";
              top_padding = 0;
              bottom_padding = 5;
              left_padding = 5;
              right_padding = 5;
              window_gap = 5;
              layout = "bsp";
            };
          };

          services.skhd = {
            enable = true;
            skhdConfig = ''
              alt - h : yabai -m window --focus west
              alt - l : yabai -m window --focus east
              alt - k : yabai -m window --focus north
              alt - j : yabai -m window --focus south
              shift + alt - h : yabai -m window --swap west
              shift + alt - l : yabai -m window --swap east
              shift + alt - k : yabai -m window --swap north
              shift + alt - j : yabai -m window --swap south
              alt - b : open -a "Brave Browser" 
              alt - c : $(yabai -m window $(yabai -m query --windows --window | jq -re ".id") --close)
              alt - return : open -n /Applications/Nix\ Apps/Ghostty.app
            '';
          };

        };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#simple
      darwinConfigurations."simple" = nix-darwin.lib.darwinSystem {
        modules = [ configuration ];
      };
    };
}
