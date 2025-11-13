{ config, pkgs, ... }:

{
  imports = [
    ./neovim
    ./ghostty.nix
    ./git.nix
    ./nushell
    ./zellij.nix
  ];

  home.packages = with pkgs; [
    tflint
    taplo
    k9s
    kubectl
    cilium-cli
    kubetail
    screen
    lazygit
    lazysql
    vscode-langservers-extracted
    yaml-language-server
    marksman
    ltex-ls
    htop
    glance
    docker-compose
    age
    sops
    nixpkgs-fmt
    nixd
    zip
    dhall
    dhall-json
    semgrep
    silver-searcher
    unzip
    jdt-language-server
  ];

  programs = {
    ssh.forwardAgent = true;
    zsh = {
      enable = true;
      autosuggestion.enable = true;

    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    carapace = {
      enable = true;
      enableZshIntegration = true;
    };

    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        add_newline = true;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
      };
    };
  };
}
