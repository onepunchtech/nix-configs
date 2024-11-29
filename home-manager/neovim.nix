{ pkgs, lib }:
let
  fromGitHub = ref: repo: pkgs.vimUtils.buildVimPlugin {
    pname = "${lib.strings.sanitizeDerivationName repo}";
    version = ref;
    src = builtins.fetchGit {
      url = "https://github.com/${repo}.git";
      ref = ref;
    };
  };

in
{
  enable = true;
  defaultEditor = true;
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;
  extraConfig = ''
  '';

  plugins =
    with pkgs.vimPlugins; [
      nvim-lspconfig
      plenary-nvim
      gruvbox-material
      mini-nvim
      nvim-treesitter.withAllGrammars
      catppuccin-nvim
      #(fromGitHub "HEAD" "elihunter173/dirbuf.nvim")
    ];
}
