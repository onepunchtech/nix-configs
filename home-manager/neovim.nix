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

  plugins =
    with pkgs.vimPlugins; [
      nvim-lspconfig
      plenary-nvim
      nvim-treesitter.withAllGrammars
      catppuccin-nvim
      oil-nvim
      lualine-nvim
      telescope-nvim
      which-key-nvim
      alpha-nvim
      dressing-nvim
      nvim-ts-autotag
      indent-blankline-nvim
      luasnip
      nvim-cmp
      cmp_luasnip
      cmp-buffer
      cmp-path
      cmp-nvim-lsp
      friendly-snippets
      lspkind-nvim
      nvim-autopairs
      nvim-ts-context-commentstring
      comment-nvim
      todo-comments-nvim
      substitute-nvim
      nvim-surround
      ];
}
