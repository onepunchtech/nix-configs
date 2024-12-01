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
  extraLuaPackages = luaPkgs: with luaPkgs; [ luautf8 ];
  extraPackages = with pkgs; [
    lua-language-server
    nixd
    terraform-lsp
    typescript-language-server
    yaml-language-server
    nodePackages.bash-language-server
    dockerfile-language-server-nodejs
    docker-compose-language-service
    dhall-lsp-server
    helm-ls
    marksman
    rustfmt
    rust-analyzer
    nodePackages.prettier
    stylua
    nixfmt-rfc-style
  ];
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
      neodev-nvim
      nvim-lspconfig
      nvim-lsp-file-operations
      trouble-nvim
      mini-icons
      conform-nvim
      gitsigns-nvim
      lazygit-nvim
      orgmode
      direnv-vim
    ];
}
