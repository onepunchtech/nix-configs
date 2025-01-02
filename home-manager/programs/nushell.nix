{

  enable = true;
  configFile.source = ../rawConfigs/nushell/config.nu;
  envFile.text = ''
    $env.CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense'
    #$env.ZELLIJ_AUTO_ATTACH = true
  '';
  shellAliases = {
    lg = "lazygit";
    z = "zellij -l welcome";
  };
}
