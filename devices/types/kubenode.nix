{ config, pkgs, ... }:

{
  imports = [./hardware-configuration.nix];

  nix = {
    gc = {
      automatic = true;
      dates = "monthly";
    };
  };

  system.stateVersion = "20.09";
  time.timeZone = "America/Denver";
  environment.systemPackages = with pkgs; [
    wget
    vim
  ];

  fonts.fonts = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    font-awesome_5
    material-icons
  ];

  services = {
    k3s = {
      enable = true;
    };
    openssh.enable = true;
    logind.extraConfig = "HandleLidSwitch=ignore";
  };

  console.font = "latarcyrheb-sun32";

  users.whitehead.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC5vw9x9O3Nz/hayQwjV4w/gwfT7V0pnN5fxNpZuPZGtcFbSZjwNhEQ66RrGlOc9EZZZf6pLckffGBWUNvV24ocrNor0IBvQZhoR/7d/WE+vCxQtQwJW/ZZagiE7noqpRyk5Ty7DwNDi05s8IMvCUQ6i+t+iVLo7TEZelvF3+1NB5Azx+MIbdtr7kmYlZ8SQwJsSIYYHTVgC8+bEQ5bkRjtjsukHoqiuNzA3z3mBqw7jFCq2PbgYbllzDlJSYkfvr2yxCRqeedab1Q/q4kSD1wq9ysqcPEUYJhDVAVnr1Rp/ndVM+N1tMw4R/ilqsghwNbPNIr7yEY7Y37WphOanqM/ whitehead@ludwig"
  ];
}
