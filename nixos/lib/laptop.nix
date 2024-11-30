{pkgs, ...}:

{
  services = {
    logind = {
      lidSwitch = "ignore";
      lidSwitchDocked = "ignore";
    };
  };

  systemd.sleep.extraConfig = ''
    AllowSuspend=yes
    AllowHibernation=yes
    AllowHybridSleep=yes
    AllowSuspendThenHibernate=no
  '';

  services.thermald.enable = true;

  services.tlp = {
    enable = true;
  };
}
