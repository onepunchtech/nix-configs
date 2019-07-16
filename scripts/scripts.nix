{pkgs, ...}:

{
  pavol = pkgs.writeShellScriptBin "pavol.sh" ./pavol.sh;
  emc = pkgs.writeShellScriptBin "emc" ./emc;
}