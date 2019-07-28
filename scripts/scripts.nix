{pkgs, ...}:

{
  emc = pkgs.writeShellScriptBin "emc" ''
    tty -s;
    if [ "0" == "$?" ]; then
        emacsclient -a "" -t "$@"
    else
        emacsclient -a "" -n -c "$@"
    fi
  '';
  opdt = pkgs.haskellPackages.callPackage ./opdt.nix { };
}
