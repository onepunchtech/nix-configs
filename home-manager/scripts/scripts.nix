{pkgs, ...}:

let opdtSrc = fetchGit {
      url = "https://github.com/onepunchlinux/opdt.git";
      ref = "master";
      rev = "175364b33926ca708cf35bd2a9010f8c573f5647";
    };

in {
  emc = pkgs.writeShellScriptBin "emc" ''
    tty -s;
    if [ "0" == "$?" ]; then
        emacsclient -a "" -t "$@"
    else
        emacsclient -a "" -n -c "$@"
    fi
  '';
  opdt = (import opdtSrc {}).opdt;
}
