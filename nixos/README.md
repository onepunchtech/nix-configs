# NIXOS Anywhere

nix run github:nix-community/nixos-anywhere -- --flake .#beara -- --generate-hardware-config nixos-facter ./hardware/facter/beara.json --target-host rot@10.10.10.73

# Secrets

- bootstrap
  - ssh key -> /etc/ssh/ssh_host_ed25519_key
  - age key -> /var/lib/sops-nix/key.txt
