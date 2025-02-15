# NIXOS Anywhere

nix run github:nix-community/nixos-anywhere -- --flake .#beara -- --generate-hardware-config nixos-facter ./hardware/facter/beara.json --target-host rot@10.10.10.73
