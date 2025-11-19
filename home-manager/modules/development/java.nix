{ pkgs, ... }:

{
  home.packages = with pkgs; [ spring-boot-cli ];
}
