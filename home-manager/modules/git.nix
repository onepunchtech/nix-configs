{ ... }:
{
  programs.git = {
    enable = true;
    userEmail = "onepunchlinux@gmail.com";
    userName = "Michael Whitehead";
    extraConfig = {
      core = {
        autocrlf = false;
        safecrlf = false;
        eol = "crlf";
      };

      url = {
        "git@github.com:" = {
          insteadOf = "https://github.com/";
        };

        "https://github.com/rust-lang/crates.io-index" = {
          insteadOf = "https://github.com/rust-lang/crates.io-index";
        };

        "https://github.com/RustSec/advisory-db" = {
          insteadOf = "https://github.com/RustSec/advisory-db";
        };
      };
    };
  };
}
