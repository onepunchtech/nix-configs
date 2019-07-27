{
  enable = true;
  userEmail = "onepunchlinux@gmail.com";
  userName = "Michael Whitehead";
  extraConfig = ''
    [core]
      autocrlf = false
      safecrlf = false
      eol = crlf

    [url "ssh://git@github.com"]
        insteadOf = https://github.com
  '';
}
