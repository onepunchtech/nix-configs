{
  cas = {
    ssh_host = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILmGUxp14yV9j/eKpMWD8aCENGcua7IN+AuhRhE98xy4 whitehead@mises";
    ssh_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICHOOP452cKpOjR2UyLQA/j9fRI898yHENqd5PCY+Ed1 whitehead@mises";
  };

  hosts = {
    k8s_control1 = {
      age = {
        publicKey = "age19nrq00th63twalj729y3454dvn8kwwf0ktphntcuw4q8q43e8a0q4a9wwa";
      };

      ssh_pub = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDCXSHODq09sXl9zl+kybL5a6XesnOqo9Pu0ClKt7/Ja whitehead@mises";
      ssh_cert = "ssh-ed25519-cert-v01@openssh.com AAAAIHNzaC1lZDI1NTE5LWNlcnQtdjAxQG9wZW5zc2guY29tAAAAIF7DjMBxJ5S8frwQ2Qqrmnuo0Hz99xSibhFZjDKu9yI2AAAAIDCXSHODq09sXl9zl+kybL5a6XesnOqo9Pu0ClKt7/JaAAAAAAAAAAAAAAACAAAADW9uZXB1bmNoIENBdjEAAAAPAAAAC3JhY2sxazhzY3AxAAAAAGfYTkQAAAAAabmCAwAAAAAAAAAAAAAAAAAAADMAAAALc3NoLWVkMjU1MTkAAAAguYZTGnXjJX2P94qkxYPxoIQ0Zy5rsg34C6FGET3zHLgAAABTAAAAC3NzaC1lZDI1NTE5AAAAQIAofinl++l222flS9AGio4p6tEFxdMCgh0+aHyrPPlsQNzMUvCp09lxZHr4jMRLYzaGxXtz9Xr0oTjcKRRZUg0= whitehead@mises";
    };
  };

  user = {

  };

  authorizedKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCrOpJm3+B7/pyGi+pzn2HbatpFY7tCDpwBcr8orQOd9B0GXTIuTKeV2lGS9Zb1TUqngo9uR2JXv0o51IZOao0zjGgug2udFvB0mQNALCrEosHVzTGopkeuiF9ZKlaHO5vbzi9zfDWs9/1A1YTa7JFt8Qrgi4EqycOli540jlvvxkEDN3PDz/36YaXCqzqj3e5tX6Nmh8xCEq70+oyA9oZ/gTMLFjLLlSigZPn0Ex3KjRpiap3LkPZGt7LPZEFWXrMMKLhzOhM7yuMewHSiYMp4s6gJursUK3etcxSHn+HeXcMdtte9XRi91PwIhnHR/oqUpP+wNpwm26qRmVeOmn2J YubiKey #26922176 PIV Slot 9a"
  ];
}
