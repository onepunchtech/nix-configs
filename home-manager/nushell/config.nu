$env.config = {
  show_banner: false,
}

$env.PATH = ($env.PATH |
  split row (char esep) |
  prepend /home/myuser/.apps |
  append /usr/bin/env
)