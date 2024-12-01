$env.config = {
  show_banner: false,
  hooks: {
    env_change: {
      PWD: [
        { ||
          if (which direnv | is-empty) {
            return
          }

          direnv export json | from json | default {} | load-env
        }

      ]
    }
  }
}

$env.PATH = ($env.PATH |
  split row (char esep) |
  prepend /home/myuser/.apps |
  append /usr/bin/env
)
