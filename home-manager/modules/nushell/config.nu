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

$env.SSH_AUTH_SOCK = '/run/user/1000/yubikey-agent/yubikey-agent.sock'

$env.config.edit_mode = 'vi'
$env.EDITOR = 'vim'

$env.shell = "nu"

$env.PATH = ($env.PATH |
  split row (char esep) |
  append /usr/bin/env
)
