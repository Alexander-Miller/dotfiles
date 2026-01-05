function passls
  set -l PASSWORD_STORE_DIR ~/.password-store

  set -l oldpwd (pwd)
  if not cd "$PASSWORD_STORE_DIR"
    return 1
  end

  find -L . -name '*.gpg' -print0 \
    | string split0 \
    | string replace -r '^\./' '' \
    | string replace -r '\.gpg$' '' \
    | sort \
    | string match -rv '^$'

  cd "$oldpwd"
  return 0
end

function rofi_pass
  set -l PW (passls | rofi -no-auto-select -i -dmenu)

  if test -n "$PW"
    pass show -- "$PW" | head -n 1 | tr -d '\n' | wtype -
  end
end
