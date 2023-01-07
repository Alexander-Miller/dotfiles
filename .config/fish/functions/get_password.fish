function get_password -a name
  if test -e $name
    return 1
  end

  set -l line (gpg2 --quiet --no-tty --batch --decrypt ~/.authinfo.gpg | rg $name)
  echo (string split "password " "$line")[-1]
end
