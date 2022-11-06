function get_password -a name
  if test -e $name
    return 1
  end

  gpg2 --quiet --no-tty --batch --decrypt ~/.authinfo.gpg \
    | rg $name \
    | awk -F ' ' '{print $NF}'
end
