{pkgs,header}: pkgs.writers.writeBashBin "imdb" ''

# dump IMDB attributes suitable for using in obsidian

source ${header}

Cmd[curl]=${pkgs.curl}/bin/curl
Cmd[jq]=${pkgs.jq}/bin/jq

### name file with The,A(n) at the end
### XXX add year; include in name
### XXX add rating?
### when creating links, don't forget ':' -> '-'
### when creating links, check for newline beforehand
### sort excluding The,A
### check for dangling links?
### rewrite all entries
### rename films to include year

usage() { echo "usage: $0 <tt...>" >&2; exit 2; }

main() {
tts=()
people=()
seen=()

[[ -d movies ]] || { echo "run this in an obsidian movies-info dir" >&2; exit 2; }

for arg in "$@"; do
  if [[ $arg =~ ^tt[0-9]{7,8}$ ]]; then
    tts+=($arg)
  elif [[ $arg =~ ^https?://www.imdb.com/title/(tt[0-9]{7,8})(/|$) ]]; then
    tts+=( "''${BASH_REMATCH[1]}" )
  elif [[ $arg =~ ^([+-])(Abi|Xander|JJ|Mum)$ ]]; then
    p="''${BASH_REMATCH[2]}"
    if [[ -e people/$p ]]; then
      if [[ ''${BASH_REMATCH[1]} == '+' ]]; then
        people+=( $p )
      else
        seen+=( $p )
      fi
    else
      echo "no such person '$p'"
      usage
    fi
  else
    echo "bad arg '$arg'" >&2
    usage
  fi
done

if [[ ''${#tts[*]} -eq 0 ]]; then
  echo "no titles '$*'" >&2
  usage
fi

for tt in ''${tts[@]}; do
  capture json gocmd 10 curl --silent --show-error -X GET "https://api.imdbapi.dev/titles/$tt" -H 'accept: application/json'
  title="$(gocmd 11 jq -r '.primaryTitle' <<<"$json")"; check_ jq
  t_="''${title//:/-}"; t_="''${t_////-}"
  target="movies/$t_.md"

  [[ -e $target ]] && { echo "already exists: $target ($tt)ph" ; exit 12; }

  echo "found title: $title"
  plot="$(gocmd 13 jq -r '.plot' <<<"$json")"; check_ jq
  year="$(gocmd 14 jq -r '.startYear' <<<"$json")"; check_ jq
  runtimeSeconds="$(gocmd 15 jq -r '.runtimeSeconds' <<<"$json")"; check_ jq
  # readarray -t genres < <(jq -r '.genres.[]' <<<"$json")
  # for g in "''${genres[@]}"; do echo "g: $g"; done
  capture_array interests gocmd 16 jq -r '.interests|.[]|.name'      <<<"$json"
  capture_array stars     gocmd 17 jq -r '.stars[]|.displayName'     <<<"$json"
  capture_array directors gocmd 18 jq -r '.directors[]|.displayName' <<<"$json"

  capture certificate="$(curl --silent --show-error -X GET "https://api.imdbapi.dev/titles/$tt/certificates" -H 'accept: application/json' | jq -r '.certificates[] | select(.country.code == "GB") | .rating' | head -n 1)"

  readarray -t images < <(curl --silent -X GET "https://api.imdbapi.dev/titles/$tt/images" | jq -r '.images[] | select (.type=="poster") | .url')


  mkdir -p _attachments
  if [[ ''${#images[*]} -eq 0 ]]; then
    echo "no images found"
  else
    img_target=movies/_attachments/"$t_".jpg
    echo "writing $img_target..."
    curl -s "''${images[0]}" | magick - -resize 600x400\> "$img_target"
  fi

  write_property() { echo "$1: $2" >> "$target"; }
  write_properties() {
    echo "$1:" >> "$target";
    local x;
    for x in "''${@:2}"; do
      echo "  - $x" >> "$target";
    done
  }

  echo "writing $target..."
  echo --- >> "$target"
  write_property imdb  "$tt"
  # quotes needed to protect, e.g., colons in the title
  write_property title "\"$title\""
  write_property cover "\"[[$t_.jpg]]\""
  write_property "UK Certificate" "$certificate"
  # quotes needed to protect, e.g., colons in the summary
  write_property summary "\"$plot\""
  write_property year "$year"

  write_property "duration" "$((runtimeSeconds/3600))h$(((runtimeSeconds%3600)/60))m"

  write_properties interests "''${interests[@]}"
  write_properties stars "''${stars[@]}"
  write_properties directors "''${directors[@]}"
  echo --- >> "$target"

  case "$p" in
    Abi ) pp=ax ;;
    Xander ) pp=xax ;;
    JJ     ) pp=jj  ;;
    Mum    ) pp=hx  ;;
    *      ) echo "no pp for '$p'" >&2; usage ;;
  esac

  for p in ''${people[@]}; do
    echo "[[$title]]" >> people/$p/$pp-wants-to-see.md
  done
  for p in ''${seen[@]}; do
    echo "[[$title]]" >> people/$p/has-seen.md
  done
done
  }

main "$@"

# -- that's all, folks! --------------------------------------------------------
''

# Local Variables:
# mode: sh
# fill-column: 80
# End:
