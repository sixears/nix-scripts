{ pkgs ? import <nixpkgs> {}, header, minfo }: ''

# vorbiscomment -l $inputfile > $commentfile
# vorbiscomment -c $commentfile -w $outputfile

# coverart=$(base64 -w 0 $jpegfile)
# vorbiscomment -a -t 'COVERART=$coverart' -t 'COVERARTMIME=image/jpeg' $infile $outfile
# ...or...
# vorbiscomment -w -t 'COVERART=$coverart' -t 'COVERARTMIME=image/jpeg' $infile

#vorbiscomment -w -t "COVERART=$(base64 -w 0 artwork.jpg)" -t 'COVERARTMIME=image/jpeg' OGGFILE

# -u: Treat unset variables and parameters other than the special parameters "@"
#     and "*" as an error when performing parameter expansion.  If expansion is
#     attempted on an unset variable or parameter, the shell prints an error
#     message, and, if not interactive, exits with a non-zero status.

# -o pipefail: If set, the return value of a pipeline is the value of the last
#              (rightmost) command to exit with a non-zero status, or zero if
#              all commands in the pipeline exit successfully.  This option is
#              disabled by default.

source ${header}

# dotglob: If set, bash includes filenames beginning with a . in the results of
#          pathname expansion.
shopt -s dotglob
# extglob: Enable ?(pattern) *(pattern) +(pattern) @(pattern) !(pattern)
shopt -s extglob

Cmd[eyeD3]=${pkgs.python3Packages.eyeD3}/bin/eyeD3
Cmd[iconv]=${pkgs.glibc.bin}/bin/iconv
Cmd[id3v2]=${pkgs.id3v2}/bin/id3v2
Cmd[jq]=${pkgs.jq}/bin/jq
Cmd[metaflac]=${pkgs.flac}/bin/metaflac
Cmd[minfo]=${minfo}/bin/minfo
Cmd[tr]=${pkgs.coreutils}/bin/tr
Cmd[yaml2json]=${pkgs.yaml2json}/bin/yaml2json

# ------------------------------------------------------------------------------

usage () {
  usage="$(''${Cmd[cat]} <<EOF
usage: $Progname OPTION* MUSIC-FILE

tag (including artwork) an mp3 or flac file

options:
 -a | --artwork <FILE>  set mp3 artwork to this file
                        default: artwork.jpg in file dir

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --show-cmds     Show individual cmds (external processes).
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
EOF
)"
  dieusage "$usage"
}

# ------------------------------------------------------------------------------

main() {
  local input="$1"

  if [[ "$input" =~ ^((.*)/)?0*([[:digit:]]+)([\ -.]+)(([^/]*)'.')?(mp3|MP3|flac|FLAC|ogg|OGG)$ ]]; then
    local indir="''${BASH_REMATCH[2]:-.}"
    local innum="''${BASH_REMATCH[3]}"
    local instub="''${BASH_REMATCH[6]}"
    local inext="''${BASH_REMATCH[7]}"

    debug "indir : '$indir'"
    debug "innum : '$innum'"
    debug "instub: '$instub'"
    debug "inext : '$inext'"

    local jpg
    if [ -n "''${artwork}" ]; then
      if [ -e "''${artwork}" ]; then
        jpg="''${artwork}"
      else
        die 10 "file not found: ''${artwork}"
      fi
    elif [ -e "$indir/artwork-$(printf '%02d' "$innum")-.jpg" ]; then
      jpg="$indir/artwork-$(printf '%02d' "$innum")-.jpg"
    else
      jpg=""
      local a
      for a in "$indir"/artwork-*(+([0-9])?(_+([0-9]))-)@(+([0-9])?(_+([0-9])))-.jpg; do
        local b
        b="$(gocmd 15 basename "$a" -.jpg)"; check_ basename
        IFS='-' read -ra bits <<<"''${b:8}"
        local t
        for t in "''${bits[@]}"; do
          if [ "$t" == "$(printf '%02d' "$innum")" ]; then
            jpg="$a"; break
          elif [[ "$t" =~ ^0*([[:digit:]]+)_0*([[:digit:]]+)$ ]]; then
            if    [ $innum -ge ''${BASH_REMATCH[1]} ] \
               && [ $innum -le ''${BASH_REMATCH[2]} ]; then
              jpg="$a"; break
            fi
          fi
        done
      done
      if [ -z "$jpg" ]; then
        if [ -e "$indir/artwork.jpg" ]; then
          jpg="$indir/artwork.jpg"
        elif [[ "$indir" =~ ^(.*)/Disc\ [0-9]+($|[ -:]) ]]; then
          jpg="''${BASH_REMATCH[1]}/artwork.jpg"
        else
          jpg="$indir/artwork.jpg"
        fi
      fi
      if [ ! -e "$jpg" ]; then
        die 8 "artwork not found '$jpg'"
      fi
    fi

    local discnum=0
    local infy discnum
    local -a cmd
    if [ -e "$indir/info.yaml" ]; then
      infy="$indir/info.yaml"
      cmd=(gocmdnodryrun 12 minfo track-info "$infy" $innum)
    elif [[ "$indir" =~ ^(.*)/Disc\ ([0-9]+)($|[ -:]) ]]; then
      infy="''${BASH_REMATCH[1]}/info.yaml"
      discnum=''${BASH_REMATCH[2]}
      cmd=(gocmdnodryrun 13 minfo track-info "$infy" $discnum $innum)
    else
      infy="$indir/info.yaml"
      cmd=(gocmdnodryrun 14 minfo track-info "$infy" $innum)
    fi
    info2 "using info: $infy"

    [ -e "$infy" ] || die 9 "not found: '$infy'"
    info2 "using dist/track# $discnum/$innum"

    local tinfo="$("''${cmd[@]}")"; [ $? -eq 0 ] || die 3 "''${cmd[*]}"
    info3 "$tinfo"

    showvar() { info2 "$(printf '%-16s\t: %s' "$1" "$2")"; }

    setvar() {
      case $# in
        1) varnm="$1"; key=".$1 // empty" ;;
        2) varnm="$1"; key="$2" ;;
        *) die 250 "setvar used with $# arguments"
      esac
      # make $n an alias to $"$varnm"
      local -n n="$varnm"
      # use <<< rather than echo to avoid weirdness if, say, tinfo includes a
      # `\"` sequence
      n="$(gocmdnodryrun 15 jq --raw-output "$key" <<< "$tinfo")"
      if [ -n "''${!varnm}" ]; then
        showvar $varnm "''${!varnm}"
      fi
    }

    # title is the full title with version, live_info;
    # song_title is the pure name
    for i in artist album album_artist title song_title version \
             live_location live_version live_date live_type     \
             track_count discid disc_count track_in_disc        \
             tracks_on_disc track_total_id                      \
             album_title work disctitle; do
      setvar "$i"
    done
    setvar release ".original_release // .release // empty"

    ## XXX album_version
    ## XXX track_num should be in set (so ++ for Discs 2,3...)
    tracks="$(gocmdnodryrun 17 minfo track-count "$infy")"
    check_ "minfo track-count '$infy'"

    showvar total_position "$track_total_id/$track_count"
    showvar disc           "$discid/$disc_count"
    showvar discpos        "$track_in_disc/$tracks_on_disc"
    showvar artwork "$jpg"

    ic() {
      gonodryrun 18 echo "$(gonodryrun 19 echo "$1" | gocmd 20 iconv -f utf-8 -t ascii//TRANSLIT)"
    }

    case "$inext" in
        mp3  ) gocmd 3 id3v2 -D "$input"
               ##    http://id3.org/id3v2.4.0-frames

               ##   TSOA
               ##
               ##    The 'Album sort order' frame defines a string which should
               ##    be used instead of the album name (TALB) for sorting
               ##    purposes. E.g. an album named "A Soundtrack" might
               ##    preferably be sorted as "Soundtrack".

               ##   TSOP
               ##
               ##    The 'Performer sort order' frame defines a string which
               ##    should be used instead of the performer (TPE2) for sorting
               ##    purposes.

               ##   TSOT
               ##
               ##    The 'Title sort order' frame defines a string which should
               ##    be used instead of the title (TIT2) for sorting purposes.

               ##   TDOR
               ##
               ##    The 'Original release time' frame contains a timestamp
               ##    describing when the original recording of the audio was
               ##    released. Timestamp format is described in the ID3v2
               ##    structure document [ID3v2-strct].

               ##   TDRC
               ##
               ##    The 'Recording time' frame contains a timestamp describing
               ##    when the audio was recorded. Timestamp format is described
               ##    in the ID3v2 structure document [ID3v2-strct].

               ##   TDRL
               ##
               ##    The 'Release time' frame contains a timestamp describing
               ##    when the audio was first released. Timestamp format is
               ##    described in the ID3v2 structure document [ID3v2-strct].

               ##   TIT1
               ##
               ##    The 'Content group description' frame is used if the sound
               ##    belongs to a larger category of sounds/music. For example,
               ##    classical music is often sorted in different musical
               ##    sections (e.g. "Piano Concerto", "Weather - Hurricane").

               ##   TIT2
               ##
               ##    The 'Title/Songname/Content description' frame is the
               ##    actual name of the piece (e.g. "Adagio", "Hurricane
               ##    Donna").

               ##   TIT3
               ##
               ##    The 'Subtitle/Description refinement' frame is used for
               ##    information directly related to the contents title
               ##    (e.g. "Op. 16" or "Performed live at Wembley").

               local args=()

               declare -A tags
	       declare -a comments=()
               tags=([song]="$(ic "$title")"
                     [artist]="$(ic "$artist")"
                     [album]="$(ic "$album_title")"
                     [track]="$track_in_disc/$tracks_on_disc"
                     [year]="$release"
                     [TPE2]="$album_artist"
                     [TPOS]="$discid/$disc_count"
                     [TIT1]="$work"
                    )
               # http://id3.org/id3v2.4.0-frames
               # https://help.mp3tag.de/main_tags.html
               if [ -n "$version" ]; then
                 args+=(--comment "version:''${version//:/-}")
               fi
               if [ -n "$live_type" ]; then
                 args+=(--comment "live type:''${live_type//:/-}")
               fi
               if [ -n "$live_location" ]; then
                 comments+=("live location:''${live_location//:/-}")
               fi
               if [ -n "$live_date" ]; then
                 tags[TRDA]="$live_date"
                 comments+=("live_date:''${live_date//:/-}")
               fi
               if [ -n "$live_version" ]; then
                 comments+=("live version:''${live_version//:/-}")
               fi
               for i in "''${!tags[@]}"; do
                 infof "%-16s:  %s\n" "$i" "''${tags[$i]}"
                 args+=("--$i"="''${tags[$i]}")
               done
               for i in "''${comments[@]}"; do
                 infof "%-16s:  %s\n" comment "$i"
                 args+=("--comment"="$i")
               done
warn "ARGS: ''${args[@]}"
               gocmd 4 id3v2 --id3v2-only "''${args[@]}" "$input"

               # it is important that eyeD3 goes last, because id3v2
               # nukes the image.
               gocmd 5 eyeD3 --add-image "$jpg":FRONT_COVER "$input"
               ;;

        flac ) gocmd 6 metaflac --remove --block-type=VORBIS_COMMENT,PICTURE \
                              "$input"
               ## https://xiph.org/vorbis/doc/v-comment.html
               ##
               ## Field names
               ##
               ## Below is a proposed, minimal list of standard field names with
               ## a description of intended use. No single or group of field
               ## names is mandatory; a comment header may contain one, all or
               ## none of the names in this list.
               ##
               ## TITLE
               ##     Track/Work name
               ##
               ## VERSION
               ##     The version field may be used to differentiate multiple
               ##     versions of the same track title in a single
               ##     collection. (e.g. remix info)
               ##
               ## ALBUM
               ##     The collection name to which this track belongs
               ##
               ## TRACKNUMBER
               ##     The track number of this piece if part of a specific
               ##     larger collection or album
               ##
               ## ARTIST
               ##     The artist generally considered responsible for the
               ##     work. In popular music this is usually the performing band
               ##     or singer. For classical music it would be the
               ##     composer. For an audio book it would be the author of the
               ##     original text.
               ##
               ## PERFORMER
               ##     The artist(s) who performed the work. In classical music
               ##     this would be the conductor, orchestra, soloists. In an
               ##     audio book it would be the actor who did the reading. In
               ##     popular music this is typically the same as the ARTIST and
               ##     is omitted.
               ##
               ## DESCRIPTION
               ##     A short text description of the contents
               ##
               ## DATE
               ##     Date the track was recorded
               ##
               ## LOCATION
               ##     Location where track was recorded
               ##

               args=(--import-picture-from="$jpg")

               declare -A tags
               tags=([song-title]="$song_title"
                     [title]="$title"
                     [artist]="$artist"
                     [album-artist]="$album_artist"
                     [album]="$album_title"
                     [tracknumber]="$track_in_disc/$tracks_on_disc"
                     [discnumber]="$discid/$disc_count"
                     [position-in-set]="$track_total_id/$track_count"
                     [work]="$work"
                     [disctitle]="$disctitle"
                    )

               for i in release version \
                        live_type live_location live_date live_version; do
                 if [ -n "''${!i}" ]; then
                   tags["$(gonodryrun 21 echo "$i" | gocmdnodryrun 22 tr _ -)"]="''${!i}"
                 fi
               done
               for i in "''${!tags[@]}"; do
                 infof "%-16s:  %s\n" "$i" "''${tags[$i]}"
                 args+=(--set-tag="$i"="''${tags[$i]}")
               done
               gocmd 7 metaflac "$input" "''${args[@]}"
               ;;

        *   ) dieusage "file ext '$inext' not handled ($input)"
    esac
  else
    dieusage "file type not handled '$input'"
  fi
}

# ------------------------------------------------------------------------------

getopt_args=( --options vha:
              --long verbose,dry-run,help,debug,show-cmds
              --long artwork:
              --name "$Progname" -- "$@" )
OPTS=$( ''${Cmd[getopt]} "''${getopt_args[@]}" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

UseShowCmds=true
args=()
artwork=""
while true; do
  case "$1" in
    -a | --artwork  ) artwork="$2"           ; shift 2 ;;

    # !!! don't forget to update usage !!!
    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift   ;;
    --show-cmds     ) ShowCmds=true          ; shift   ;;
    -h | --help     ) usage                            ;;
    --dry-run       ) DryRun=true            ; shift   ;;
    --debug         ) Debug=true             ; shift   ;;
    --              ) shift; args+=( "$@" )  ; break   ;;
    *               ) args+=( "$1" )         ; shift   ;;
  esac
done

if $Debug; then
  debug "CALLED_AS: "
  i=0
  while [[ $i -le $# ]]; do
    j="''${@:$i:1}"
    debugf ">  %02d: '$j'\n" $i
    i=$((i+1))
  done

  debug "ARGS: "
  i=0
  while [[ $i -lt ''${#args[@]} ]]; do
    j="''${args[$i]}"
    debugf ">  %02d: '$j'\n" $i
    i=$((i+1))
  done
fi

case $# in
  0 ) usage ;;
  * ) for i in "''${args[@]}"; do
        x="$(gocmdnodryrun 11 realpath --physical --canonicalize-existing "$i")"
        check_ "realpath $i"
        main "$x"
      done
      ;;
esac
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
