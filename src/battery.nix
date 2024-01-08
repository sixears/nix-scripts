{ pkgs, header, ... }: pkgs.writeScriptBin "battery" ''#!/usr/bin/env -S -i ${pkgs.bash}/bin/bash

set -u -o pipefail -o noclobber;
shopt -s nullglob
shopt -s nullglob
shopt -s dotglob

source ${header}

Cmd[gnuplot]=${pkgs.gnuplot}/bin/gnuplot
Cmd[perl]=${pkgs.perl}/bin/perl

readonly BATTERIES=/sys/bus/acpi/drivers/battery
readonly BATT1=$BATTERIES/PNP0C0A\:00
readonly BATT1UEVENT=$BATT1/power_supply/BAT0/subsystem/BAT0/uevent
readonly TAB="$(echo -en '\t')"

Brief=false
Simple=false
if [[ -t 1 ]]; then Colour=true; else Colour=false; fi

# ------------------------------------------------------------------------------

colour()             { printf -v "$3" "\033[%dm%s\033[0m" "$1" "$2"; }
colour_light_red()   { colour 91 "$@"; }
colour_light_green() { colour 92 "$@"; }
colour_light_cyan()  { colour 96 "$@"; }

# --------------------------------------

div_1m_3sf() {
  local val="$1" varname="$2"
  local mwh="$((val/1000))"
  local wh_i="$(($mwh/1000))"
  local wh_r="$(($mwh%1000))"
  printf -v "$varname" %d.%03d "$wh_i" "$wh_r"
}

# --------------------------------------

split2() {
  local delimiter="$1" var="$2" keyvar="$3" valuevar="$4"
  local IFS="$delimiter"
  read "$keyvar" "$valuevar" <<< "$var"
}

# --------------------------------------

sortwords() {
  echo "$@" | ''${Cmd[tr]} ' ' '\n' | ''${Cmd[sort]} | ''${Cmd[sort]}
}

# --------------------------------------

show_location() { echo "$BATT1UEVENT"; }

# --------------------------------------

raw() { gocmd 15 cat "$BATT1UEVENT"; }

# --------------------------------------

# print to var, terminated with a backslash-newline
bknl() { printf -v "$1" '%s\\\n' "$2"; }

# http://gnuplot.sourceforge.net/demo/nonlinear1.html
# How to make a broken x-axis
#  axis_gap = 25.
#  f(x) = (x <= 100) ? x : (x < 500) ? NaN : (x - 400 + axis_gap)
#  g(x) = (x <= 100) ? x : (x < 100 + axis_gap) ? NaN : (x + 400 - axis_gap)
#  set xrange [15:600] noextend
#  set nonlinear x via f(x) inverse g(x)

#perl -nlF/\\t/ -E '$a{$F[0]}=undef;END{@a=sort keys %a;my $b=0;for my $a (@a) { say "$b $a" if $a - $b > 300;$b=$a} }' /tmp/battery.raw
#(1611933475 # 74
#1611933549) (1611956790 # 479
#1611957269) (1611989200 # 329
#1611989529) (1612002528 # 65
#1612002593

# ranges 1611933475 - 1611933549
#        1611956790 - 1611957269

gscript() {
  local file=/tmp/battery.tdv
# gnuplotting.org
  local result="$(''${Cmd[perl]} -plE 's{\*$}{\\}' <<'EOF'
     set terminal postscript eps enhanced color solid colortext 9;
     set output 'multiple_plots.eps';
     set title 'battery performance';

  axis_gap = 25.
#  f(x) = (x <= 1611933549) ? x : (x < 1611956790) ? NaN : (x <= 1611956790) ? (x - (1611956790-1611933549) + axis_gap) : (x < 1611957269) ? NaN : (x - (1611957269-1611956790) + axis_gap)
  f(x) = (x <= 1611933549) ? x : (x < 1611956790) ? NaN : (x <= 1611956790) ? (x - (1611956790-1611933549) + axis_gap) : (x < 1611957269) ? NaN : (x - (1611957269-1611956790) + axis_gap)
  g(x) = (x <= 1611933549) ? x : (x < 1611956790 + axis_gap) ? NaN : (x + (1611956790-1611933549) - axis_gap)
  set xrange [1611933475:1612002593] noextend
  set nonlinear x via f(x) inverse g(x)

set xlabel "date-time"
# set ylabel "";
set y2label "energy (Wh)";
set x2label "bob";
     set xdata time
     set timefmt "%s"
     set ytics nomirror;
     set y2tics nomirror;

# set yrange [0:20000]

# set ytic 0,5000
# set y2range [0:120000]
# set y2tic 0,20000;
set style function linespoints
set pointintervalbox 3
# set style line 1 lw 4 lc rgb '#990042' ps 0.1 pt 6 pi 5
set style line 1 lc rgb '#0060ad' lt 1 lw 2 pt 7 pi -1 ps 1.5
set style line 2  lc rgb '#0025ad' lt 1 lw 1.5 # --- blue
# set style line 2 lw 3 lc rgb '#31f120' ps 2 pt 12 pi 3
set style line 3 lw 3 lc rgb '#0044a5' ps 2 pt 9 pi 5
set style line 4 lw 4 lc rgb '#888888' ps 2 pt 7 pi 4
;
EOF
)"
#  [martyn:tmp:0]$ perl -plF'/\t/' -i -E 'say "" if $l < $F[0]-300;$l=$F[0];' /tmp/battery.raw

  bknl result "''${result}plot '/tmp/battery.raw' using 1:3 ls 1  title 'energy (Wh)'  axis x1y2,"
  bknl result "''${result}     ''\'''\'           using 1:4 w l title 'p.d. (V)'     axis x1y1,"
  bknl result "''${result}     ''\'''\'           using 1:6 w lp title 'min p.d. (V)' axis x1y1,"
  result+="     ''\'''\'           using 1:5 w lp title 'power(W)' axis x1y2"
  printf -v "$1" %s "$result"
}

gnuplot() {
  local gtxt
  gscript gtxt

  local gscript_fn
  mktemp gscript_fn
  # use of 'tee' allows us to write but not in dry-run mode; but without the
  # complexity of an eval
  go 12 echo "$gtxt" | gocmd 13 tee "$gscript_fn" > /dev/null
  ''${Cmd[cat]} "$gscript_fn"

  gocmd 14 gnuplot "$gscript_fn"
}

dump_gscript() { local gtxt; gscript gtxt; echo "$gtxt"; }

# --------------------------------------

main() {
  local datetime="$(gocmd 11 date +"%s$TAB%FZ%T")"; check_ date

  local -A uevent=()
  [[ -e $BATT1UEVENT ]] || die 3 "No battery found (at $BATT1UEVENT)"
  local l; while read l; do
             local key val
             split2 = "$l" key val
             if [[ POWER_SUPPLY_ == ''${key:0:13} ]]; then
               uevent[''${key:13}]="$val"
             else
               warn "Unrecognized key: '$key' (''${key:0:13})"
             fi
           done < $BATT1UEVENT

  local k v
  for k in $( sortwords "''${!uevent[@]}" ); do
    debug "$k: ''${uevent[$k]}"
  done

  local energy_design energy_full energy_now
  if [[ -v uevent[ENERGY_FULL_DESIGN] ]]; then
    energy_design="''${uevent[ENERGY_FULL_DESIGN]}"
    energy_full="''${uevent[ENERGY_FULL]}"
    energy_now="''${uevent[ENERGY_NOW]}"
  elif  [[ -v uevent[CHARGE_FULL_DESIGN] ]]; then
    energy_design="''${uevent[CHARGE_FULL_DESIGN]}"
    energy_full="''${uevent[CHARGE_FULL]}"
    energy_now="''${uevent[CHARGE_NOW]}"
  else
    die 5 "No charge stats found in '$BATT1UEVENT' (''${!uevent[*]})"
  fi

  local energy_design_wh energy_full_wh energy_now_wh
  div_1m_3sf "$energy_design" energy_design_wh
  div_1m_3sf "$energy_full"   energy_full_wh
  div_1m_3sf "$energy_now"    energy_now_wh

  local energy_capacity_pc=$((100*energy_full/energy_design))
  local energy_capacity_wh="''${energy_full_wh}/''${energy_design_wh}"
  local charge_pc="$((100*energy_now/energy_full))"

  local voltage_design_min voltage_now
  div_1m_3sf ''${uevent[VOLTAGE_MIN_DESIGN]} voltage_design_min
  div_1m_3sf ''${uevent[VOLTAGE_NOW]}        voltage_now

  local v_now="$voltage_now"
  local charge_pc_pc="''${charge_pc}%"
  if $Colour && ! $Simple; then
    if [[ ''${uevent[VOLTAGE_NOW]} -lt ''${uevent[VOLTAGE_MIN_DESIGN]} ]]; then
      colour_light_red "''${voltage_now}V" v_now
    else
      colour_light_cyan "''${voltage_now}V" v_now
    fi

    if [[ $charge_pc -gt 50 ]]; then
      colour_light_cyan "''${charge_pc}%" charge_pc_pc
    elif [[ $charge_pc -lt 25 ]]; then
      colour_light_red "''${charge_pc}%" charge_pc_pc
    else
      colour_light_green "''${charge_pc}%" charge_pc_pc
    fi
  fi

  local v_health="''${v_now} (design min: ''${voltage_design_min}V)"

  local disch_m disch_h disch_time
  if [[ 0 -eq ''${uevent[POWER_NOW]:-0} ]]; then
    disch_m=0
    disch_h=0
    disch_time=0
  else
    disch_m=$((60*''${uevent[ENERGY_NOW]}/''${uevent[POWER_NOW]}))
    disch_h="$( printf %dh%02dm $(($disch_m/60)) $(($disch_m%60)) )"
    disch_time="''${disch_m}mins ($disch_h)"
  fi

  local power_w; div_1m_3sf ''${uevent[POWER_NOW]:-0} power_w;
  local power_w_w="''${power_w}W"

  if $Brief; then
    echo "$datetime$TAB''${charge_pc_pc}$TAB$disch_time$TAB$v_now$TAB$power_w_w"
  elif $Simple; then
    echo "$datetime$TAB''${energy_now_wh}$TAB$voltage_now$TAB$power_w$TAB$voltage_design_min"
  else
    echo "Capacity Health: $energy_capacity_pc% (''${energy_capacity_wh}Wh)"
    echo "Charge: ''${charge_pc_pc}"
    echo "Voltage Health: $v_health"
    echo "Discharge Time: ''${disch_time}"
    echo "Power Now: ''${power_w}"
  fi
}

watch() { while main once; do gocmd 10 sleep "$1"; done; }

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
usage: $Progname OPTION* MODE?

Show battery status.

Modes:
  once ) Show the battery status.  This is the default.
  loc  ) Show where we're looking for the battery status file.

Options:
 -c | --colour     Force colourized output
 -n | --no-colour  Force monochrome output
 -b | --brief      Give brief (one-line) output.
 -s | --simple     Like brief, but without units, colour and with est. time
                   remaining in minutes only (for, e.g., gnuplot)
 -v | --verbose    Be more garrulous about happenings.
 --dry-run         Don't make any stateful changes to the world.
 --debug           Output additional statements to help developers debug.
 --help            This help text.
EOF
)"

orig_args=("$@")
getopt_args=( -o vbncs
              --long colour,no-colour,brief,simple
              --long verbose,dry-run,help,debug
            )
OPTS=$( ''${Cmd[getopt]} "''${getopt_args[@]}" -n "$Progname" -- "$@" )

[ $? -eq 0 ] || die 2 "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

artist=""
users=()
while true; do
  case "$1" in
    -b | --brief       ) Brief=true   ; shift ;;
    -s | --simple      ) Simple=true  ; shift ;;
    -c | --colour      ) Colour=true  ; shift ;;
    -n | --no-colour   ) Colour=false ; shift ;;
    # !!! don't forget to update usage !!!
    -v | --verbose  ) Verbose=$((Verbose+1))   ; shift   ;;
    --help          ) usage                              ;;
    --dry-run       ) DryRun=true              ; shift   ;;
    --debug         ) Debug=true               ; shift ;;
    --              ) args+=("''${@:2}")       ; break ;;
    *               ) args+=("$1")             ; shift ;;
  esac
done

debug "CALLED AS: $(showcmd "$0" "''${orig_args[@]}")"

if $Brief && $Simple; then
  dieusage "Choose --brief(-b) or --simple(-s) but not both!"
fi

case ''${#args[@]} in
  0 ) main  once;;
  1 ) case "''${args[0]}" in
        once ) main once                                 ;;
        loc  ) show_location                             ;;
        raw  ) raw                                       ;;
        gnuplot ) gnuplot ;;
        gscript ) dump_gscript ;;
        *    ) if [[ ''${args[0]} =~ ^[0-9]+$ ]] \
                    && [[ 0 -lt ''${args[0]} ]]; then
                 watch ''${args[0]}
               else
                 dieusage "unknown mode ''\'''${args[0]}'"
               fi
               ;;

      esac ;;
  2 ) case "''${args[0]}" in
        watch ) if [[ ''${args[1]} =~ ^[0-9]+$ ]] \
                    && [[ 0 -lt ''${args[0]} ]]; then
                  watch  ''${args[1]}
                else
                  dieusage "watch arg ''\'''${args[1]}' should be a posint"
                fi
                ;;
        *     ) dieusage "unknown mode ''\'''${args[@]}'" ;;
      esac ;;
  * ) usage ;;
esac
''

# Local Variables:
# mode: sh
# End:
