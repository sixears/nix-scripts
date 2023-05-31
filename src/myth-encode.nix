mysql --skip-column-names --raw mythconverg -u mythtv --password=mythtv -e "select season,episode,subtitle,basename from recorded where title  = 'Heartbeat'" | { old_ifs="$IFS"; IFS="$(echo -en \\t)"; while read s e t b; do hb scan /archive0/mythtv/"$b"; done; IFS="$old_ifs"; }


