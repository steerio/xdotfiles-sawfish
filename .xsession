export NO_AT_BRIDGE=1

if [ -e $HOME/.fehbg ]; then
  . $HOME/.fehbg
fi

syndaemon -dki 0.5
setxkbmap -layout us,hu,il -variant ,,phonetic -option caps:escape -option grp:alt_space_toggle

picom -b
exec sawfish
