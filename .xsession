if [ -e $HOME/.fehbg ]; then
  . $HOME/.fehbg
else
  xsetroot -solid '#102030'
fi

syndaemon -dki 0.5
exec sawfish
