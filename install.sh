#!/bin/sh

[ "$EUID" -ne 0 ] && {
  echo "Run the script as root."
  exit 1
}

[ -z "$NARVAL_HOME" ] && {
  export NARVAL_HOME=$(pwd)
  for shellrc in ~/.bashrc ~/.zshrc ~/.profile; do
    [ -f "$shellrc" ] && {
      grep -q "NARVAL_HOME" "$shellrc" || echo "export NARVAL_HOME=$NARVAL_HOME" >> "$shellrc"
      . "$shellrc"
    }
  done
  . ~/.bashrc

}

command -v fasm >/dev/null || cp tools/fasm /usr/local/bin/fasm
command -v cargo >/dev/null || curl https://sh.rustup.rs -sSf | sh

cargo build && cp target/debug/narval /usr/local/bin/narval
