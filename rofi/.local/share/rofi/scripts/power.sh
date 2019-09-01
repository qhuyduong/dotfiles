#!/usr/bin/env bash

main() {
  if [ -z "$1" ]; then
    printf "Poweroff\nReboot\nSuspend"
  else
    systemctl "$(echo "$1" | tr '[:upper:]' '[:lower:]')"
    # systemctl "$1"
  fi
}

main "$@"

exit 0
