#!/usr/bin/env bash

BROWSER=firefox-developer-edition
CACHE_FILE=~/.local/share/rofi/scripts/cache/github-repos.cache

get_repositories() {
  cat $CACHE_FILE
}

open_repository() {
  (${BROWSER} --new-tab https://github.com/"$1" &) > /dev/null 2>&1
}

main() {
  if [ -z "$1" ]; then
    get_repositories
  else
    open_repository "$1"
  fi
}

main "$@"

exit 0
