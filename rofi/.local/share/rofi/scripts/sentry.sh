#!/usr/bin/env bash

BROWSER=google-chrome-stable
CACHE_FILE=~/.local/share/rofi/scripts/cache/sentry-projects.cache

get_projects() {
  cat $CACHE_FILE
}

get_project_id_from_name() {
  get_projects | jq -r --arg PROJECT_NAME "$1" '.[] | select(.name == $PROJECT_NAME).id'
}

open_project() {
  (${BROWSER} --new-tab https://sentry.io/organizations/employment-hero/issues/?environment=production\&project=$1 &) > /dev/null 2>&1
}

main() {
  if [ -z "$1" ]; then
    get_projects | jq -r 'map(.name) | join("\n")'
  else
    open_project $(get_project_id_from_name $1)
  fi
}

main "$@"

exit 0
