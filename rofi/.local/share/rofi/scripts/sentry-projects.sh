#!/bin/zsh

CACHE_FILE=~/.local/share/rofi/scripts/cache/sentry-projects.cache

# Get all the projects for the user with curl and Sentry API
fetch_projects() {
  curl -H "Authorization: Bearer $SENTRY_TOKEN" "https://sentry.io/api/0/projects/" | jq '[ .[] | { id: .id, name: .name } ]' > $CACHE_FILE
}

main() {
  fetch_projects
}

main

exit 0
