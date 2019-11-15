#!/usr/bin/env bash

CACHE_FILE=~/.local/share/rofi/scripts/cache/github-repos.cache

# Get all the repositories for the user with curl and GitHub API
fetch_repositories() {
  touch $CACHE_FILE
  for INDEX in 1 2 3; do
    curl -s -H "authorization: token $GITHUB_TOKEN" "https://api.github.com/user/repos?per_page=100&page=$INDEX" | jq -r 'map(.full_name) | join("\n")' >> $CACHE_FILE
  done
}

main() {
  fetch_repositories
}

main

exit 0
