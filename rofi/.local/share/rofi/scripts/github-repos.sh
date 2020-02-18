#!/bin/zsh

CACHE_FILE=~/.local/share/rofi/scripts/cache/github-repos.cache

# Get all the repositories for the user with curl and GitHub API
fetch_repositories() {
  PAGES=$(curl -s -I -H "authorization: token $GITHUB_TOKEN" "https://api.github.com/user/repos?per_page=100&page=1" | sed -r -n '/^Link:/ s/.*page=([0-9]+).*/\1/p')
  for INDEX in {1..$PAGES}; do
    curl -s -H "authorization: token $GITHUB_TOKEN" "https://api.github.com/user/repos?per_page=100&page=$INDEX" | jq -r 'map(.full_name) | join("\n")' >> $CACHE_FILE
  done
}

main() {
  echo > $CACHE_FILE
  fetch_repositories
}

main

exit 0
