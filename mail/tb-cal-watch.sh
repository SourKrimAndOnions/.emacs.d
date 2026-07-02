#!/usr/bin/env bash
# Regenerate the Exchange org-agenda file whenever Owl updates the calendar
# cache. Same block-settle-run debounce as the mail watcher.
set -uo pipefail

GEN="$HOME/.local/bin/tb-cal-sync.py"
CALDIR="$(python3 "$GEN" --print-watch-dir)" || { echo "cannot locate calendar dir"; exit 1; }

run() { python3 "$GEN" >/dev/null 2>&1; }

run   # initial build / catch-up

while true; do
  inotifywait -q -q -e close_write -e modify -e moved_to \
    "$CALDIR" >/dev/null 2>&1 || { sleep 5; continue; }
  sleep 5
  run
done
