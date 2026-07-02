#!/usr/bin/env bash
# Deploy the Thunderbird->notmuch/org mail+calendar pipeline on this machine.
# Idempotent: safe to re-run after a `git pull`. Assumes the prerequisites in
# the "mail & calendar" section of config.org are already met (notmuch,
# inotify-tools, thunderbird + Owl with a synced account).
set -euo pipefail

REPO="$HOME/.emacs.d/mail"
BIN="$HOME/.local/bin"
UNITS="$HOME/.config/systemd/user"

mkdir -p "$BIN" "$UNITS"

# Scripts: symlink so a git pull updates the deployed copies automatically.
for f in tb-mbox-sync.py tb-notmuch-watch.sh tb-cal-sync.py tb-cal-watch.sh; do
  ln -sf "$REPO/$f" "$BIN/$f"
  chmod +x "$REPO/$f"
done

# notmuch config: only write if absent, so a customized one is never clobbered.
if [ ! -e "$HOME/.notmuch-config" ]; then
  cp "$REPO/notmuch-config" "$HOME/.notmuch-config"
  echo "installed ~/.notmuch-config"
fi

# systemd user units (they use %h, so no per-machine editing).
cp "$REPO/tb-notmuch.service" "$REPO/tb-calendar.service" "$UNITS/"

systemctl --user daemon-reload
systemctl --user enable tb-notmuch.service tb-calendar.service
# restart (not just start) so a re-run after `git pull` picks up new code
systemctl --user restart tb-notmuch.service tb-calendar.service

# First index (watchers also do an initial pass, but this makes it synchronous).
notmuch new || true

echo
echo "Done. Check:  systemctl --user status tb-notmuch.service tb-calendar.service"
echo "Then in Emacs:  M-x notmuch   /   M-x org-agenda"
