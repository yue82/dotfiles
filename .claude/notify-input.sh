#!/usr/bin/env bash
set -euo pipefail

raw=$(cat)

# 受信した JSON を全文記録 (デバッグ用、不要になったら消す)
{ echo "--- $(date -Iseconds) ---"; echo "$raw"; } >> /tmp/claude-notification-hook.log

event=$(echo "$raw" | jq -r '.hook_event_name // ""')
tool=$(echo "$raw" | jq -r '.tool_name // ""')
msg=$(echo "$raw" | jq -r '.message // ""')

if [ "$event" = "PermissionRequest" ] && [ -n "$tool" ]; then
  body="${tool} 許可待ち"
elif [ -n "$msg" ]; then
  body="$msg"
else
  body="入力待ち"
fi

# Claude が動いている pane の所属 window の番号:名前 を取得
# TMUX_PANE は Claude プロセスが動いている pane の ID (フックに継承される)
if [ -n "${TMUX_PANE:-}" ]; then
  window=$(tmux display-message -p -t "$TMUX_PANE" '#I:#W' 2>/dev/null || echo "?")
else
  window=$(tmux display-message -p '#I:#W' 2>/dev/null || echo "?")
fi

# -d 0 で「次のキー押下まで」表示 (window 切替えキーでも消える)
tmux display-message -d 0 "Claude [${window}]: ${body}" 2>/dev/null || true
