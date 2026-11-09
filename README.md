# agent-shell-observer

Watch and ask the coding agents of [agent-shell](https://github.com/xenodium/agent-shell).

- `agent-shell-observer-explain`, `-ask`, `-prompt`: send the region or
  current line, with its file and lines, to the project's agent shell.
- `agent-shell-observer-toggle`: a list of every agent session at the right
  of the frame, with the conversation picked beside it.
- With the list closed, the mode line counts shells waiting for you and
  working, and desktop notifications say when one needs you.

## Setup

```elisp
(use-package agent-shell-observer
  :load-path "~/dev/agent-shell-observer"
  :commands (agent-shell-observer-explain agent-shell-observer-ask
             agent-shell-observer-prompt agent-shell-observer-toggle
             agent-shell-observer-set-first-agent)
  :autoload agent-shell-observer-watch-shell
  :init
  (add-hook 'agent-shell-mode-hook #'agent-shell-observer-watch-shell))
```

## Tests

```sh
emacs -Q --batch -l test/agent-shell-observer-test.el -f ert-run-tests-batch-and-exit
```
