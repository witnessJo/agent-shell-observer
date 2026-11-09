# agent-shell-observer

Watch and ask the coding agents of
[agent-shell](https://github.com/xenodium/agent-shell) from one place in Emacs.

![agent-shell-observer: the session list and a conversation beside the code](images/observer.png)

Select some code and ask about it. The question goes to your project's agent
shell with the file and line numbers attached, so the agent can read the code
around it. Every agent session, whether you started it from here or on its own,
shows up in one list on the right of the frame. Each row shows:

- what the session is doing: working, waiting for you, idle, or closed
- when it last did something
- which agent it is and which project it works in
- what it is working on

Open a row to see its branch, what it is waiting for, and its questions. The
conversation shows below the list.

## Features

- **Ask about code.** `agent-shell-observer-explain` sends the region (or the
  current line) to the project's agent shell and asks for an explanation.
  `agent-shell-observer-ask` asks your own question about it.
  `agent-shell-observer-prompt` picks one of the prompts you have registered,
  such as "review: bugs" or "test: write unit tests".
- **Choose the agent.** Questions go to the first agent in
  `agent-shell-observer-agents` that is installed. If an agent fails (for
  example, it hits a usage limit) or never starts (for example, it is not
  signed in), it is skipped for a while and the question goes to the next
  agent. `agent-shell-observer-set-first-agent` sets which agent is tried
  first.
- **One list for every session.** `agent-shell-observer-toggle` shows or hides
  the list and the conversation beside it. Press `L` in the list to put the
  conversation below the list, to its right, or to its left.
- **Notifications.** While the list is closed, the mode line counts the shells
  waiting for you (⚑) and the ones working (▶). A desktop notification tells
  you when a shell asks for a permission, finishes, or fails, unless you are
  already looking at it.
- **History.** Your questions and their agent sessions are saved, so you can
  bring a conversation back after restarting Emacs.
- **Readable input.** Markdown in your own messages is rendered the same way
  agent-shell renders the agent's replies.

## Requirements

- Emacs 29.1 or later
- [agent-shell](https://github.com/xenodium/agent-shell) 0.91.1 or later, with
  at least one agent set up (Claude Code, Codex, Antigravity, opencode, …)
- Optional: [Ollama](https://ollama.com) running locally, to write short titles
  for questions (see `agent-shell-observer-summarize-titles`)

## Installation

With `use-package` and Emacs 30 or later:

```elisp
(use-package agent-shell-observer
  :vc (:url "https://github.com/witnessJo/agent-shell-observer" :rev :newest)
  :commands (agent-shell-observer-explain agent-shell-observer-ask
             agent-shell-observer-prompt agent-shell-observer-toggle
             agent-shell-observer-set-first-agent)
  :autoload agent-shell-observer-watch-shell
  :init
  ;; Track every agent shell from the moment it starts.
  (add-hook 'agent-shell-mode-hook #'agent-shell-observer-watch-shell)
  :bind (("C-c a e" . agent-shell-observer-explain)
         ("C-c a a" . agent-shell-observer-ask)
         ("C-c a l" . agent-shell-observer-prompt)
         ("C-c a h" . agent-shell-observer-toggle)))
```

Or clone the repository and use `:load-path` instead of `:vc`.

## Commands

| Command | What it does |
|---|---|
| `agent-shell-observer-explain` | Ask the agent to explain the region or the current line |
| `agent-shell-observer-ask` | Ask your own question about the region or the current line |
| `agent-shell-observer-prompt` | Ask one of `agent-shell-observer-prompts`, picked by name |
| `agent-shell-observer-toggle` | Show or hide the session list and the conversation |
| `agent-shell-observer-set-first-agent` | Choose the agent tried first, and save the order with Custom |

### In the list

| Key | What it does |
|---|---|
| `TAB` / click the arrow | Expand or collapse a session |
| `RET` / double click | Open the conversation below the list |
| `v` | Go to the code a question was about |
| `f` | Show all sessions, only those waiting for you, or only those working |
| `/` | Filter by agent, project, branch or state |
| `L` | Put the conversation below, to the right of, or to the left of the list |
| `D` | Remove a question, or a whole session, from the list (the agent keeps its sessions) |
| `g` | Refresh |
| `q` | Hide the list and the conversation |

## Options

| Variable | Default | |
|---|---|---|
| `agent-shell-observer-agents` | `(antigravity claude-code codex)` | Agents to ask, most wanted first |
| `agent-shell-observer-prompt-hook` | `nil` | Text added to the end of every prompt, e.g. `"Answer in Korean."` |
| `agent-shell-observer-prompts` | a dozen prompts | `(NAME . PROMPT)` pairs for `agent-shell-observer-prompt` |
| `agent-shell-observer-layout` | `below` | Where the conversation goes: `below`, `right` or `left` of the list |
| `agent-shell-observer-history-file` | `agent-shell-observer-history.eld` in `user-emacs-directory` | Where questions are saved; `nil` turns saving off |
| `agent-shell-observer-notify-states` | `(waiting done error)` | Which states send a desktop notification |
| `agent-shell-observer-summarize-titles` | `t` | Have a local Ollama model write each question's title |
| `agent-shell-observer-owned-functions` | `nil` | Functions that mark a shell as driven by something else, so its prompts don't become sessions of their own |

## Tests

```sh
emacs -Q --batch -l test/agent-shell-observer-test.el -f ert-run-tests-batch-and-exit
```
