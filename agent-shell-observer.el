;;; agent-shell-observer.el --- Watch and ask the coding agents -*- lexical-binding: t -*-
;; Copyright (C) 2026 witnessjo

;; Author: witnessjo <jongyoungcha@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.91.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/witnessJo/agent-shell-observer

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Every agent session goes into one list, `*agent-observer*', at the
;; right of the frame, with the agent-shell of the conversation picked
;; below it.  A session's row says whether its shell runs, what it is
;; doing (working, waiting for a permission or a question, idle), its
;; agent, project, git branch, when it last did something and its task,
;; and opens like a drop-down onto its details: where it works and what
;; it waits for.  The answers, permission requests and follow-ups are the
;; shell's own.  `agent-shell-observer-toggle' shows or hides both.
;;
;; With the list closed, the mode line counts the shells waiting for you
;; (⚑) and those working (▶), and a desktop notification says when one
;; asks for something, finishes or fails, unless it is the window in
;; front of you.
;;
;; To follow every agent shell from its start:
;;
;;   (add-hook 'agent-shell-mode-hook #'agent-shell-observer-watch-shell)
;;
;; Commands of one's own can send prompts that the list follows, with
;; `agent-shell-observer-submit'; `agent-shell-observer-failed-functions'
;; hears when one fails, to send it elsewhere.

;;; Code:

(require 'seq)
(require 'map)
(require 'tabulated-list)
(require 'subr-x)
(require 'agent-shell-observer-display)

(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--resolve-config-designator "agent-shell")
(declare-function agent-shell--update-bootstrapping-fragment "agent-shell" (&rest args))
(declare-function agent-shell--can-insert-into-prompt-p "agent-shell-prompt")
(declare-function agent-shell-prompt-queue "agent-shell-prompt-queue")
(declare-function meow-motion-mode "meow-core")
(defvar agent-shell--state)

;;; Options

(defgroup agent-shell-observer nil
  "Watch and ask the coding agents."
  :group 'tools)

(defvar agent-shell-observer-start-seconds 45
  "How long a shell that has no session yet may keep a prompt waiting.
An agent that is not signed in never starts one, and says nothing; its
prompts fail after this many seconds, for
`agent-shell-observer-failed-functions' to send them elsewhere.")

(defvar agent-shell-observer-list-display-action
  '((display-buffer-in-side-window)
    (side . right)
    (slot . 1)
    (window-width . 0.45))
  "How the list of sessions is shown.")

(defvar agent-shell-observer-shell-display-action
  '((display-buffer-in-side-window)
    (side . right)
    (slot . 2)
    (window-width . 0.45))
  "How the shell answering the question picked in the list is shown.")

(defvar agent-shell-observer-list-share 0.2
  "The part of the side's height the list takes, the shell the rest.")

(defcustom agent-shell-observer-layout 'below
  "Where the conversation goes beside the list.
`below' it, the two as one panel at the right of the frame; or to its
`right' or `left', the two side by side at the right of the frame.  L in
the list goes through them."
  :type '(choice (const :tag "Below the list" below)
                 (const :tag "Right of the list" right)
                 (const :tag "Left of the list" left))
  :group 'agent-shell-observer)

(defvar agent-shell-observer-wide-width 0.6
  "The part of the frame's width the list and conversation take side by side.")

(defvar agent-shell-observer-wide-list-share 0.4
  "The part of that width the list takes, the conversation the rest.")

;;; History

(defvar agent-shell-observer-history-file
  (locate-user-emacs-file "agent-shell-observer-history.eld")
  "File keeping questions and agent session identifiers across restarts.
Conversation contents remain with the agent; opening a saved question
asks that agent to restore its session.  Nil disables persistence.")

(defvar agent-shell-observer-history-limit 100
  "Maximum number of questions to keep, newest first.
This limits the local list and saved index; agent-owned sessions remain intact.")

(defvar agent-shell-observer-restore-message-limit 100
  "Maximum recent user and agent messages retained during history replay.
Chunks of the same message count once.  Tool and thought notifications
stay with their message.  The ACP server may still transmit older history.")

;;; Agents

(defun agent-shell-observer--agent-of (shell-buffer)
  "Return the identifier of the agent SHELL-BUFFER runs."
  (with-current-buffer shell-buffer
    (map-nested-elt agent-shell--state '(:agent-config :identifier))))

;;; The list

(defconst agent-shell-observer-list-buffer "*agent-observer*"
  "Name of the buffer listing every question asked.")

(defvar agent-shell-observer--entries nil
  "Every question asked, newest first.
Each is a plist of :id, :time, :cwd, :location, :anchor, :title and
:prompt, which stay as they are, and :status, :agent,
:shell, :failed and :tried, which change as the agents answer.
:anchor is where the prompt says the code is, to find it in the shell.")

(defvar agent-shell-observer--next-id 0
  "The id the next question gets.")

(defvar agent-shell-observer--shown nil
  "The question whose shell is shown below the list.")

(defvar agent-shell-observer--history-readable t
  "Nil after a failed history load, to avoid overwriting that file.")

(defconst agent-shell-observer--saved-keys
  '(:id :time :cwd :location :anchor :title :prompt
    :status :agent :session-id :failed :tried :summary)
  "Entry fields that can survive an Emacs restart.")

(defun agent-shell-observer--trim-history ()
  "Keep only the newest `agent-shell-observer-history-limit' questions.
Identifiers increase across restarts, unlike the displayed clock time."
  (unless (and (integerp agent-shell-observer-history-limit) (> agent-shell-observer-history-limit 0))
    (user-error "Agent chat history limit must be a positive integer"))
  (setq agent-shell-observer--entries
        (seq-take (seq-sort-by (lambda (entry) (plist-get entry :id)) #'>
                              agent-shell-observer--entries)
                  agent-shell-observer-history-limit)))

(defun agent-shell-observer--save-history ()
  "Atomically save questions, excluding live buffers and subscriptions."
  (agent-shell-observer--trim-history)
  (when (and agent-shell-observer-history-file agent-shell-observer--history-readable)
    (let* ((file (expand-file-name agent-shell-observer-history-file))
           (directory (file-name-directory file))
           (coding-system-for-write 'utf-8-unix)
           temp)
      (condition-case err
          (unwind-protect
              (progn
                (make-directory directory t)
                (setq temp (make-temp-file (expand-file-name ".agent-shell-observer-" directory)))
                (with-temp-file temp
                  (let ((print-length nil) (print-level nil))
                    (prin1
                     (list :version 1 :entries
                           (mapcar (lambda (entry)
                                     (let (saved)
                                       (dolist (key agent-shell-observer--saved-keys)
                                         (setq saved (plist-put saved key (plist-get entry key))))
                                       saved))
                                   agent-shell-observer--entries))
                     (current-buffer))))
                (set-file-modes temp #o600)
                (rename-file temp file t))
            (when (and temp (file-exists-p temp)) (delete-file temp)))
        (error (message "agent-observer: could not save history: %s"
                        (error-message-string err)))))))

(defun agent-shell-observer--load-history ()
  "Restore the question list without starting any agent processes.
Keep an unreadable file intact, and leave live state alone on failure."
  (let ((file (and agent-shell-observer-history-file
                   (file-exists-p agent-shell-observer-history-file)
                   agent-shell-observer-history-file)))
    (when file
      (condition-case err
          (let* ((data (with-temp-buffer
                         (insert-file-contents file)
                         (let ((data (read (current-buffer))))
                           (skip-chars-forward " \t\r\n")
                           (unless (eobp) (error "Trailing history data"))
                           data)))
                 (entries (plist-get data :entries)))
            (unless (and (equal (plist-get data :version) 1)
                         (proper-list-p entries)
                         (seq-every-p
                          (lambda (entry)
                            (and (proper-list-p entry)
                                 (integerp (plist-get entry :id))
                                 (> (plist-get entry :id) 0)
                                 (seq-every-p (lambda (key) (stringp (plist-get entry key)))
                                              '(:time :cwd :location :anchor :prompt :status))
                                 (symbolp (plist-get entry :agent))
                                 (or (null (plist-get entry :session-id))
                                     (stringp (plist-get entry :session-id)))))
                          entries))
              (error "Invalid history format"))
            (dolist (entry entries)
              (plist-put entry :shell nil)
              (when (member (plist-get entry :status)
                            '("Waiting" "Answering…" "Permission" "Restoring…"))
                (plist-put entry :status "Stopped (Emacs restarted)")))
            (setq agent-shell-observer--entries entries
                  agent-shell-observer--next-id
                  (apply #'max 0 (mapcar (lambda (entry) (plist-get entry :id)) entries))
                  agent-shell-observer--shown nil
                  agent-shell-observer--history-readable t)
            (agent-shell-observer--trim-history))
        (error
         (setq agent-shell-observer--history-readable nil)
         (message "agent-observer: could not load history (saving disabled): %s"
                  (error-message-string err)))))))

(defun agent-shell-observer--entry (id)
  "Return the question whose id is ID."
  (seq-find (lambda (entry) (eq (plist-get entry :id) id))
            agent-shell-observer--entries))

(defun agent-shell-observer--status-face (status)
  "Return the face STATUS is shown in."
  (cond ((equal status "Done") 'success)
        ((string-prefix-p "Error" status) 'error)
        ((string-prefix-p "Stopped" status) 'shadow)
        (t 'warning)))

(defun agent-shell-observer--live-p (shell)
  "Return non-nil when SHELL is a shell whose agent is still running.
The shell's own process and, once it has one, its agent's connection
both have to be alive: either one ending leaves nothing to talk to."
  (and (buffer-live-p shell)
       (process-live-p (get-buffer-process shell))
       (let ((agent (and (local-variable-p 'agent-shell--state shell)
                         (map-nested-elt (buffer-local-value 'agent-shell--state shell)
                                         '(:client :process)))))
         (or (not (processp agent)) (process-live-p agent)))))

;;;; Sessions
;;
;; The list has a row per agent session: a shell that runs, or the
;; questions of one whose shell is closed, kept together by their
;; session identifier.  A session row opens like a drop-down, TAB or a
;; click on its arrow, onto where it works, what it waits for, and its
;; questions, newest first; RET or a double click on a question opens
;; that conversation below the list, and on the session its newest one.
;; A session opens by itself when a question is asked in it, not when it
;; is opened or brought back, and stays as it is left.

(defvar agent-shell-observer--opened (make-hash-table :test #'equal)
  "Sessions opened in the list, by key.")

(defvar agent-shell-observer--closed (make-hash-table :test #'equal)
  "Sessions closed in the list by hand, by key, which stay closed.")

(defconst agent-shell-observer--asking-statuses
  '("Waiting" "Answering…" "Permission")
  "Statuses of questions being asked, not of conversations brought back.")

(defun agent-shell-observer--session-id-of (shell)
  "Return the identifier of SHELL's session, or nil."
  (and (local-variable-p 'agent-shell--state shell)
       (map-nested-elt (buffer-local-value 'agent-shell--state shell) '(:session :id))))

(defun agent-shell-observer--sessions ()
  "Return the sessions of the list, the one active last first.
Each is a plist of :key, :shell, live or nil, and :entries, its
questions newest first.  Questions whose shell is closed join a shell
that runs their session again, or one another by session identifier."
  (let ((by-key (make-hash-table :test #'equal))
        (by-session (make-hash-table :test #'equal))
        sessions)
    (dolist (shell (agent-shell-observer--shells))
      (let ((session (list :key shell :shell shell :entries nil)))
        (puthash shell session by-key)
        (push session sessions)
        (when-let* ((id (agent-shell-observer--session-id-of shell)))
          (puthash (cons (agent-shell-observer--agent-of shell) id) shell by-session))))
    (dolist (entry agent-shell-observer--entries)
      (let* ((shell (plist-get entry :shell))
             (id (plist-get entry :session-id))
             (key (cond ((buffer-live-p shell) shell)
                        ((and id (gethash (cons (plist-get entry :agent) id) by-session)))
                        (id (list 'session (plist-get entry :agent) id))
                        (t (list 'question (plist-get entry :id)))))
             (session (or (gethash key by-key)
                          (let ((new (list :key key
                                           :shell (and (bufferp key) key)
                                           :entries nil)))
                            (puthash key new by-key)
                            (push new sessions)
                            new))))
        (plist-put session :entries (append (plist-get session :entries) (list entry)))))
    (sort sessions
          (lambda (a b)
            (let ((ra (agent-shell-observer--session-rank a))
                  (rb (agent-shell-observer--session-rank b)))
              (or (> (car ra) (car rb))
                  (and (= (car ra) (car rb)) (> (cdr ra) (cdr rb)))))))))

(defun agent-shell-observer--session-rank (session)
  "Return how recent SESSION is, as (LIVE . WHEN): running shells first."
  (let ((shell (plist-get session :shell))
        (newest (or (plist-get (car (plist-get session :entries)) :id) 0)))
    (if (agent-shell-observer--live-p shell)
        (cons 1 (or (buffer-local-value 'agent-shell-observer--shell-activity shell) newest))
      (cons 0 newest))))

(defun agent-shell-observer--session-of (entry)
  "Return the session ENTRY belongs to."
  (seq-find (lambda (session) (memq entry (plist-get session :entries)))
            (agent-shell-observer--sessions)))

(defun agent-shell-observer--session-by-key (key)
  "Return the session whose key is KEY, or nil.
A shell killed since the list was drawn is found by its questions."
  (let ((sessions (agent-shell-observer--sessions)))
    (or (seq-find (lambda (session) (equal (plist-get session :key) key)) sessions)
        (and (bufferp key)
             (seq-find (lambda (session)
                         (seq-some (lambda (entry) (eq (plist-get entry :shell) key))
                                   (plist-get session :entries)))
                       sessions)))))

(defun agent-shell-observer--session-state (session)
  "Return what SESSION's shell is doing, `closed' once it no longer runs.
While its conversation is being brought back, `restoring'."
  (let ((shell (plist-get session :shell)))
    (cond
     ((seq-some (lambda (entry) (equal (plist-get entry :status) "Restoring…"))
                (plist-get session :entries))
      'restoring)
     ((agent-shell-observer--live-p shell)
      (buffer-local-value 'agent-shell-observer--shell-status shell))
     (t 'closed))))

(defun agent-shell-observer--session-agent (session)
  "Return SESSION's agent, as a string."
  (let ((shell (plist-get session :shell)))
    (format "%s" (or (if (buffer-live-p shell)
                         (or (agent-shell-observer--agent-of shell)
                             ;; A shell not yet started has no config, but
                             ;; its buffer is named after its agent.
                             (let ((name (buffer-name shell)))
                               (and (string-match "\\`\\(.+?\\) Agent @" name)
                                    (downcase (match-string 1 name)))))
                       (plist-get (car (plist-get session :entries)) :agent))
                     ""))))

(defun agent-shell-observer--session-cwd (session)
  "Return the directory SESSION works in."
  (let ((shell (plist-get session :shell)))
    (or (and (buffer-live-p shell)
             (with-current-buffer shell
               (or (ignore-errors (agent-shell-cwd)) default-directory)))
        (plist-get (car (plist-get session :entries)) :cwd)
        default-directory)))

(defun agent-shell-observer--session-task (session)
  "Return what SESSION is about, as a title of a few words.
The agent's own title when it gives one, else the title written for the
session's first question, else that question as typed, or where it was
asked when it was a default one, or the shell's last prompt."
  (let ((shell (plist-get session :shell))
        (first (car (last (plist-get session :entries)))))
    (or (and (buffer-live-p shell)
             (buffer-local-value 'agent-shell-observer--shell-title shell))
        (and first (plist-get first :summary))
        (and first (plist-get first :title))
        (and first (plist-get first :location))
        (and (buffer-live-p shell)
             (buffer-local-value 'agent-shell-observer--shell-task shell))
        "")))

(defun agent-shell-observer--session-time (session)
  "Return when SESSION last did something, as HH:MM."
  (let ((shell (plist-get session :shell)))
    (or (and (agent-shell-observer--live-p shell)
             (when-let* ((activity (buffer-local-value 'agent-shell-observer--shell-activity
                                                       shell)))
               (format-time-string "%H:%M" activity)))
        (plist-get (car (plist-get session :entries)) :time)
        "")))

(defvar agent-shell-observer-branch-seconds 30
  "How long a project's git branch is shown before it is read again.")

(defvar agent-shell-observer--branches (make-hash-table :test #'equal)
  "Git branches read, as DIRECTORY to (TIME . BRANCH).")

(defun agent-shell-observer--branch (directory)
  "Return DIRECTORY's git branch, read again after a while, or \"\"."
  (let ((known (gethash directory agent-shell-observer--branches)))
    (if (and known (< (- (float-time) (car known)) agent-shell-observer-branch-seconds))
        (cdr known)
      (let ((branch (or (and (file-directory-p directory)
                             (ignore-errors
                               (let ((default-directory directory))
                                 (car (process-lines "git" "rev-parse"
                                                     "--abbrev-ref" "HEAD")))))
                        "")))
        (puthash directory (cons (float-time) branch) agent-shell-observer--branches)
        branch))))

(defun agent-shell-observer--session-branch (session)
  "Return the git branch of SESSION's directory while its shell runs."
  (if (agent-shell-observer--live-p (plist-get session :shell))
      (agent-shell-observer--branch (agent-shell-observer--session-cwd session))
    ""))

(defun agent-shell-observer--state-label (state)
  "Return STATE as the list shows it, in its face."
  (pcase state
    ('working (propertize "Working" 'face 'warning))
    ('waiting (propertize "Waiting" 'face 'error))
    ('idle (propertize "Idle" 'face 'success))
    ('error (propertize "Error" 'face 'error))
    ('closed (propertize "Closed" 'face 'shadow))
    ('restoring (propertize "Restoring" 'face 'warning))
    (_ (propertize "Starting" 'face 'shadow))))

(defun agent-shell-observer--open-p (session)
  "Return non-nil when SESSION's row is opened in the list.
One not closed by hand opens by itself when a question is asked in it.
Opening it, and so bringing a closed one back, leaves it as it is."
  (let ((key (plist-get session :key)))
    (when (and (not (gethash key agent-shell-observer--closed))
               (seq-some (lambda (entry)
                           (member (plist-get entry :status)
                                   agent-shell-observer--asking-statuses))
                         (plist-get session :entries)))
      (puthash key t agent-shell-observer--opened))
    (gethash key agent-shell-observer--opened)))

(defun agent-shell-observer--set-open (session open)
  "Open SESSION's row in the list when OPEN, close it otherwise."
  (let ((key (plist-get session :key)))
    (if open
        (progn (puthash key t agent-shell-observer--opened)
               (remhash key agent-shell-observer--closed))
      (remhash key agent-shell-observer--opened)
      (puthash key t agent-shell-observer--closed))))

;;;; Titles
;;
;; A session's task is a title of a few words, as chat applications give
;; their conversations: the agent's own when it gives one that is more
;; than "Session" and an id, or else one a local model writes for the
;; session's first question.  The model runs on this machine with Ollama,
;; so nothing is sent away and no credit is spent; it writes a title for
;; each question once, in the background, and the history keeps it.
;; While it is away, or with `agent-shell-observer-summarize-titles' nil,
;; the question's own words stand in.

(defvar agent-shell-observer-summarize-titles t
  "Non-nil to have a local model write a title for each question.")

(defvar agent-shell-observer-title-model "qwen3.5:2b"
  "The Ollama model that writes the titles.")

(defvar agent-shell-observer-title-url "http://localhost:11434/api/chat"
  "Ollama's chat endpoint, on this machine.")

(defvar agent-shell-observer-title-seconds 60
  "How long a title may take before it is given up.")

(defvar agent-shell-observer-title-retry-seconds 300
  "How long to wait after Ollama could not be reached before asking again.")

(defconst agent-shell-observer--title-instruction
  "Give a short title, 3 to 6 words, in the language of the request, for this coding request. Reply with the title only."
  "What the model is told to do with a question.")

(defvar agent-shell-observer--title-queue nil
  "Questions waiting for a title, next first.")

(defvar agent-shell-observer--title-busy nil
  "The question whose title is being written, or nil.")

(defvar agent-shell-observer--title-given-up nil
  "Ids of questions the model gave no title for, not asked again this time.")

(defvar agent-shell-observer--title-resume-at 0
  "When titles may be asked for again, after Ollama could not be reached.")

(defconst agent-shell-observer--generic-title-regexp
  "\\`Session [0-9a-f-]\\{6,\\}\\'"
  "A title that only names the session's id.")

(defun agent-shell-observer--meaningful-title (title)
  "Return TITLE's first line, or nil when it is blank or only the session's id."
  (when-let* ((line (agent-shell-observer--first-line title)))
    (unless (string-match-p agent-shell-observer--generic-title-regexp line)
      line)))

(defvar agent-shell-observer-title-text-function nil
  "Function of an entry returning the text its title is written from.
Nil, or a nil return, for the entry's prompt as sent.  For commands that
add the same text to every prompt, to leave it out of the titles.")

(defun agent-shell-observer--title-source (entry)
  "Return what ENTRY's title is written from."
  (let ((text (or (and agent-shell-observer-title-text-function
                       (funcall agent-shell-observer-title-text-function entry))
                  (plist-get entry :prompt)
                  "")))
    (truncate-string-to-width (string-trim text) 700)))

(defun agent-shell-observer--clean-title (text)
  "Return the title in the model's reply TEXT, or nil when there is none."
  (when-let* ((line (agent-shell-observer--first-line text)))
    (let ((title (string-trim line "[ \t\"'“”‘’`*#:-]+" "[ \t\"'“”‘’`*.。:]+")))
      (unless (string-empty-p title)
        (truncate-string-to-width title 60 nil nil "…")))))

(defvar url-http-end-of-headers)
;; Bound around `url-retrieve', so they have to be special here too.
(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

(defun agent-shell-observer--request-title (text callback)
  "Ask the local model for a title of TEXT, in the background.
CALLBACK gets the title, or nil and the reason: `unreachable' when Ollama
could not be reached, `empty' when it answered with no title."
  (require 'url)
  (let* ((done nil)
         (finish (lambda (title reason)
                   (unless done
                     (setq done t)
                     (funcall callback title reason))))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-serialize
            `((model . ,agent-shell-observer-title-model)
              (stream . :false)
              ;; Thinking first would make a few words take seconds.
              (think . :false)
              (options . ((num_predict . 24) (temperature . 0.2)))
              (messages . [((role . "system")
                            (content . ,agent-shell-observer--title-instruction))
                           ((role . "user") (content . ,text))])))
           'utf-8))
         (buffer (condition-case nil
                     (url-retrieve
                      agent-shell-observer-title-url
                      (lambda (status)
                        (let ((reply (unless (plist-get status :error)
                                       (ignore-errors
                                         (goto-char url-http-end-of-headers)
                                         (map-nested-elt
                                          (json-parse-buffer :object-type 'alist)
                                          '(message content))))))
                          (kill-buffer (current-buffer))
                          (if (plist-get status :error)
                              (funcall finish nil 'unreachable)
                            (let ((title (agent-shell-observer--clean-title reply)))
                              (funcall finish title (unless title 'empty))))))
                      nil t t)
                   (error nil))))
    (if (not buffer)
        (funcall finish nil 'unreachable)
      (run-at-time agent-shell-observer-title-seconds nil
                   (lambda ()
                     (unless done
                       (when (buffer-live-p buffer)
                         (let ((process (get-buffer-process buffer)))
                           (when process (delete-process process)))
                         (kill-buffer buffer))
                       (funcall finish nil 'unreachable)))))))

(defun agent-shell-observer--wants-title-p (entry)
  "Return non-nil when ENTRY has no title written yet and may get one."
  (and (null (plist-get entry :summary))
       (plist-get entry :prompt)
       (not (memq (plist-get entry :id) agent-shell-observer--title-given-up))
       (not (memq entry agent-shell-observer--title-queue))
       (not (eq entry agent-shell-observer--title-busy))))

(defun agent-shell-observer--queue-titles ()
  "Have titles written for the questions without one, newest first."
  (when (and agent-shell-observer-summarize-titles
             (>= (float-time) agent-shell-observer--title-resume-at))
    (setq agent-shell-observer--title-queue
          (append agent-shell-observer--title-queue
                  (seq-filter #'agent-shell-observer--wants-title-p
                              agent-shell-observer--entries)))
    (agent-shell-observer--next-title)))

(defun agent-shell-observer--next-title ()
  "Write the next waiting question's title, unless one is being written."
  (unless agent-shell-observer--title-busy
    (when-let* ((entry (pop agent-shell-observer--title-queue)))
      (setq agent-shell-observer--title-busy entry)
      (agent-shell-observer--request-title
       (agent-shell-observer--title-source entry)
       (lambda (title reason)
         (setq agent-shell-observer--title-busy nil)
         (cond
          (title
           (plist-put entry :summary title)
           (when (memq entry agent-shell-observer--entries)
             (agent-shell-observer--save-history)
             (agent-shell-observer--refresh-list)))
          ((eq reason 'unreachable)
           ;; Ollama is not running: the rest wait for a while.
           (setq agent-shell-observer--title-queue nil
                 agent-shell-observer--title-resume-at
                 (+ (float-time) agent-shell-observer-title-retry-seconds)))
          (t (push (plist-get entry :id) agent-shell-observer--title-given-up)))
         (run-at-time 0 nil #'agent-shell-observer--next-title))))))

;;;; Rows

(defun agent-shell-observer--short-status (status)
  "Return a question's STATUS as the list shows it, in its face."
  (propertize (cond ((string-prefix-p "Stopped" status) "Stopped")
                    ((string-prefix-p "Error" status) "Error")
                    ((equal status "Answering…") "Running")
                    (t (string-remove-suffix "…" status)))
              'face (agent-shell-observer--status-face status)
              'help-echo status))

(defvar agent-shell-observer--link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'agent-shell-observer-follow-link)
    ;; The list's `follow-link' turns a click on a link into mouse-2.
    (define-key map [mouse-2] #'agent-shell-observer-follow-link)
    (define-key map (kbd "RET") #'agent-shell-observer-follow-link)
    map)
  "Keys of the paths in the list that go somewhere.")

(defun agent-shell-observer--link (text action help)
  "Return TEXT as a link: a click or RET on it calls ACTION, HELP says where."
  (propertize text
              'face 'link
              'mouse-face 'highlight
              'help-echo help
              'keymap agent-shell-observer--link-map
              'agent-shell-observer-link action
              'rear-nonsticky t))

(defun agent-shell-observer-follow-link (&optional event)
  "Go where the link clicked, or at point, leads."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (if-let* ((action (get-text-property (point) 'agent-shell-observer-link)))
      (funcall action)
    (user-error "No link here")))

(defface agent-shell-observer-arrow
  '((t :height 1.3 :inherit shadow))
  "Face of the arrow a session's drop-down opens and closes with."
  :group 'agent-shell-observer)

(defvar agent-shell-observer--arrow-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'agent-shell-observer-list-click-arrow)
    ;; The list's `follow-link' turns a click on highlighted text into
    ;; mouse-2, so that has to open and close it too.
    (define-key map [mouse-2] #'agent-shell-observer-list-click-arrow)
    ;; The first click of a double click has opened or closed it already.
    (define-key map [double-mouse-1] #'ignore)
    (define-key map [triple-mouse-1] #'ignore)
    map)
  "Keys of a session's arrow.")

(defun agent-shell-observer--arrow (open)
  "Return the arrow of a session's row, pointing down when OPEN.
A click on it opens or closes the drop-down."
  (propertize (if open "▼" "▶")
              'face 'agent-shell-observer-arrow
              'mouse-face 'highlight
              'keymap agent-shell-observer--arrow-map
              ;; Only the arrow itself, not what follows it.
              'rear-nonsticky t
              'help-echo (if open "mouse-1: hide the details" "mouse-1: show the details")))

(defun agent-shell-observer--session-cells (session)
  "Return SESSION's row: mark, state, last activity, agent, project, task.
The row is for the eye going down the list: what each session is doing
and when it last did something first, then whose and where, and what it
is about with the rest of the width.  The branch and the rest are in the
drop-down."
  (let* ((state (agent-shell-observer--session-state session))
         (shell (plist-get session :shell))
         (detail (and (buffer-live-p shell)
                      (buffer-local-value 'agent-shell-observer--shell-detail shell)))
         (cwd (agent-shell-observer--session-cwd session)))
    (vector (concat (agent-shell-observer--arrow (agent-shell-observer--open-p session))
                    " "
                    (if (eq state 'closed)
                        (propertize "○" 'face 'shadow
                                    'help-echo "The shell is closed; RET starts it again")
                      (propertize "●" 'face 'success 'help-echo "The shell is running")))
            (propertize (agent-shell-observer--state-label state)
                        'help-echo (if detail (format "%s: %s" state detail)
                                     (format "%s" state)))
            (propertize (agent-shell-observer--session-time session) 'face 'shadow)
            (agent-shell-observer--session-agent session)
            (propertize (file-name-nondirectory (directory-file-name cwd))
                        'help-echo (abbreviate-file-name cwd))
            (agent-shell-observer--session-task session))))

(defun agent-shell-observer--session-id (session)
  "Return the identifier of SESSION's agent session, or nil."
  (let ((shell (plist-get session :shell)))
    (or (and (buffer-live-p shell) (agent-shell-observer--session-id-of shell))
        (plist-get (car (plist-get session :entries)) :session-id))))

(defun agent-shell-observer--detail-lines (session)
  "Return the lines shown below SESSION's row when it is open.
Where it works on one line: path, branch and session; and what it waits
for, when it does, on a line of its own that stands out."
  (let* ((shell (plist-get session :shell))
         (state (agent-shell-observer--session-state session))
         (detail (and (buffer-live-p shell)
                      (buffer-local-value 'agent-shell-observer--shell-detail shell)))
         (id (agent-shell-observer--session-id session))
         (branch (agent-shell-observer--session-branch session))
         (dim (lambda (text) (propertize text 'face 'shadow))))
    (delq nil
          (list (concat "    "
                        (let ((cwd (agent-shell-observer--session-cwd session)))
                          (if (file-directory-p cwd)
                              (agent-shell-observer--link
                               (abbreviate-file-name cwd)
                               (lambda () (agent-shell-observer--open-file cwd))
                               (format "mouse-1: open %s" (abbreviate-file-name cwd)))
                            (abbreviate-file-name cwd)))
                        (unless (string-empty-p branch)
                          (concat "  " (funcall dim "branch ") branch))
                        (when id
                          (concat "  " (funcall dim "session ")
                                  (propertize (substring id 0 (min 8 (length id)))
                                              'help-echo id))))
                (when (eq state 'restoring)
                  (concat "    " (propertize "⟳ Bringing the conversation back…"
                                             'face 'warning)))
                (when (and detail (memq state '(waiting error idle)))
                  (concat "    "
                          (propertize (pcase state
                                        ('waiting (format "⚑ Waiting for %s" detail))
                                        ('error (format "Error: %s" detail))
                                        (_ (format "Stopped: %s" detail)))
                                      'face (if (eq state 'idle) 'shadow 'error))))))))

(defun agent-shell-observer--list-entries ()
  "Return the rows of the list, for `tabulated-list-entries'.
A session's row has the id (session . KEY), and the lines below it
\(detail KEY N)."
  (let (rows)
    (dolist (session (seq-filter #'agent-shell-observer--visible-p
                                 (agent-shell-observer--sessions)))
      (let ((key (plist-get session :key)))
        (push (list (cons 'session key) (agent-shell-observer--session-cells session)) rows)
        (when (agent-shell-observer--open-p session)
          (let ((n 0))
            (dolist (line (agent-shell-observer--detail-lines session))
              (push (list (list 'detail key (setq n (1+ n))) (vector line)) rows))))))
    (nreverse rows)))

(defun agent-shell-observer--print-row (id cols)
  "Insert the row ID with COLS: a session in columns, its details as lines.
For `tabulated-list-printer'."
  (if (eq (car-safe id) 'session)
      (tabulated-list-print-entry id cols)
    (let ((beg (point)))
      (insert (aref cols 0) "\n")
      (add-text-properties beg (point) `(tabulated-list-id ,id tabulated-list-entry ,cols)))))

(defun agent-shell-observer--session-at-point ()
  "Return the session of the list's current line, or signal."
  (let ((id (tabulated-list-get-id)))
    (or (and (memq (car-safe id) '(session detail))
             (agent-shell-observer--session-by-key
              (if (eq (car id) 'session) (cdr id) (nth 1 id))))
        (user-error "No session on this line"))))

(defun agent-shell-observer-list-toggle-session ()
  "Open or close the drop-down of the session on this line."
  (interactive)
  (let ((session (agent-shell-observer--session-at-point)))
    (agent-shell-observer--set-open session (not (agent-shell-observer--open-p session)))
    (agent-shell-observer--refresh-list)
    ;; Stay on the session's row.
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (equal (tabulated-list-get-id) (cons 'session (plist-get session :key)))))
      (forward-line 1))))

;;;; Filter
;;
;; Two filters narrow the sessions, and both hold at once.  f goes through
;; every session, those waiting for you and those working.  / picks terms
;; with the minibuffer's completion: the agents, projects, branches and
;; states of the sessions, with how many have each, or any text.  A
;; session stays when it has every term, in any case, in those or in its
;; questions, and the list narrows as they are typed.  An empty / clears
;; them.  The header names both filters.

(defconst agent-shell-observer--filters
  '((nil . "all")
    (waiting . "waiting for you")
    (working . "working"))
  "What f shows, in the order it goes through them, as (FILTER . NAME).")

(defvar agent-shell-observer--filter nil
  "The filter of `agent-shell-observer--filters' the list shows, nil for all.")

(defvar agent-shell-observer--filter-terms nil
  "Terms every session of the list has to have, or nil for any.")

(defvar crm-separator)

(defun agent-shell-observer--session-terms (session)
  "Return the terms / offers for SESSION, as (TERM . KIND)."
  (list (cons (agent-shell-observer--session-agent session) "agent")
        (cons (file-name-nondirectory
               (directory-file-name (agent-shell-observer--session-cwd session)))
              "project")
        (cons (agent-shell-observer--session-branch session) "branch")
        (cons (substring-no-properties
               (agent-shell-observer--state-label (agent-shell-observer--session-state session)))
              "state")))

(defun agent-shell-observer--state-visible-p (session)
  "Return non-nil when the filter of f lets SESSION through."
  (or (null agent-shell-observer--filter)
      (eq (agent-shell-observer--session-state session) agent-shell-observer--filter)))

(defun agent-shell-observer--visible-p (session)
  "Return non-nil when both filters of the list let SESSION through."
  (and (agent-shell-observer--state-visible-p session)
       (or (null agent-shell-observer--filter-terms)
           (let ((text (string-join
                        (append (list (agent-shell-observer--session-task session))
                                (mapcar #'car (agent-shell-observer--session-terms session))
                                (mapcan (lambda (entry)
                                          (list (or (plist-get entry :title) "")
                                                (or (plist-get entry :location) "")))
                                        (plist-get session :entries)))
                        " "))
                 (case-fold-search t))
             (seq-every-p (lambda (term) (string-match-p (regexp-quote term) text))
                          agent-shell-observer--filter-terms)))))

(defun agent-shell-observer--filter-label ()
  "Return what the header says of the filters, or nil with none on."
  (let ((labels (delq nil
                      (list (when agent-shell-observer--filter
                              (format "[%s]" (alist-get agent-shell-observer--filter
                                                        agent-shell-observer--filters)))
                            (when agent-shell-observer--filter-terms
                              (format "[/%s]" (string-join agent-shell-observer--filter-terms
                                                           ", ")))))))
    (when labels
      (propertize (string-join labels " ") 'face 'warning))))

(defun agent-shell-observer--report-filter ()
  "Say in the echo area how many sessions the filters show."
  (let ((sessions (agent-shell-observer--sessions)))
    (message "agent-observer: %s (%d of %d)"
             (or (agent-shell-observer--filter-label) "all")
             (seq-count #'agent-shell-observer--visible-p sessions)
             (length sessions))))

(defun agent-shell-observer-list-cycle-filter ()
  "Show the next of every session, those waiting for you and those working."
  (interactive)
  (let ((next (or (cadr (memq (assq agent-shell-observer--filter agent-shell-observer--filters)
                              agent-shell-observer--filters))
                  (car agent-shell-observer--filters))))
    (setq agent-shell-observer--filter (car next))
    (agent-shell-observer--refresh-list)
    (agent-shell-observer--report-filter)))

(defun agent-shell-observer--filter-candidates ()
  "Return the terms / offers, as (TERM KIND COUNT), most sessions first."
  (let ((counts (make-hash-table :test #'equal))
        (kinds (make-hash-table :test #'equal)))
    (dolist (session (seq-filter #'agent-shell-observer--state-visible-p
                                 (agent-shell-observer--sessions)))
      (dolist (term (agent-shell-observer--session-terms session))
        (unless (string-empty-p (car term))
          (puthash (car term) (1+ (gethash (car term) counts 0)) counts)
          (puthash (car term) (cdr term) kinds))))
    (let (candidates)
      (maphash (lambda (term count)
                 (push (list term (gethash term kinds) count) candidates))
               counts)
      (sort candidates (lambda (a b) (> (nth 2 a) (nth 2 b)))))))

(defun agent-shell-observer--read-terms (contents)
  "Return the terms written in CONTENTS, the minibuffer's text."
  (seq-remove #'string-empty-p
              (mapcar #'string-trim (split-string contents crm-separator t))))

(defun agent-shell-observer--filter-as-typed (&rest _)
  "Narrow the list to the terms typed so far in the minibuffer."
  (setq agent-shell-observer--filter-terms
        (agent-shell-observer--read-terms (minibuffer-contents-no-properties)))
  (agent-shell-observer--refresh-list))

(defun agent-shell-observer-list-filter ()
  "Keep only the sessions with every term picked, narrowing as they are typed.
Terms are picked with completion from the agents, projects, branches and
states of the sessions, or typed as any text, separated by commas.  None
shows every session again; \\[keyboard-quit] keeps the filter there was."
  (interactive)
  (require 'crm)
  (let* ((before agent-shell-observer--filter-terms)
         (candidates (agent-shell-observer--filter-candidates))
         (width (apply #'max 0 (mapcar (lambda (c) (string-width (car c))) candidates)))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      `(metadata
                        (display-sort-function . identity)
                        (annotation-function
                         . ,(lambda (term)
                              (when-let* ((candidate (assoc term candidates)))
                                (concat (make-string (- (+ width 2) (string-width term)) ?\s)
                                        (propertize (format "%-8s %d" (nth 1 candidate)
                                                            (nth 2 candidate))
                                                    'face 'completions-annotations))))))
                    (complete-with-action action (mapcar #'car candidates)
                                          string predicate)))))
    (condition-case nil
        (setq agent-shell-observer--filter-terms
              (agent-shell-observer--read-terms
               (string-join
                (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'after-change-functions
                                #'agent-shell-observer--filter-as-typed nil t))
                  (completing-read-multiple
                   "Filter (comma between terms, empty for all): " table nil nil
                   (when before (concat (string-join before ",") ","))))
                ",")))
      (quit (setq agent-shell-observer--filter-terms before)))
    (agent-shell-observer--refresh-list)
    (agent-shell-observer--report-filter)))

(defun agent-shell-observer-list-refresh ()
  "Read the branches again and redraw the list."
  (interactive)
  (clrhash agent-shell-observer--branches)
  (agent-shell-observer--refresh-list))

(defun agent-shell-observer--follow ()
  "Show the shell of the question point has moved to in the list."
  (when-let* ((entry (agent-shell-observer--entry (tabulated-list-get-id))))
    (unless (eq entry agent-shell-observer--shown)
      (agent-shell-observer--show entry))))

(defvar agent-shell-session-restore-verbosity)

(defvar-local agent-shell-observer--restoring nil
  "Entries being restored in this shell, paired with their prior statuses.
They never enter the prompt queue or the failure retry path.")

(defun agent-shell-observer--bound-restore-messages (original state notification)
  "Retain only recent messages from NOTIFICATION while restoring STATE.
Use ORIGINAL unchanged for shells not restored by this package.  The
accumulator uses agent-shell's newest-first turn/notification layout,
but groups by message so `full' replay renders at most the configured
number of messages without accumulating the entire conversation."
  (let* ((buffer (map-elt state :buffer))
         (pending (map-elt state :pending-restore)))
    (if (not (and pending (buffer-live-p buffer)
                  (buffer-local-value 'agent-shell-observer--restoring buffer)))
        (funcall original state notification)
      ;; map-put! updates existing alist cells; seed our additional cells once.
      (unless (assq :agent-shell-observer-message-count pending)
        (nconc pending (list (cons :agent-shell-observer-message-count 0)
                             (cons :agent-shell-observer-omitted-count 0)
                             (cons :agent-shell-observer-message-kind nil)
                             (cons :agent-shell-observer-message-id nil)
                             (cons :agent-shell-observer-last-kind nil))))
      (let* ((limit agent-shell-observer-restore-message-limit)
             (update (map-nested-elt notification '(params update)))
             (kind (map-elt update 'sessionUpdate))
             (id (map-elt update 'messageId))
             (message-p (member kind '("user_message_chunk" "agent_message_chunk")))
             (count (or (map-elt pending :agent-shell-observer-message-count) 0))
             (turns (map-elt pending :prompt-turns))
             (new-message
              (and message-p
                   (or (= count 0)
                       (not (equal kind (map-elt pending :agent-shell-observer-message-kind)))
                       (if id
                           (not (equal id (map-elt pending :agent-shell-observer-message-id)))
                         (not (equal kind (map-elt pending :agent-shell-observer-last-kind))))))))
        (unless (and (integerp limit) (> limit 0))
          (error "Agent chat restore message limit must be a positive integer"))
        (when new-message
          (when (> count 0) (push nil turns))
          (setq count (1+ count))
          (when (> count limit)
            (setcdr (nthcdr (1- limit) turns) nil)
            (setq count limit)
            (map-put! pending :agent-shell-observer-omitted-count
                      (1+ (or (map-elt pending :agent-shell-observer-omitted-count) 0)))))
        (unless turns (setq turns (list nil)))
        (push notification (car turns))
        (map-put! pending :prompt-turns turns)
        (map-put! pending :agent-shell-observer-message-count count)
        (map-put! pending :agent-shell-observer-last-kind kind)
        (when message-p
          (map-put! pending :agent-shell-observer-message-kind kind)
          (map-put! pending :agent-shell-observer-message-id id))))))

(defun agent-shell-observer--show-restore-limit (state)
  "Show a visible notice when STATE's replay omitted older messages."
  (let ((omitted (map-nested-elt state '(:pending-restore :agent-shell-observer-omitted-count)))
        (count (map-nested-elt state '(:pending-restore :agent-shell-observer-message-count))))
    (when (and omitted (> omitted 0))
      (agent-shell--update-bootstrapping-fragment
       :state state :block-id "agent_shell_observer_recent_history"
       :body (format "Showing the latest %d messages; %d older messages omitted."
                     count omitted)))))

(defun agent-shell-observer--ensure-sendable (&optional shell)
  "Reject new submissions to SHELL while its history is being restored."
  (with-current-buffer (or shell (current-buffer))
    (when agent-shell-observer--restoring
      (user-error "History is still restoring; send your question after restoration finishes"))))

(defun agent-shell-observer--guard-submission (&rest args)
  "Guard shell submissions and queued prompts, using ARGS' shell if supplied."
  (agent-shell-observer--ensure-sendable (plist-get args :shell-buffer)))

(defun agent-shell-observer--guard-insert (&rest args)
  "Allow composing text, but reject insertion with submission during restore."
  (when (plist-get args :submit)
    (apply #'agent-shell-observer--guard-submission args)))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--append-restore-notification :around
              #'agent-shell-observer--bound-restore-messages)
  (advice-add 'agent-shell--render-pending-restore :before
              #'agent-shell-observer--show-restore-limit)
  ;; Cover direct C-c C-c, viewport sends, queued prompts, and the final send.
  ;; These guards only affect shells being restored by this package.
  (dolist (function '(shell-maker-submit agent-shell--send-command
                       agent-shell--prompt-queue-enqueue))
    (advice-add function :before #'agent-shell-observer--guard-submission))
  (dolist (function '(agent-shell-insert agent-shell--insert-to-shell-buffer))
    (advice-add function :before #'agent-shell-observer--guard-insert)))

(defun agent-shell-observer--finish-restore (&optional failure)
  "Finish displaying this shell's saved history, or report FAILURE.
Restore each entry's prior status on success.  Never submit a prompt."
  (let ((restoring agent-shell-observer--restoring)
        (id (map-nested-elt agent-shell--state '(:session :id))))
    (setq agent-shell-observer--restoring nil)
    (dolist (saved restoring)
      (agent-shell-observer--set-status
       (car saved)
       (if (or failure (not (equal id (plist-get (car saved) :session-id))))
           "Error (restore)"
         (cdr saved))))
    (when restoring
      (message (if (or failure
                       (not (equal id (plist-get (caar restoring) :session-id))))
                   "History restoration failed; no prompt was sent"
                 "History restored: displaying previous messages")))))

(defun agent-shell-observer--revive (entry)
  "Bring back a shell for ENTRY, whose own has been closed.
Its agent opens its session again, replaying the conversation whole.
Without a session identifier, report an error.  Restoring history
never sends ENTRY's prompt or starts a retry on another agent."
  (require 'agent-shell)
  (let ((id (plist-get entry :session-id))
        (cwd (plist-get entry :cwd))
        (config (and (plist-get entry :agent)
                     (agent-shell--resolve-config-designator (plist-get entry :agent)))))
    (cond
     ((not (and (stringp id) (not (string-empty-p id))))
      (user-error "No saved session ID; cannot restore history. No prompt was sent"))
     ((when id
        (when-let* ((shell (seq-some
                           (lambda (other)
                             (and (equal id (plist-get other :session-id))
                                  (eq (plist-get entry :agent) (plist-get other :agent))
                                  (equal cwd (plist-get other :cwd))
                                  (buffer-live-p (plist-get other :shell))
                                  (plist-get other :shell)))
                           agent-shell-observer--entries)))
          ;; Several questions can belong to one conversation.
          (plist-put entry :shell shell)
          (with-current-buffer shell
            (when (and agent-shell-observer--restoring
                       (not (assq entry agent-shell-observer--restoring)))
              (push (cons entry (plist-get entry :status)) agent-shell-observer--restoring)
              (agent-shell-observer--set-status entry "Restoring…")))
          shell)))
     ((and id config)
      (let* ((status (plist-get entry :status))
             (shell (condition-case err
                        (progn
                          (agent-shell-observer--set-status entry "Restoring…")
                          (agent-shell--start :config config :no-focus t :new-session t
                                              :session-id id :cwd cwd))
                      (error
                       (agent-shell-observer--set-status entry "Error (restore)")
                       (signal (car err) (cdr err))))))
        ;; Read when the session is opened, in the shell's buffer.
        (with-current-buffer shell
          (setq-local agent-shell-session-restore-verbosity 'full
                      agent-shell-observer--restoring (list (cons entry status))))
        (plist-put entry :shell shell)
        (agent-shell-observer--watch shell)
        (message "Restoring %s history: displaying saved messages…"
                 (plist-get entry :agent))
        shell))
     (t (user-error "Agent configuration unavailable; cannot restore history. No prompt was sent")))))

(defun agent-shell-observer--open-entry (entry)
  "Show ENTRY's conversation below the list and go to its shell.
A shell that has been closed is brought back first."
  (unless (buffer-live-p (plist-get entry :shell))
    (agent-shell-observer--revive entry))
  (agent-shell-observer--show entry)
  (when-let* ((window (and (buffer-live-p (plist-get entry :shell))
                           (get-buffer-window (plist-get entry :shell)))))
    (select-window window)))

(defun agent-shell-observer--open-session (session)
  "Show SESSION below the list and go to it: its newest question, or its shell.
Its drop-down stays as it is."
  (if-let* ((entry (car (plist-get session :entries))))
      (agent-shell-observer--open-entry entry)
    (let ((shell (plist-get session :shell)))
      (unless (buffer-live-p shell)
        (user-error "This session has nothing to open"))
      (agent-shell-observer--remember-view)
      (setq agent-shell-observer--shown nil)
      (when-let* ((window (agent-shell-observer--display-shell shell)))
        (agent-shell-observer--arrange)
        (select-window window))
      (agent-shell-observer--refresh-list))))

(defun agent-shell-observer--go-shell (shell)
  "Show the list with SHELL's session beside it, and go to the shell."
  (agent-shell-observer--display-list)
  (when-let* ((session (seq-find (lambda (session) (eq (plist-get session :shell) shell))
                                 (agent-shell-observer--sessions))))
    (agent-shell-observer--open-session session))
  (when-let* ((window (get-buffer-window shell t)))
    (select-frame-set-input-focus (window-frame window))
    (select-window window)))

(defun agent-shell-observer-list-open ()
  "Open the conversation of the session on this line below the list.
Go to its shell; a shell that has been closed is brought back first."
  (interactive)
  (agent-shell-observer--open-session (agent-shell-observer--session-at-point)))

(defvar-local agent-shell-observer--session-overlay nil
  "Overlay marking the session displayed below the list.")

(defface agent-shell-observer-open
  '((((class color) (background dark)) :background "#26364d" :weight bold :extend t)
    (((class color) (background light)) :background "#dce6f5" :weight bold :extend t)
    (t :inverse-video t))
  "Face of the session row whose shell is shown below the list."
  :group 'agent-shell-observer)

(defun agent-shell-observer--open-session-key ()
  "Return the key of the session shown below the list, or nil.
The shell in the window below decides; failing that, the question shown."
  (let ((shown (when-let* ((window (agent-shell-observer--shell-window)))
                 (window-buffer window))))
    (plist-get (or (and (buffer-live-p shown)
                        (seq-find (lambda (session) (eq (plist-get session :shell) shown))
                                  (agent-shell-observer--sessions)))
                   (and agent-shell-observer--shown
                        (agent-shell-observer--session-of agent-shell-observer--shown)))
               :key)))

(defun agent-shell-observer--mark-line (face help)
  "Return an overlay over this line, in FACE, with a bar in the fringe.
HELP is what the mouse says over it.  A bar, not a triangle, so it does
not read as a session's arrow."
  (let ((overlay (make-overlay (line-beginning-position) (line-beginning-position 2))))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'before-string
                 (propertize " " 'display '(left-fringe vertical-bar success)))
    (overlay-put overlay 'help-echo help)
    overlay))

(defun agent-shell-observer--mark-open-conversation ()
  "Mark the session shown below the list, whatever line point is on.
Its row stands out whole, with a bar in the fringe."
  (when-let* ((buffer (get-buffer agent-shell-observer-list-buffer)))
    (with-current-buffer buffer
      (when (overlayp agent-shell-observer--session-overlay)
        (delete-overlay agent-shell-observer--session-overlay))
      (setq agent-shell-observer--session-overlay nil)
      (when-let* ((key (agent-shell-observer--open-session-key)))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (equal (tabulated-list-get-id) (cons 'session key))
              (setq agent-shell-observer--session-overlay
                    (agent-shell-observer--mark-line 'agent-shell-observer-open
                                                     "Shown below the list")))
            (forward-line 1)))))))

(defun agent-shell-observer-list-click (event)
  "Move to the row clicked, and open nothing.
A double click opens it, with `agent-shell-observer-list-double-click';
a session's arrow opens and closes its drop-down instead, with
`agent-shell-observer-list-click-arrow'."
  (interactive "e")
  (mouse-set-point event))

(defun agent-shell-observer-list-double-click (event)
  "Open the conversation of the session double-clicked below the list."
  (interactive "e")
  (mouse-set-point event)
  (when (tabulated-list-get-id)
    (agent-shell-observer-list-open)))

(defun agent-shell-observer-list-click-arrow (event)
  "Open or close the drop-down of the session whose arrow is clicked."
  (interactive "e")
  (mouse-set-point event)
  (agent-shell-observer-list-toggle-session))

(defun agent-shell-observer--open-file (file)
  "Open FILE in a code window and return it, preserving conversation windows."
  (let* ((buffer (find-file-noselect file))
         (windows (seq-remove
                   (lambda (window)
                     (or (window-parameter window 'window-side)
                         (window-dedicated-p window)
                         (with-current-buffer (window-buffer window)
                           (or (derived-mode-p 'agent-shell-mode)
                               (equal (buffer-name) agent-shell-observer-list-buffer)))))
                   (window-list nil 'no-minibuf)))
         (window (or (seq-find (lambda (w) (eq (window-buffer w) buffer)) windows)
                     (car (sort windows (lambda (a b)
                                          (> (window-use-time a) (window-use-time b)))))
                     (display-buffer buffer '((display-buffer-pop-up-window)
                                              (inhibit-same-window . t))))))
    (unless (window-live-p window)
      (user-error "No code window available"))
    (set-window-buffer window buffer)
    (select-window window)
    window))

(defvar agent-shell-markdown-open-file-function)

(with-eval-after-load 'agent-shell-markdown
  (setq agent-shell-markdown-open-file-function #'agent-shell-observer--open-file))

(defun agent-shell-observer-list-delete ()
  "Take the session on this line off the list, after asking.
The list forgets what it kept of the session; the agent keeps its own
sessions, and a shell still open stays in the list."
  (interactive)
  (let ((entries
         (let ((entries (plist-get (agent-shell-observer--session-at-point) :entries)))
           (unless entries
             (user-error "The list keeps nothing of this session to take off"))
           (when (y-or-n-p "Take this session off the list? ")
             entries))))
    (when entries
      (setq agent-shell-observer--entries
            (seq-difference agent-shell-observer--entries entries #'eq))
      (when (memq agent-shell-observer--shown entries)
        (setq agent-shell-observer--shown nil))
      (agent-shell-observer--save-history)
      (agent-shell-observer--refresh-list)
      (agent-shell-observer--follow))))

(defun agent-shell-observer--wide-p ()
  "Return non-nil when the list and conversation go side by side."
  (memq agent-shell-observer-layout '(right left)))

(defun agent-shell-observer--shell-window ()
  "Return the window beside the list that shows a question's shell, if any."
  (seq-find (lambda (window)
              (or (eq (window-parameter window 'agent-shell-observer-role) 'shell)
                  (and (eq (window-parameter window 'window-side) 'right)
                       (eq (window-parameter window 'window-slot) 2))))
            (window-list nil 'no-minibuf)))

(defun agent-shell-observer--display-list ()
  "Show the list where `agent-shell-observer-layout' puts it.
Return its window."
  (let ((buffer (agent-shell-observer--list-buffer)))
    (or (get-buffer-window buffer)
        (if (not (agent-shell-observer--wide-p))
            (display-buffer buffer agent-shell-observer-list-display-action)
          (when-let* ((window (let ((window-combination-resize nil))
                                ;; Taken from the windows beside it only, so
                                ;; the code keeps the rest of the frame.
                                (display-buffer
                                 buffer
                                 `((display-buffer-in-direction)
                                   (direction . rightmost)
                                   (window-width . ,agent-shell-observer-wide-width)
                                   (dedicated . agent-shell-observer))))))
            (set-window-parameter window 'agent-shell-observer-role 'list)
            window)))))

(defun agent-shell-observer--display-shell (shell)
  "Show SHELL where `agent-shell-observer-layout' puts the conversation.
Return its window.  Side by side, the list's window is split for it."
  (if (not (agent-shell-observer--wide-p))
      (display-buffer shell agent-shell-observer-shell-display-action)
    (when-let* ((window
                 (or (agent-shell-observer--shell-window)
                     (when-let* ((list (agent-shell-observer--display-list))
                                 (new (ignore-errors
                                        ;; Only the list gives up its width.
                                        (let ((window-combination-resize nil))
                                          (split-window
                                           list
                                           (- (round (* (window-total-width list)
                                                        (- 1 agent-shell-observer-wide-list-share))))
                                           agent-shell-observer-layout)))))
                       (set-window-parameter new 'agent-shell-observer-role 'shell)
                       new))))
      ;; Softly dedicated: other buffers go elsewhere, shells come here.
      (set-window-dedicated-p window nil)
      (set-window-buffer window shell)
      (set-window-dedicated-p window 'agent-shell-observer)
      window)))

(defun agent-shell-observer--panel-windows ()
  "Return conversation and list windows on the current frame."
  (seq-filter
   (lambda (window)
     (or (eq window (agent-shell-observer--shell-window))
         (with-current-buffer (window-buffer window)
           (or (equal (buffer-name) agent-shell-observer-list-buffer)
               (derived-mode-p 'agent-shell-mode)))))
   (window-list nil 'no-minibuf)))

(defun agent-shell-observer-hide ()
  "Hide the conversation list and conversation windows together."
  (interactive)
  (agent-shell-observer--remember-view)
  (dolist (window (agent-shell-observer--panel-windows))
    (when (window-live-p window)
      (if (or (window-parameter window 'window-side)
              (window-parameter window 'agent-shell-observer-role))
          (ignore-errors (delete-window window))
        (quit-restore-window window 'bury)))))

(defun agent-shell-observer-list-cycle-layout ()
  "Put the conversation below the list, then to its right, then to its left."
  (interactive)
  (let ((shell (when-let* ((window (agent-shell-observer--shell-window)))
                 (window-buffer window))))
    (setq agent-shell-observer-layout
          (pcase agent-shell-observer-layout ('below 'right) ('right 'left) (_ 'below)))
    (agent-shell-observer-hide)
    (agent-shell-observer-toggle)
    ;; A shell shown with no question of the list comes back too.
    (when (and (buffer-live-p shell) (not agent-shell-observer--shown))
      (agent-shell-observer--display-shell shell)
      (agent-shell-observer--arrange))
    (when-let* ((window (get-buffer-window agent-shell-observer-list-buffer)))
      (select-window window))
    (message "agent-observer: the conversation %s the list"
             (pcase agent-shell-observer-layout
               ('below "below") ('right "right of") (_ "left of")))))

(defun agent-shell-observer-list-scroll-left ()
  "Move the history viewport eight columns toward the start of its rows."
  (interactive)
  (scroll-right 8 t))

(defun agent-shell-observer-list-scroll-right ()
  "Move the history viewport eight columns toward the end of its rows."
  (interactive)
  (scroll-left 8 t))

(defun agent-shell-observer--list-header ()
  "Return column labels clipped to the horizontal offset of this window."
  (let ((header (make-string tabulated-list-padding ?\s))
        (window (if (and (boundp 'mode-line-window) (window-live-p mode-line-window))
                    mode-line-window (selected-window))))
    (dotimes (index (length tabulated-list-format))
      (let* ((column (aref tabulated-list-format index))
             (label (car column))
             (width (nth 1 column)))
        (setq header
              (concat header label
                      (unless (= index (1- (length tabulated-list-format)))
                        (make-string (max 1 (1+ (- width (string-width label)))) ?\s))))))
    (truncate-string-to-width header (string-width header) (window-hscroll window))))

(defun agent-shell-observer--list-header-inset ()
  "Align the header with the text area past this window's left fringe."
  (let* ((window (if (and (boundp 'mode-line-window) (window-live-p mode-line-window))
                     mode-line-window (selected-window)))
         (pixels (car (window-fringes window))))
    (propertize " " 'display `(space :width (,pixels)))))

(defun agent-shell-observer--init-list-header ()
  "Keep a fixed header row whose columns follow this window's scrolling."
  (setq header-line-format '("" (:eval (agent-shell-observer--list-header-inset))
                            header-line-indent (:eval (agent-shell-observer--list-header))
                            (:eval (when-let* ((label (agent-shell-observer--filter-label)))
                                     (concat "  " label))))))

(defvar-keymap agent-shell-observer-list-mode-map
  :parent tabulated-list-mode-map
  "RET" #'agent-shell-observer-list-open
  "<mouse-1>" #'agent-shell-observer-list-click
  "<double-mouse-1>" #'agent-shell-observer-list-double-click
  "<triple-mouse-1>" #'agent-shell-observer-list-click
  "TAB" #'agent-shell-observer-list-toggle-session
  "<tab>" #'agent-shell-observer-list-toggle-session
  "D" #'agent-shell-observer-list-delete
  "f" #'agent-shell-observer-list-cycle-filter
  "L" #'agent-shell-observer-list-cycle-layout
  "/" #'agent-shell-observer-list-filter
  "g" #'agent-shell-observer-list-refresh
  "C-<left>" #'agent-shell-observer-list-scroll-left
  "C-<right>" #'agent-shell-observer-list-scroll-right
  "<wheel-left>" #'agent-shell-observer-list-scroll-left
  "<wheel-right>" #'agent-shell-observer-list-scroll-right
  "S-<wheel-up>" #'agent-shell-observer-list-scroll-left
  "S-<wheel-down>" #'agent-shell-observer-list-scroll-right
  "q" #'agent-shell-observer-hide)

(define-derived-mode agent-shell-observer-list-mode tabulated-list-mode "Agent-Observer"
  "Agent sessions, the one active last first, each with its questions.
TAB, or a click on its arrow, opens or closes a session's details.  RET,
or a double click elsewhere on the line, opens the conversation of the
line below the list: a question's, or a session's newest one.  A single
click only moves to the line.

\\{agent-shell-observer-list-mode-map}"
  (setq tabulated-list-format [("" 3 nil)
                               ("State" 9 nil)
                               ("Last" 5 nil)
                               ("Agent" 11 nil)
                               ("Project" 18 nil)
                               ("Task" 0 nil)])
  (setq tabulated-list-entries #'agent-shell-observer--list-entries)
  (setq tabulated-list-printer #'agent-shell-observer--print-row)
  (setq tabulated-list-padding 0)
  (setq-local truncate-lines t
              horizontal-scroll-bar 'bottom)
  (agent-shell-observer--init-list-header)
  (hl-line-mode 1))

(defun agent-shell-observer--list-buffer ()
  "Return the list buffer, creating it when needed."
  (or (get-buffer agent-shell-observer-list-buffer)
      (with-current-buffer (get-buffer-create agent-shell-observer-list-buffer)
        (agent-shell-observer-list-mode)
        (setq buffer-read-only t)
        ;; One line a question, however narrow the side.
        (setq truncate-lines t)
        ;; Meow's normal state takes RET for itself; motion leaves it to
        ;; the list.
        (when (fboundp 'meow-motion-mode)
          (meow-motion-mode 1))
        (current-buffer))))

(defun agent-shell-observer--refresh-list ()
  "Redraw the list, keeping each window on the question it was on.
Questions without a title get one written."
  (agent-shell-observer--queue-titles)
  (when-let* ((buffer (get-buffer agent-shell-observer-list-buffer)))
    (with-current-buffer buffer
      (let ((windows (mapcar (lambda (window)
                               (cons window (with-selected-window window
                                              (tabulated-list-get-id))))
                             (get-buffer-window-list buffer nil t))))
        (setq-local horizontal-scroll-bar 'bottom)
        (tabulated-list-print t)
        (agent-shell-observer--mark-open-conversation)
        ;; Point and start are set on the windows themselves: moving the
        ;; buffer's point leaves a window that sat past the old end
        ;; starting there, with the rows out of sight above it.
        (dolist (window windows)
          (set-window-scroll-bars (car window) nil t nil 'bottom)
          (let ((row (save-excursion
                       (goto-char (point-min))
                       (while (and (not (eobp))
                                   (not (equal (tabulated-list-get-id) (cdr window))))
                         (forward-line 1))
                       (if (eobp) (point-min) (point)))))
            (set-window-point (car window) row)
            (unless (pos-visible-in-window-p row (car window))
              (set-window-start (car window) (point-min)))
            (when (>= (window-start (car window)) (point-max))
              (set-window-start (car window) (point-min)))))))))

(defun agent-shell-observer--arrange ()
  "Give the list `agent-shell-observer-list-share' of the side, the shell the rest.
Side by side, the split has made the widths already."
  (let ((list (get-buffer-window agent-shell-observer-list-buffer))
        (shell (agent-shell-observer--shell-window)))
    (when (and list shell (not (agent-shell-observer--wide-p)))
      (let ((target (round (* agent-shell-observer-list-share
                              (+ (window-total-height list) (window-total-height shell))))))
        (ignore-errors
          (window-resize list (- target (window-total-height list))))))))

(defun agent-shell-observer--remember-view ()
  "Remember the visible question's scroll and cursor positions.
Views belong to entries, since several questions may share a shell.
Markers follow edits to the shell, and are never written to history."
  (when-let* ((entry agent-shell-observer--shown)
              (shell (plist-get entry :shell))
              (window (agent-shell-observer--shell-window))
              ((eq (window-buffer window) shell)))
    (with-current-buffer shell
      (plist-put entry :view
                 (list :buffer shell
                       :start (copy-marker (window-start window))
                       :point (copy-marker (window-point window))
                       :hscroll (window-hscroll window)
                       :vscroll (window-vscroll window t))))))

(defun agent-shell-observer--show (entry &optional replayed)
  "Show ENTRY's shell, keeping its last scroll and cursor positions.
The first visit starts at the question.  REPLAYED means a newly loaded
conversation has finished replaying, so its question can now be found."
  (unless (and replayed (eq entry agent-shell-observer--shown))
    (agent-shell-observer--remember-view))
  (when replayed (plist-put entry :view nil))
  (setq agent-shell-observer--shown entry)
  (let* ((shell (plist-get entry :shell))
         (view (plist-get entry :view)))
    (when (buffer-live-p shell)
      (when-let* ((window (agent-shell-observer--display-shell shell)))
        ;; Resize first: resizing afterwards can change the restored position.
        (agent-shell-observer--arrange)
        (if (and (eq (plist-get view :buffer) shell)
                 (marker-buffer (plist-get view :start))
                 (marker-buffer (plist-get view :point)))
            (progn
              (set-window-point window (plist-get view :point))
              (set-window-start window (plist-get view :start) t)
              (set-window-hscroll window (plist-get view :hscroll))
              (set-window-vscroll window (plist-get view :vscroll) t))
          ;; The prompt names the code it is about; the last time it does
          ;; is this question, follow-ups and all after it.  The window
          ;; starts at the question above that name.
          (when-let* ((at (with-current-buffer shell
                            (save-excursion
                              (goto-char (point-max))
                              (when (search-backward (plist-get entry :anchor) nil t)
                                (search-backward (car (split-string (plist-get entry :prompt) "\n"))
                                                 nil t)
                                (line-beginning-position))))))
            (set-window-start window at)
            (set-window-point window at)
            (set-window-hscroll window 0)
            (set-window-vscroll window 0 t))))))
  (agent-shell-observer--mark-open-conversation))

(defun agent-shell-observer--set-status (entry status)
  "Give ENTRY the status STATUS, and show it in the list."
  (plist-put entry :status status)
  (agent-shell-observer--save-history)
  (agent-shell-observer--refresh-list))

(defun agent-shell-observer--add-entry (cwd prompt props)
  "Put PROMPT, to be sent in CWD, at the top of the list and return it.
PROPS is the plist `agent-shell-observer-submit' takes."
  (let* ((project (file-name-nondirectory (directory-file-name cwd)))
         (location (plist-get props :location))
         (entry (list :id (setq agent-shell-observer--next-id (1+ agent-shell-observer--next-id))
                      :time (format-time-string "%H:%M")
                      :cwd cwd
                      ;; Prompts from every project share the list.
                      :location (cond ((null location) project)
                                      ((file-name-absolute-p location) location)
                                      (t (concat project "/" location)))
                      :anchor location
                      :title (plist-get props :title)
                      :prompt prompt
                      :status "Waiting" :agent nil :shell nil :session-id nil
                      :failed nil :tried nil))
         (list (agent-shell-observer--list-buffer)))
    (push entry agent-shell-observer--entries)
    (agent-shell-observer--save-history)
    (when-let* ((window (agent-shell-observer--display-list)))
      (with-current-buffer list
        (tabulated-list-print t))
      (set-window-start window (with-current-buffer list (point-min)))
      (set-window-point window (with-current-buffer list (point-min))))
    entry))

;;; Following the shells
;;
;; A shell's events say nothing of which question they answer, so the
;; questions sent wait in the shell's `agent-shell-observer--pending' until
;; an `input-submitted' carries one of them; the events after that, until
;; `turn-complete', keep its status.  Prompts typed in a shell that has
;; questions of the list match nothing and leave the list alone; the
;; first one typed in a shell started on its own, with `agent-shell', is
;; a question of its own, so every conversation shows in the list.

;;;; What each shell is doing
;;
;; Apart from its questions, each shell keeps what it is doing, from its
;; own events: `working' from a prompt until the turn ends, `waiting'
;; while a permission or a question is up, `idle' after, `error' after a
;; failed request, and `starting' until its session is up.  A conversation
;; being replayed into a shell says nothing new, so its events change
;; nothing.  These make the sessions' rows, the mode line and the desktop
;; notifications.

(defvar-local agent-shell-observer--shell-status 'starting
  "What this shell is doing: starting, working, waiting, idle or error.")

(defvar-local agent-shell-observer--shell-detail nil
  "What `agent-shell-observer--shell-status' is about.
Such as the permission asked.")

(defvar-local agent-shell-observer--shell-activity nil
  "When this shell last did something, as a float time.")

(defvar-local agent-shell-observer--shell-title nil
  "The title the agent gave the session, unless it is only its id.")

(defvar-local agent-shell-observer--shell-task nil
  "The first line of the last prompt.")

(defun agent-shell-observer--shells ()
  "Return the agent shells followed or open."
  (seq-filter (lambda (buffer)
                (or (local-variable-p 'agent-shell-observer--subscription buffer)
                    (eq (buffer-local-value 'major-mode buffer) 'agent-shell-mode)))
              (buffer-list)))

(defun agent-shell-observer--first-line (text)
  "Return the first line of TEXT, or nil when it is blank."
  (when (and (stringp text) (not (string-blank-p text)))
    (car (split-string (string-trim text) "\n"))))

(defun agent-shell-observer--shell-set (status &optional detail quiet)
  "Make this shell's state STATUS about DETAIL, and show it.
Coming to a new state notifies the desktop, unless QUIET."
  (let ((before agent-shell-observer--shell-status)
        (before-detail agent-shell-observer--shell-detail))
    (setq agent-shell-observer--shell-status status
          agent-shell-observer--shell-detail detail)
    (unless (and (eq before status) (equal before-detail detail))
      (agent-shell-observer--refresh-list)
      (agent-shell-observer--update-mode-line)
      (unless (or quiet (eq before status))
        (agent-shell-observer--notify
         (current-buffer)
         (pcase status
           ('waiting 'waiting)
           ('error 'error)
           ('idle (and (eq before 'working) 'done))))))))

(defun agent-shell-observer--track (event)
  "Keep what this shell is doing, from EVENT.  Called in the shell."
  (let ((data (map-elt event :data))
        (name (map-elt event :event)))
    (setq agent-shell-observer--shell-activity (float-time))
    (cond
     ((eq name 'session-restored)
      (agent-shell-observer--shell-set 'idle nil t))
     ;; Replayed history is no news.
     ((map-elt agent-shell--state :pending-restore))
     (t
      (pcase name
        ('input-submitted
         (when-let* ((line (agent-shell-observer--first-line (map-elt data :prompt))))
           (setq agent-shell-observer--shell-task line))
         (agent-shell-observer--shell-set 'working))
        ((or 'agent-message-chunk 'tool-call-update)
         ;; A permission or a question is only over once it is answered.
         (unless (memq agent-shell-observer--shell-status '(working waiting))
           (agent-shell-observer--shell-set 'working)))
        ('permission-request
         (agent-shell-observer--shell-set
          'waiting (or (agent-shell-observer--first-line
                        (map-nested-elt data '(:tool-call :title)))
                       "a permission")))
        ('elicitation-request
         (agent-shell-observer--shell-set 'waiting "a question"))
        ((or 'permission-response 'elicitation-response)
         (agent-shell-observer--shell-set 'working))
        ('turn-complete
         (let ((reason (map-elt data :stop-reason)))
           (agent-shell-observer--shell-set
            'idle (unless (member reason '(nil "end_turn")) (format "%s" reason)))))
        ('session-title-changed
         (setq agent-shell-observer--shell-title
               (agent-shell-observer--meaningful-title (map-elt data :title)))
         (agent-shell-observer--refresh-list))
        ((or 'init-finished 'prompt-ready)
         (when (eq agent-shell-observer--shell-status 'starting)
           (agent-shell-observer--shell-set 'idle nil t)))
        ('error
         (agent-shell-observer--shell-set 'error (or (map-elt data :message) "unknown"))))))))

;;;; Attention
;;
;; What the shells need shows with the list closed: the mode line counts
;; those waiting for you (⚑) and those working (▶), and a desktop
;; notification says when one comes to a state of
;; `agent-shell-observer-notify-states', unless its shell is the selected
;; window of a focused frame.  Clicking the notification opens the shell.

(defvar agent-shell-observer-notify-states '(waiting done error)
  "What sends a desktop notification when a shell comes to it.
`waiting' is a permission or a question for you, `done' a turn ended,
`error' a failed request.  Nil sends none.")

(defvar agent-shell-observer--notifications nil
  "Desktop notifications still up, as (SHELL-BUFFER . NOTIFICATION-ID).")

(defvar agent-shell-observer--mode-line-string nil
  "What the mode line shows of the shells, or nil when nothing does.")
(put 'agent-shell-observer--mode-line-string 'risky-local-variable t)

(defvar agent-shell-observer--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'agent-shell-observer-toggle)
    map)
  "Keys of the mode line's shell counts.")

(defun agent-shell-observer--update-mode-line ()
  "Count the shells waiting for you and those working."
  (let* ((states (mapcar (lambda (shell)
                           (and (agent-shell-observer--live-p shell)
                                (buffer-local-value 'agent-shell-observer--shell-status shell)))
                         (agent-shell-observer--shells)))
         (waiting (seq-count (lambda (state) (eq state 'waiting)) states))
         (working (seq-count (lambda (state) (eq state 'working)) states)))
    (setq agent-shell-observer--mode-line-string
          (when (> (+ waiting working) 0)
            (propertize
             (concat " "
                     (when (> waiting 0)
                       (propertize (format "⚑%d" waiting) 'face 'error))
                     (when (and (> waiting 0) (> working 0)) " ")
                     (when (> working 0)
                       (propertize (format "▶%d" working) 'face 'warning))
                     " ")
             'help-echo (format "Agent shells: %d waiting for you, %d working\nmouse-1: show them"
                                waiting working)
             'mouse-face 'mode-line-highlight
             'local-map agent-shell-observer--mode-line-map)))
    (force-mode-line-update t)))

(unless (memq 'agent-shell-observer--mode-line-string global-mode-string)
  (setq global-mode-string
        (append (or global-mode-string '("")) '(agent-shell-observer--mode-line-string))))

(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function notifications-notify "notifications")
(defvar agent-shell-prefer-viewport-interaction)

(defun agent-shell-observer--in-sight-p (shell)
  "Return non-nil when SHELL is the selected window of a focused frame."
  (and (eq (frame-focus-state) t)
       (memq (window-buffer (selected-window))
             (list shell
                   (and (bound-and-true-p agent-shell-prefer-viewport-interaction)
                        (fboundp 'agent-shell-viewport--buffer)
                        (ignore-errors (agent-shell-viewport--buffer
                                        :shell-buffer shell :existing-only t)))))))

(defun agent-shell-observer--notify (shell what)
  "Tell the desktop SHELL came to WHAT, unless it is in sight.
WHAT is one of `agent-shell-observer-notify-states', or nil for nothing.
A shell's later notification replaces its earlier one."
  (when (and what
             (memq what agent-shell-observer-notify-states)
             (not (agent-shell-observer--in-sight-p shell)))
    (let* ((detail (buffer-local-value 'agent-shell-observer--shell-detail shell))
           (title (format "%s %s" (agent-shell-observer--agent-of shell)
                          (pcase what
                            ('waiting (format "needs %s" (or detail "you")))
                            ('done "is done")
                            (_ "failed"))))
           (cwd (with-current-buffer shell
                  (or (ignore-errors (agent-shell-cwd)) default-directory)))
           (body (string-join
                  (delq nil (list (or (buffer-local-value 'agent-shell-observer--shell-title shell)
                                      (buffer-local-value 'agent-shell-observer--shell-task shell))
                                  (file-name-nondirectory (directory-file-name cwd))
                                  (and (eq what 'error) detail)))
                  "\n")))
      (condition-case nil
          (progn
            (require 'notifications)
            (setf (alist-get shell agent-shell-observer--notifications)
                  (notifications-notify
                   :title title :body body :app-name "Emacs"
                   :replaces-id (alist-get shell agent-shell-observer--notifications)
                   :actions '("default" "Open")
                   :on-action (lambda (_notification _key)
                                (when (buffer-live-p shell)
                                  (agent-shell-observer--go-shell shell)))
                   :on-close (lambda (_notification _reason)
                               (setf (alist-get shell agent-shell-observer--notifications
                                                nil t)
                                     nil)))))
        ;; No D-Bus or no notification server: the echo area still tells.
        (error (message "agent-observer: %s: %s" title body))))))

(defvar-local agent-shell-observer--pending nil
  "Questions sent to this shell that it has not started on, oldest first.")

(defvar-local agent-shell-observer--current nil
  "The question this shell is answering, or nil.")

(defvar-local agent-shell-observer--subscription nil
  "This shell's subscription to its events, once it has one.")

(defun agent-shell-observer--same-text-p (sent prompt)
  "Return non-nil when SENT carries PROMPT, whitespace aside."
  (let ((squeeze (lambda (text)
                   (replace-regexp-in-string "[ \t\n]+" " " (string-trim text)))))
    (string-search (funcall squeeze prompt) (funcall squeeze sent))))

(defvar agent-shell-observer-owned-functions nil
  "Functions telling whether a shell's prompts belong to something else.
Each is called with the shell buffer; when one returns non-nil, what is
typed there does not become a session row of its own, as for the shells
a workflow drives with prompts of its own.")

(defun agent-shell-observer--owned-p (shell-buffer)
  "Return non-nil when SHELL-BUFFER answers a question of the list.
Shells `agent-shell-observer-owned-functions' claim count as owned: their
prompts are their owner's."
  (or (buffer-local-value 'agent-shell-observer--pending shell-buffer)
      (buffer-local-value 'agent-shell-observer--current shell-buffer)
      (run-hook-with-args-until-success 'agent-shell-observer-owned-functions
                                        shell-buffer)
      (seq-some (lambda (entry) (eq (plist-get entry :shell) shell-buffer))
                agent-shell-observer--entries)))

(defun agent-shell-observer--add-shell-entry (shell-buffer prompt)
  "Put PROMPT, typed in SHELL-BUFFER, at the top of the list and return it.
It stands for the conversation of a shell started on its own; Where is
the shell's project, and the list is redrawn without being shown."
  (let* ((cwd (with-current-buffer shell-buffer
                (or (ignore-errors (agent-shell-cwd)) default-directory)))
         (first (car (split-string (string-trim prompt) "\n")))
         (entry (list :id (setq agent-shell-observer--next-id (1+ agent-shell-observer--next-id))
                      :time (format-time-string "%H:%M")
                      :cwd cwd
                      :location (file-name-nondirectory (directory-file-name cwd))
                      :anchor first
                      :title first
                      :prompt prompt
                      :status "Answering…"
                      :agent (agent-shell-observer--agent-of shell-buffer)
                      :shell shell-buffer
                      :session-id (with-current-buffer shell-buffer
                                    (map-nested-elt agent-shell--state '(:session :id)))
                      :failed nil :tried nil)))
    (push entry agent-shell-observer--entries)
    (agent-shell-observer--save-history)
    (agent-shell-observer--refresh-list)
    entry))

(defun agent-shell-observer--on-event (event)
  "Keep what the shell EVENT comes from is doing, and its questions' status.
Called in the shell buffer."
  (agent-shell-observer--track event)
  (let ((data (map-elt event :data))
        (current agent-shell-observer--current))
    (pcase (map-elt event :event)
      ('input-submitted
       ;; Queued prompts can go out merged into one, and are answered as
       ;; one.
       (let ((matched (seq-filter
                       (lambda (entry)
                         (agent-shell-observer--same-text-p
                          (or (map-elt data :prompt) "") (plist-get entry :prompt)))
                       agent-shell-observer--pending)))
         (setq agent-shell-observer--pending
               (seq-difference agent-shell-observer--pending matched #'eq))
         (setq agent-shell-observer--current
               (or (car matched)
                   ;; The first prompt of a shell started on its own.
                   (unless (or (agent-shell-observer--owned-p (current-buffer))
                               (string-blank-p (or (map-elt data :prompt) "")))
                     (agent-shell-observer--add-shell-entry
                      (current-buffer) (map-elt data :prompt)))))
         (dolist (entry matched)
           ;; To open the conversation again once this shell is closed.
           (plist-put entry :session-id
                      (map-nested-elt agent-shell--state '(:session :id)))
           (agent-shell-observer--set-status entry "Answering…"))))
      ('session-restored
       (agent-shell-observer--finish-restore)
       ;; The conversation is back: scroll to the question shown.
       (when (eq (plist-get agent-shell-observer--shown :shell) (current-buffer))
         (agent-shell-observer--show agent-shell-observer--shown t)))
      ('init-finished
       ;; Resume-only agents do not replay history or emit session-restored.
       (when (and agent-shell-observer--restoring
                  (not (map-elt agent-shell--state :supports-session-load)))
         (agent-shell-observer--finish-restore)))
      ;; Work on the answer, text or tools, but a permission request is
      ;; only over once it has been answered.
      ((or 'agent-message-chunk 'tool-call-update)
       (when (and current
                  (not (member (plist-get current :status) '("Answering…" "Permission"))))
         (agent-shell-observer--set-status current "Answering…")))
      ('permission-request
       (when current
         (agent-shell-observer--set-status current "Permission")))
      ('permission-response
       (when current
         (agent-shell-observer--set-status current "Answering…")))
      ('turn-complete
       (when current
         (let ((reason (map-elt data :stop-reason)))
           (agent-shell-observer--set-status
            current (if (member reason '(nil "end_turn"))
                        "Done"
                      (format "Stopped (%s)" reason))))
         (setq agent-shell-observer--current nil)))
      ('error
       (agent-shell-observer--finish-restore t)
       ;; The question being answered fails, or, when the session never
       ;; came up, every question still waiting for it.
       (let ((failed (cond (current (list current))
                           ((not (map-nested-elt agent-shell--state '(:session :id)))
                            (prog1 agent-shell-observer--pending
                              (setq agent-shell-observer--pending nil))))))
         (when failed
           (setq agent-shell-observer--current nil)
           (agent-shell-observer--fail (current-buffer) failed
                                   (or (map-elt data :message) "unknown")))))
      ('clean-up
       (agent-shell-observer--finish-restore t)
       ;; The session shows closed, every question of it.
       (run-at-time 0 nil #'agent-shell-observer--shell-gone)
       (dolist (entry (delq nil (cons current agent-shell-observer--pending)))
         (agent-shell-observer--set-status entry "Shell closed"))))))

(defvar agent-shell-observer-failed-functions nil
  "Functions told that a prompt of the list failed.
Each is called with the entry, the agent that failed it and why.  One
that returns non-nil has taken the entry on, as by sending it to another
shell with `agent-shell-observer-resubmit'; when none does, the entry
shows the error.")

(defun agent-shell-observer--fail (shell-buffer entries message)
  "Report ENTRIES failed by SHELL-BUFFER's agent with MESSAGE."
  (let ((agent (agent-shell-observer--agent-of shell-buffer)))
    ;; Out of the shell's event, which is no place to start another.
    (dolist (entry entries)
      (run-at-time 0 nil #'agent-shell-observer--failed entry agent message))))

(defun agent-shell-observer--failed (entry agent message)
  "Let `agent-shell-observer-failed-functions' take ENTRY, failed by AGENT.
MESSAGE says why; when no function takes it, ENTRY shows the error."
  (plist-put entry :failed (cons (format "%s failed: %s" agent message)
                                 (plist-get entry :failed)))
  (unless (run-hook-with-args-until-success 'agent-shell-observer-failed-functions
                                            entry agent message)
    (message "agent-observer: %s failed: %s" agent message)
    (agent-shell-observer--set-status entry "Error")))

(defun agent-shell-observer--check-started (entry shell-buffer)
  "Fail ENTRY if SHELL-BUFFER never started a session for it."
  (when (and (buffer-live-p shell-buffer)
             (with-current-buffer shell-buffer
               (and (memq entry agent-shell-observer--pending)
                    (not (map-nested-elt agent-shell--state '(:session :id))))))
    (with-current-buffer shell-buffer
      (setq agent-shell-observer--pending (delq entry agent-shell-observer--pending)))
    (agent-shell-observer--fail
     shell-buffer (list entry)
     (format "no session after %ds, is it signed in?" agent-shell-observer-start-seconds))))

(defun agent-shell-observer--dispatch (entry shell-buffer)
  "Send ENTRY's prompt to SHELL-BUFFER, and show the shell below the list."
  (agent-shell-observer--ensure-sendable shell-buffer)
  (let ((agent (agent-shell-observer--agent-of shell-buffer)))
    (plist-put entry :agent agent)
    (plist-put entry :shell shell-buffer)
    ;; A retry must never keep the previous agent's session identifier.
    (plist-put entry :session-id
               (with-current-buffer shell-buffer
                 (map-nested-elt agent-shell--state '(:session :id))))
    (plist-put entry :tried (cons agent (plist-get entry :tried))))
  (agent-shell-observer--set-status entry "Waiting")
  (agent-shell-observer--watch shell-buffer)
  ;; Waiting before it goes out: a submit can report back at once.
  (with-current-buffer shell-buffer
    (setq agent-shell-observer--pending
          (append agent-shell-observer--pending (list entry))))
  (run-at-time agent-shell-observer-start-seconds nil
               #'agent-shell-observer--check-started entry shell-buffer)
  (let ((prompt (plist-get entry :prompt)))
    (if (agent-shell--can-insert-into-prompt-p :shell-buffer shell-buffer)
        (agent-shell-insert :text prompt :submit t :no-focus t
                            :shell-buffer shell-buffer)
      (with-current-buffer shell-buffer
        (agent-shell-prompt-queue prompt))))
  (agent-shell-observer--show entry)
  entry)

(defun agent-shell-observer--shell-gone ()
  "Show the shells as they are now that one has gone."
  (agent-shell-observer--refresh-list)
  (agent-shell-observer--update-mode-line))

(defun agent-shell-observer--shell-killed ()
  "Show this shell's session closed once the buffer is gone.
For the shell's `kill-buffer-hook'; the buffer is still live while it runs."
  (setf (alist-get (current-buffer) agent-shell-observer--notifications nil t) nil)
  (run-at-time 0 nil #'agent-shell-observer--shell-gone))

(defun agent-shell-observer--watch (shell-buffer)
  "Have SHELL-BUFFER's events go to the list, once."
  (with-current-buffer shell-buffer
    (add-hook 'kill-buffer-hook #'agent-shell-observer--shell-killed nil t)
    (unless agent-shell-observer--subscription
      ;; A shell up before it is followed starts from what it shows.
      (setq agent-shell-observer--shell-activity (float-time)
            agent-shell-observer--shell-status
            (cond ((and (fboundp 'shell-maker-busy) (ignore-errors (shell-maker-busy)))
                   'working)
                  ((map-nested-elt agent-shell--state '(:session :id)) 'idle)
                  (t 'starting))
            agent-shell-observer--shell-title
            (agent-shell-observer--meaningful-title
             (map-nested-elt agent-shell--state '(:session :title))))
      (setq agent-shell-observer--subscription
            (agent-shell-subscribe-to :shell-buffer shell-buffer
                                      :on-event #'agent-shell-observer--on-event)))))

(defun agent-shell-observer-watch-shell ()
  "Have the current agent shell's conversation go to the list.
For `agent-shell-mode-hook', so shells started on their own show too."
  (agent-shell-observer--watch (current-buffer)))

;;; Commands

(defun agent-shell-observer-submit (shell-buffer prompt &rest props)
  "Send PROMPT to SHELL-BUFFER and follow it at the top of the list.
PROPS may give :title, what the session is called until a title is
written for it; and :location, where in the project the prompt is about,
as the prompt says it, such as \"src/foo.el:10-20\", for the shell to be
scrolled there when the session is opened.  The shell is shown below the
list.  Return the entry, for
`agent-shell-observer-resubmit'."
  (require 'agent-shell)
  (agent-shell-observer--ensure-sendable shell-buffer)
  (let ((cwd (with-current-buffer shell-buffer (agent-shell-cwd))))
    (agent-shell-observer--dispatch
     (agent-shell-observer--add-entry cwd prompt props) shell-buffer)))

(defun agent-shell-observer-resubmit (entry shell-buffer)
  "Send ENTRY's prompt again, to SHELL-BUFFER, as when its agent failed it.
Return ENTRY."
  (agent-shell-observer--dispatch entry shell-buffer))

(defun agent-shell-observer-toggle ()
  "Show or hide the list of sessions and the conversation beside it.
Where the conversation goes is `agent-shell-observer-layout'."
  (interactive)
  (if (agent-shell-observer--panel-windows)
      (agent-shell-observer-hide)
    (agent-shell-observer--display-list)
    (agent-shell-observer--refresh-list)
    (when-let* ((entry (or agent-shell-observer--shown (car agent-shell-observer--entries))))
      (agent-shell-observer--show entry))))

;; Re-evaluating the source must not discard questions already in memory.
(unless agent-shell-observer--entries
  (agent-shell-observer--load-history))

(provide 'agent-shell-observer)
;;; agent-shell-observer.el ends here
