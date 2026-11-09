;;; agent-shell-observer-test.el --- Asking agents about code -*- lexical-binding: t -*-

;; The shells are fakes: a buffer with the state an agent-shell keeps,
;; whose events the tests send themselves.

(require 'package)
(setq load-prefer-newer t)
(package-initialize)
(require 'ert)
(require 'cl-lib)
(require 'crm)
;; Loaded here, so the tests' own notifications-notify is not redefined.
(require 'notifications nil t)
(require 'agent-shell)
(add-to-list 'load-path
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defvar agent-shell-observer-history-file)
(let ((agent-shell-observer-history-file nil))
  (require 'agent-shell-observer))

(defvar agent-shell-observer-test--handlers nil
  "Each fake shell's event handler, as (SHELL . HANDLER).")

(defvar agent-shell-observer-test--shells nil
  "The fake shells open, newest first.")

(defvar agent-shell-observer-test--silent nil
  "Agents whose shells never start a session, like one not signed in.")

(defun agent-shell-observer-test--shell (agent cwd)
  "Return a new fake shell of AGENT in CWD."
  (let ((buffer (generate-new-buffer (format " *fake %s*" agent))))
    (with-current-buffer buffer
      (setq default-directory cwd)
      (setq-local agent-shell--state
                  `((:agent-config . ((:identifier . ,agent)))
                    (:session . ((:id . ,(unless (memq agent agent-shell-observer-test--silent)
                                           "session")))))))
    (push buffer agent-shell-observer-test--shells)
    buffer))

(defun agent-shell-observer-test--emit (shell event &rest data)
  "Have SHELL report EVENT with DATA."
  (with-current-buffer shell
    (funcall (alist-get shell agent-shell-observer-test--handlers)
             (list (cons :event event) (cons :data data)))))

(defun agent-shell-observer-test--rows ()
  "Return the question rows of the list, as lists of their columns.
Sessions are opened first, so every question is in the list."
  (with-current-buffer agent-shell-observer-list-buffer
    (dolist (session (agent-shell-observer--sessions))
      (agent-shell-observer--set-open session t))
    (agent-shell-observer--refresh-list)
    (let (rows)
      (goto-char (point-min))
      (while (not (eobp))
        (when (integerp (tabulated-list-get-id))
          (push (append (tabulated-list-get-entry) nil) rows))
        (forward-line 1))
      (nreverse rows))))

(defmacro agent-shell-observer-test-with-agents (&rest body)
  "Run BODY with fake shells for antigravity and codex in a fresh list."
  (declare (indent 0))
  `(let* ((project (file-name-as-directory (make-temp-file "agent-shell-observer-" t)))
          (file (expand-file-name "code.el" project))
          (agent-shell-observer-agents '(antigravity codex))
          (agent-shell-observer-start-seconds 0.2)
          (agent-shell-observer--entries nil)
          (agent-shell-observer--next-id 0)
          (agent-shell-observer-history-file (expand-file-name "history.eld" project))
          (agent-shell-observer--history-readable t)
          (agent-shell-observer--shown nil)
          (agent-shell-observer--rested nil)
          (agent-shell-observer--opened (make-hash-table :test #'equal))
          (agent-shell-observer--closed (make-hash-table :test #'equal))
          (agent-shell-observer--filter nil)
          (agent-shell-observer--filter-terms nil)
          (agent-shell-observer-notify-states nil)
          ;; No titles are written unless a test asks; none reach Ollama.
          (agent-shell-observer-summarize-titles nil)
          (agent-shell-observer--title-queue nil)
          (agent-shell-observer--title-busy nil)
          (agent-shell-observer--title-given-up nil)
          (agent-shell-observer--title-resume-at 0)
          (agent-shell-observer-test--handlers nil)
          (agent-shell-observer-test--shells nil)
          (agent-shell-observer-test--silent agent-shell-observer-test--silent))
     (with-temp-file file (insert "first line\nsecond line\nthird line\n"))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-cwd)
                    (lambda (&rest _) project))
                   ((symbol-function 'agent-shell-project-buffers)
                    (lambda (&rest _) agent-shell-observer-test--shells))
                   ((symbol-function 'agent-shell-observer--installed-p)
                    (lambda (agent) (memq agent '(antigravity codex))))
                   ((symbol-function 'agent-shell--resolve-config-designator)
                    (lambda (agent) `((:identifier . ,agent))))
                   ((symbol-function 'agent-shell--start)
                    (lambda (&rest args)
                      (agent-shell-observer-test--shell
                       (map-elt (plist-get args :config) :identifier) project)))
                   ((symbol-function 'agent-shell-subscribe-to)
                    (lambda (&rest args)
                      (push (cons (plist-get args :shell-buffer) (plist-get args :on-event))
                            agent-shell-observer-test--handlers)
                      'subscription))
                   ((symbol-function 'agent-shell--can-insert-into-prompt-p)
                    (lambda (&rest _) t))
                   ((symbol-function 'agent-shell-insert)
                    (lambda (&rest args)
                      (let ((shell (plist-get args :shell-buffer)))
                        (with-current-buffer shell
                          (goto-char (point-max))
                          (insert "\n> " (plist-get args :text) "\n"))
                        (unless (memq (agent-shell-observer--agent-of shell)
                                      agent-shell-observer-test--silent)
                          (agent-shell-observer-test--emit
                           shell 'input-submitted :prompt (plist-get args :text)))))))
           (with-current-buffer (find-file-noselect file)
             ,@body))
       (dolist (shell agent-shell-observer-test--shells) (kill-buffer shell))
       (when-let* ((buffer (find-buffer-visiting file)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (when (get-buffer agent-shell-observer-list-buffer)
         (kill-buffer agent-shell-observer-list-buffer))
       (delete-directory project t))))

(defun agent-shell-observer-test--ask (line &optional question)
  "Ask about LINE of the current buffer, with QUESTION if given."
  (goto-char (point-min))
  (forward-line (1- line))
  (agent-shell-observer--send (line-beginning-position) (line-end-position) question))

(defun agent-shell-observer-test--settle (seconds)
  "Let timers run for SECONDS."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end)
      (accept-process-output nil 0.02))))

(ert-deftest agent-shell-observer-prompt-says-where-the-code-is ()
  (agent-shell-observer-test-with-agents
    (goto-char (point-min))
    (forward-line 1)
    (let* ((context (agent-shell-observer--context
                     (line-beginning-position) (line-end-position) project))
           (prompt (agent-shell-observer--prompt "Why?" context)))
      (should (equal (plist-get context :location) "code.el:2"))
      (should (equal (plist-get context :line) 2))
      (should (string-prefix-p "Why?\n" prompt))
      (should (string-search agent-shell-observer-answer-style prompt))
      (should (string-search "code.el:2\n```emacs-lisp\nsecond line\n```" prompt)))))

(ert-deftest agent-shell-observer-region-spans-its-lines ()
  (agent-shell-observer-test-with-agents
    (let ((context (agent-shell-observer--context
                    (point-min) (save-excursion (goto-char (point-min)) (forward-line 2) (point))
                    project)))
      ;; A region ending at a line's start does not take that line.
      (should (equal (plist-get context :location) "code.el:1-2")))))

(ert-deftest agent-shell-observer-status-follows-the-shell ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((shell (car agent-shell-observer-test--shells))
          (status (lambda () (nth 1 (car (agent-shell-observer-test--rows))))))
      (should (equal (nth 2 (car (agent-shell-observer-test--rows))) "antigravity"))
      (should (equal (funcall status) "Running"))
      (agent-shell-observer-test--emit shell 'permission-request)
      (should (equal (funcall status) "Permission"))
      ;; The tool asking may report before it is answered.
      (agent-shell-observer-test--emit shell 'tool-call-update)
      (should (equal (funcall status) "Permission"))
      (agent-shell-observer-test--emit shell 'permission-response)
      (should (equal (funcall status) "Running"))
      ;; Tools running after a permission keep it answering, no text needed.
      (agent-shell-observer-test--emit shell 'permission-request)
      (agent-shell-observer-test--emit shell 'permission-response)
      (agent-shell-observer-test--emit shell 'tool-call-update)
      (should (equal (funcall status) "Running"))
      (agent-shell-observer-test--emit shell 'agent-message-chunk :text-chunk "It is")
      (should (equal (funcall status) "Running"))
      (agent-shell-observer-test--emit shell 'turn-complete :stop-reason "end_turn")
      (should (equal (funcall status) "Done")))))

(ert-deftest agent-shell-observer-follow-ups-in-the-shell-leave-the-list ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((shell (car agent-shell-observer-test--shells)))
      (agent-shell-observer-test--emit shell 'turn-complete :stop-reason "end_turn")
      (agent-shell-observer-test--emit shell 'input-submitted :prompt "typed in the shell")
      (agent-shell-observer-test--emit shell 'turn-complete :stop-reason "cancelled")
      (should (equal (agent-shell-observer-test--rows)
                     (list (list (nth 0 (car (agent-shell-observer-test--rows)))
                                 "Done" "antigravity"
                                 (concat (file-name-nondirectory (directory-file-name project))
                                         "/code.el:1")
                                 "")))))))

(ert-deftest agent-shell-observer-newest-question-on-top ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--ask 3 "What is the third line?")
    (let ((rows (agent-shell-observer-test--rows)))
      (should (= (length rows) 2))
      (should (equal (nth 4 (nth 0 rows)) "What is the third line?"))
      ;; The default question is left out of the list.
      (should (equal (nth 4 (nth 1 rows)) "")))))

(ert-deftest agent-shell-observer-failure-goes-to-the-next-agent ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((antigravity (car agent-shell-observer-test--shells)))
      (agent-shell-observer-test--emit antigravity 'error :message "usage limit")
      (agent-shell-observer-test--settle 0.05)
      (should (equal (nth 2 (car (agent-shell-observer-test--rows))) "codex"))
      (should (equal (plist-get (car agent-shell-observer--entries) :failed)
                     '("antigravity failed: usage limit")))
      ;; The failed agent rests: the next question skips it too.
      (agent-shell-observer-test--ask 2)
      (should (equal (nth 2 (car (agent-shell-observer-test--rows))) "codex")))))

(ert-deftest agent-shell-observer-no-agent-left-is-an-error ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--emit (car agent-shell-observer-test--shells) 'error :message "one")
    (agent-shell-observer-test--settle 0.05)
    (agent-shell-observer-test--emit (car agent-shell-observer-test--shells) 'error :message "two")
    (agent-shell-observer-test--settle 0.05)
    (should (equal (nth 1 (car (agent-shell-observer-test--rows))) "Error"))))

(ert-deftest agent-shell-observer-agent-that-never-starts-is-passed-over ()
  (let ((agent-shell-observer-test--silent '(antigravity)))
    (agent-shell-observer-test-with-agents
      (agent-shell-observer-test--ask 1)
      (should (equal (nth 1 (car (agent-shell-observer-test--rows))) "Waiting"))
      (agent-shell-observer-test--settle 0.4)
      (should (equal (nth 2 (car (agent-shell-observer-test--rows))) "codex"))
      (should (string-prefix-p "antigravity failed: no session after"
                               (car (plist-get (car agent-shell-observer--entries) :failed)))))))

(ert-deftest agent-shell-observer-unlisted-shells-are-not-asked ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--shell 'opencode project)
    (agent-shell-observer-test--ask 1)
    (should (equal (nth 2 (car (agent-shell-observer-test--rows))) "antigravity"))))

(ert-deftest agent-shell-observer-shells-started-on-their-own-join-the-list ()
  (agent-shell-observer-test-with-agents
    (let ((shell (agent-shell-observer-test--shell 'opencode project))
          (name (file-name-nondirectory (directory-file-name project))))
      (with-current-buffer shell (agent-shell-observer-watch-shell))
      (agent-shell-observer--list-buffer)
      (agent-shell-observer-test--emit shell 'input-submitted :prompt "Fix the build\nplease")
      (should (equal (cdr (car (agent-shell-observer-test--rows)))
                     (list "Running" "opencode" name "Fix the build")))
      (should (equal (plist-get (car agent-shell-observer--entries) :session-id) "session"))
      (agent-shell-observer-test--emit shell 'turn-complete :stop-reason "end_turn")
      (should (equal (nth 1 (car (agent-shell-observer-test--rows))) "Done"))
      ;; Follow-ups stay in the conversation's row.
      (agent-shell-observer-test--emit shell 'input-submitted :prompt "And the tests")
      (should (= (length (agent-shell-observer-test--rows)) 1)))))

(ert-deftest agent-shell-observer-owned-shells-stay-out-of-the-list ()
  (agent-shell-observer-test-with-agents
    (let* ((shell (agent-shell-observer-test--shell 'codex project))
           (agent-shell-observer-owned-functions
            (list (lambda (buffer) (eq buffer shell)))))
      (with-current-buffer shell
        (agent-shell-observer-watch-shell))
      (agent-shell-observer--list-buffer)
      (agent-shell-observer-test--emit shell 'input-submitted :prompt "Workflow step")
      (should (null agent-shell-observer--entries)))))

(ert-deftest agent-shell-observer-first-agent-is-asked-first ()
  (agent-shell-observer-test-with-agents
    (let (saved)
      (cl-letf (((symbol-function 'customize-save-variable)
                 (lambda (symbol value &rest _)
                   (push symbol saved)
                   (set symbol value))))
        (agent-shell-observer-set-first-agent 'codex)
        (should (equal agent-shell-observer-agents '(codex antigravity)))
        (should (equal saved '(agent-shell-observer-agents)))
        (agent-shell-observer-test--ask 1)
        (should (equal (nth 2 (car (agent-shell-observer-test--rows))) "codex"))
        ;; An agent not listed yet joins at the front.
        (agent-shell-observer-set-first-agent 'opencode)
        (should (equal agent-shell-observer-agents '(opencode codex antigravity)))))))

(ert-deftest agent-shell-observer-resting-forgets-old-failures ()
  (let ((agent-shell-observer-rest-seconds 60)
        (agent-shell-observer--rested (list (cons 'codex (float-time))
                                        (cons 'codex (- (float-time) 10))
                                        (cons 'antigravity (- (float-time) 120)))))
    (should (equal (agent-shell-observer--resting-agents) '(codex)))
    (should (= (length agent-shell-observer--rested) 2))))

;;; Closed shells

(ert-deftest agent-shell-observer-records-the-session ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (should (equal (plist-get (car agent-shell-observer--entries) :session-id) "session"))))

(ert-deftest agent-shell-observer-reopens-the-session-of-a-closed-shell ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((entry (car agent-shell-observer--entries))
          started)
      (kill-buffer (plist-get entry :shell))
      (setq agent-shell-observer-test--shells (seq-filter #'buffer-live-p agent-shell-observer-test--shells))
      (cl-letf (((symbol-function 'agent-shell--start)
                 (lambda (&rest args)
                   (setq started args)
                   (agent-shell-observer-test--shell
                    (map-elt (plist-get args :config) :identifier) project))))
        (with-current-buffer (agent-shell-observer--list-buffer)
          (goto-char (point-min))
          (agent-shell-observer-list-open)))
      (should (equal (plist-get started :session-id) "session"))
      (should (eq (map-elt (plist-get started :config) :identifier) 'antigravity))
      (should (buffer-live-p (plist-get entry :shell)))
      ;; The whole conversation comes back, in that shell only.
      (should (eq (buffer-local-value 'agent-shell-session-restore-verbosity
                                      (plist-get entry :shell))
                  'full))
      ;; Its events reach the list again.
      (should (alist-get (plist-get entry :shell) agent-shell-observer-test--handlers)))))

(defmacro agent-shell-observer-test-without-resending (&rest body)
  "Run BODY while failing on any prompt submission, retry, or confirmation."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-shell-insert)
              (lambda (&rest _) (ert-fail "History must not submit a prompt")))
             ((symbol-function 'agent-shell-prompt-queue)
              (lambda (&rest _) (ert-fail "History must not queue a prompt")))
             ((symbol-function 'agent-shell-observer--dispatch)
              (lambda (&rest _) (ert-fail "History must not dispatch a question")))
             ((symbol-function 'agent-shell-observer--retry)
              (lambda (&rest _) (ert-fail "History must not retry a question")))
             ((symbol-function 'agent-shell-observer--fail)
              (lambda (&rest _) (ert-fail "History must not switch agents")))
             ((symbol-function 'y-or-n-p)
              (lambda (&rest _) (ert-fail "History must not offer resubmission"))))
     ,@body))

(ert-deftest agent-shell-observer-history-without-session-never-resends ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((entry (car agent-shell-observer--entries)))
      (plist-put entry :session-id nil)
      (kill-buffer (plist-get entry :shell))
      (setq agent-shell-observer-test--shells nil)
      (agent-shell-observer-test-without-resending
        (with-current-buffer (agent-shell-observer--list-buffer)
          (goto-char (point-min))
          (should-error (agent-shell-observer-list-open) :type 'user-error)))
      (should-not (buffer-live-p (plist-get entry :shell)))
      (should-not agent-shell-observer-test--shells))))

(ert-deftest agent-shell-observer-history-shows-restoring-until-replay-completes ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--emit (car agent-shell-observer-test--shells)
                             'turn-complete :stop-reason "end_turn")
    (let ((entry (car agent-shell-observer--entries)))
      (kill-buffer (plist-get entry :shell))
      (setq agent-shell-observer-test--shells nil)
      (agent-shell-observer-test-without-resending
        (let ((shell (agent-shell-observer--revive entry)))
          (should (equal (nth 1 (car (agent-shell-observer-test--rows))) "Restoring"))
          (with-current-buffer shell
            (setf (alist-get :supports-session-load agent-shell--state) t))
          (agent-shell-observer-test--emit shell 'init-finished)
          (should (equal (plist-get entry :status) "Restoring…"))
          (agent-shell-observer-test--emit shell 'agent-message-chunk :text-chunk "Old answer")
          (should (equal (plist-get entry :status) "Restoring…"))
          (agent-shell-observer-test--emit shell 'session-restored)
          (should (equal (plist-get entry :status) "Done"))
          (with-current-buffer shell
            (should-not agent-shell-observer--restoring)
            (should-not agent-shell-observer--pending)))))))

(ert-deftest agent-shell-observer-history-errors-never-resend-or-retry ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((entry (car agent-shell-observer--entries)))
      (kill-buffer (plist-get entry :shell))
      (setq agent-shell-observer-test--shells nil)
      (agent-shell-observer-test-without-resending
        (let ((shell (agent-shell-observer--revive entry)))
          (agent-shell-observer-test--emit shell 'error :message "Session no longer exists")
          (agent-shell-observer-test--settle 0.05)
          (should (equal (plist-get entry :status) "Error (restore)"))
          (should (= (length agent-shell-observer-test--shells) 1))
          (should (equal (plist-get entry :session-id) "session")))))))

(ert-deftest agent-shell-observer-history-start-failure-never-resends ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((entry (car agent-shell-observer--entries)))
      (kill-buffer (plist-get entry :shell))
      (agent-shell-observer-test-without-resending
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest _) (error "Agent executable unavailable"))))
          (should-error (agent-shell-observer--revive entry))))
      (should (equal (plist-get entry :status) "Error (restore)")))))

(ert-deftest agent-shell-observer-history-rejects-another-session-on-restore ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((entry (car agent-shell-observer--entries)))
      (kill-buffer (plist-get entry :shell))
      (setq agent-shell-observer-test--shells nil)
      (agent-shell-observer-test-without-resending
        (let ((shell (agent-shell-observer--revive entry)))
          (with-current-buffer shell
            (setf (alist-get :id (alist-get :session agent-shell--state)) "different-session"))
          (agent-shell-observer-test--emit shell 'init-finished)
          (should (equal (plist-get entry :status) "Error (restore)")))))))

;;; Persistent questions

;;; User messages rendered in place

(defun agent-shell-observer-test--render (source)
  "Return SOURCE rendered in place in a buffer of its own."
  (with-temp-buffer
    (insert source)
    (agent-shell-observer--render-user-input (point-min) (point-max))
    (buffer-string)))

(ert-deftest agent-shell-observer-user-markdown-renders-committed-input-in-place ()
  (with-temp-buffer
    (insert "# Heading\n\n**bold** and `inline` and [link](https://example.com)\n\n- one\n- two\n\n| a | b |\n|---|---|\n| 1 | 2 |\n\n```emacs-lisp\n(defun hello () \"hello\")\n```\n")
    (add-text-properties (point-min) (point-max) '(read-only t front-sticky (read-only)))
    (agent-shell-observer--render-user-input (point-min) (point-max))
    (let ((text (buffer-string)))
      (should (string-search "Heading" text))
      (should-not (string-search "**bold**" text))
      (should-not (string-search "[link](" text))
      (should-not (string-search "```" text))
      (should (text-property-any (point-min) (point-max)
                                 'agent-shell-markdown-source-block-body t))
      (should (equal (get-text-property (1+ (string-search "link" text))
                                        'agent-shell-markdown-url)
                     "https://example.com"))
      ;; Still as immutable as when it was frozen.
      (should (get-text-property (point-min) 'read-only))
      (should (get-text-property (1- (point-max)) 'read-only))
      ;; A second pass, as history refreshes make, changes nothing.
      (agent-shell-observer--render-user-input (point-min) (point-max))
      (should (equal-including-properties text (buffer-string)))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest agent-shell-observer-user-markdown-is-selectable-as-rendered ()
  (with-temp-buffer
    (insert "앞 **굵게** 뒤\n")
    (agent-shell-observer--render-user-input (point-min) (point-max))
    ;; The rendering is the buffer's text: a region takes what is seen.
    (goto-char (point-min))
    (search-forward "굵게")
    (set-mark (match-beginning 0))
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end)) "굵게"))
    ;; And keeps its source for copying, as a reply's does.
    (should (equal (get-text-property (region-beginning) 'agent-shell-markdown-source)
                   "**굵게**"))))

(ert-deftest agent-shell-observer-user-markdown-code-copy-works ()
  (with-temp-buffer
    (insert "```emacs-lisp\n(message \"hello\")\n```\n")
    (agent-shell-observer--render-user-input (point-min) (point-max))
    (let* ((copy (text-property-any (point-min) (point-max)
                                    'agent-shell-markdown-source-block-copy t))
           (map (get-char-property copy 'keymap))
           (kill-ring nil))
      (should copy)
      (goto-char copy)
      (call-interactively (lookup-key map (kbd "RET")))
      (should (equal (string-trim-right (car kill-ring)) "(message \"hello\")")))))

(ert-deftest agent-shell-observer-user-markdown-hangul-emphasis ()
  (dolist (example '(("**강조**만" "강조만" "강조" agent-shell-markdown-bold)
                     ("앞**강조**뒤" "앞강조뒤" "강조" agent-shell-markdown-bold)
                     ("앞*기울임*뒤" "앞기울임뒤" "기울임" agent-shell-markdown-italic)
                     ("앞__강조__뒤" "앞강조뒤" "강조" agent-shell-markdown-bold)
                     ("앞***강조***뒤" "앞강조뒤" "강조" agent-shell-markdown-bold)))
    (let* ((rendered (agent-shell-observer-test--render (car example)))
           (position (string-search (nth 2 example) rendered))
           (face (get-text-property position 'face rendered)))
      (should (equal (substring-no-properties rendered) (nth 1 example)))
      (should (memq (nth 3 example) (ensure-list face))))))

(ert-deftest agent-shell-observer-user-markdown-hangul-emphasis-leaves-code-and-escapes ()
  (let ((rendered (agent-shell-observer-test--render
                   "`**강조**만`\n\n```text\n**강조**만\n```\n\n\\*\\*강조\\*\\*만\na*b*c\n")))
    (should (string-search "**강조**만" rendered))
    (should (string-search "a*b*c" rendered))
    (should (= (cl-count ?* (substring-no-properties rendered)) 14))))

(ert-deftest agent-shell-observer-user-markdown-hangul-emphasis-preserves-link-destinations ()
  (let* ((url "https://example.com/**강조**만")
         (rendered (agent-shell-observer-test--render (format "[문서](%s)\n" url))))
    (should (equal (get-text-property (string-search "문서" rendered)
                                      'agent-shell-markdown-url rendered)
                   url))))

(ert-deftest agent-shell-observer-user-markdown-reduces-gaps-but-keeps-code-blank-lines ()
  (let* ((source "# 제목\n\n\n\n**내용**\n\n```python\nprint(1)\n\n\nprint(2)\n```\n")
         (rendered (agent-shell-observer-test--render source))
         (gap (string-match "\n\n" rendered)))
    (should gap)
    (should (equal (get-text-property (1+ gap) 'line-height rendered)
                   agent-shell-observer-user-blank-line-height))
    (should (string-search "print(1)\n\n\nprint(2)" rendered))
    (should-not (string-search "제목\n\n\n" rendered))))

(ert-deftest agent-shell-observer-user-markdown-only-renders-committed-history ()
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode
                comint-prompt-regexp "^Me> ")
    (insert (propertize "Me> " 'field 'output 'font-lock-face 'comint-highlight-prompt))
    (insert "**first question**\n")
    (insert (propertize "<shell-maker-end-of-prompt>" 'shell-maker--marker t))
    (insert "\n**agent answer**\n")
    (insert (propertize "Me> " 'field 'output 'font-lock-face 'comint-highlight-prompt))
    (insert "**editable draft**")
    (agent-shell-observer-render-user-history)
    (let ((text (buffer-string)))
      (should (string-search "Me> first question\n<shell-maker-end-of-prompt>" text))
      ;; The reply is agent-shell's to render, and the draft is still being typed.
      (should (string-search "**agent answer**" text))
      (should (string-search "**editable draft**" text)))))

(ert-deftest agent-shell-observer-user-markdown-renders-submitted-input ()
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode)
    (insert "**question**\n```elisp\n(+ 1 2)\n```\n")
    (setq-local comint-last-input-start (copy-marker (point-min))
                comint-last-input-end (copy-marker (point-max)))
    (shell-maker--freeze-submitted-input)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-prefix-p "question\n" text))
      (should-not (string-search "```" text))
      (should (string-search "(+ 1 2)" text)))
    (should (get-text-property (point-min) 'read-only))))

(ert-deftest agent-shell-observer-user-markdown-render-failure-keeps-input ()
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode)
    (insert "**original question**")
    (setq-local comint-last-input-start (copy-marker (point-min))
                comint-last-input-end (copy-marker (point-max)))
    (cl-letf (((symbol-function 'agent-shell--render-markdown)
               (lambda (&rest _) (error "Renderer unavailable"))))
      ;; A render failure cannot abort the submit pipeline after input capture.
      (shell-maker--freeze-submitted-input)
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "**original question**")))))

(defun agent-shell-observer-test--notification (kind text &optional id)
  "Return a history update of KIND containing TEXT and optional message ID."
  `((method . "session/update")
    (params . ((update . ((sessionUpdate . ,kind)
                         (messageId . ,id)
                         (content . ((type . "text") (text . ,text)))))))))

(ert-deftest agent-shell-observer-replay-keeps-100-messages-not-100-chunks ()
  (with-temp-buffer
    (setq-local agent-shell-observer--restoring '((entry . "Done")))
    (let* ((agent-shell-observer-restore-message-limit 100)
           (pending (agent-shell--make-pending-restore))
           (state (list (cons :buffer (current-buffer)) (cons :pending-restore pending)))
           notice)
      (dotimes (number 250)
        (let ((kind (if (cl-evenp number) "user_message_chunk" "agent_message_chunk"))
              (id (format "message-%d" number)))
          (agent-shell--append-restore-notification
           state (agent-shell-observer-test--notification kind (format "%d-a" number) id))
          (when (cl-oddp number)
            (agent-shell--append-restore-notification
             state (agent-shell-observer-test--notification "tool_call" "old tool output")))
          ;; Matching IDs keep chunks together even across tool notifications.
          (agent-shell--append-restore-notification
           state (agent-shell-observer-test--notification kind (format "%d-b" number) id))
          (should (<= (length (map-elt pending :prompt-turns)) 100))))
      (should (= (map-elt pending :agent-shell-observer-message-count) 100))
      (should (= (map-elt pending :agent-shell-observer-omitted-count) 150))
      (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment)
                 (lambda (&rest args) (setq notice (plist-get args :body)))))
        (agent-shell-observer--show-restore-limit state))
      (should (string-search "latest 100 messages" notice))
      (should (string-search "150 older messages omitted" notice))
      (let ((messages (agent-shell--pop-pending-restore state)))
        (should (= (length messages) 100))
        (should (equal (map-nested-elt (caar messages) '(params update messageId))
                       "message-150"))
        (should (equal (map-nested-elt (car (car (last messages)))
                                      '(params update messageId))
                       "message-249"))
        (should (equal (map-nested-elt (car (last (car (last messages))))
                                      '(params update content text))
                       "249-b"))
        (should-not (map-elt state :pending-restore))))))

(ert-deftest agent-shell-observer-replay-limits-agents-without-message-ids ()
  (with-temp-buffer
    (setq-local agent-shell-observer--restoring '((entry . "Done")))
    (let* ((agent-shell-observer-restore-message-limit 2)
           (pending (agent-shell--make-pending-restore))
           (state (list (cons :buffer (current-buffer)) (cons :pending-restore pending))))
      (dolist (update '(("user_message_chunk" "old question")
                        ("agent_message_chunk" "answer-a")
                        ("agent_message_chunk" "answer-b")
                        ("tool_call" "tool")
                        ("agent_message_chunk" "after tool")))
        (agent-shell--append-restore-notification
         state (apply #'agent-shell-observer-test--notification update)))
      (should (= (map-elt pending :agent-shell-observer-message-count) 2))
      (should (= (map-elt pending :agent-shell-observer-omitted-count) 1))
      (let ((messages (agent-shell--pop-pending-restore state)))
        (should (= (length messages) 2))
        (should (equal (map-nested-elt (caar messages) '(params update content text))
                       "answer-a"))))))

(ert-deftest agent-shell-observer-replay-leaves-unmanaged-shells-unchanged ()
  (with-temp-buffer
    (let* ((pending (agent-shell--make-pending-restore))
           (state (list (cons :buffer (current-buffer)) (cons :pending-restore pending))))
      (dolist (kind '("user_message_chunk" "agent_message_chunk" "user_message_chunk"))
        (agent-shell--append-restore-notification
         state (agent-shell-observer-test--notification kind "chunk")))
      (should (= (length (map-elt pending :prompt-turns)) 2))
      (should-not (map-elt pending :agent-shell-observer-message-count)))))

(ert-deftest agent-shell-observer-restoring-blocks-new-questions-until-finished ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--emit (car agent-shell-observer-test--shells)
                             'turn-complete :stop-reason "end_turn")
    (let ((entry (car agent-shell-observer--entries)))
      (kill-buffer (plist-get entry :shell))
      (setq agent-shell-observer-test--shells nil)
      (let ((shell (agent-shell-observer--revive entry)))
        (should-error (agent-shell-observer-test--ask 2) :type 'user-error)
        (should (= (length agent-shell-observer--entries) 1))
        (should-error (agent-shell-observer--dispatch entry shell) :type 'user-error)
        (with-current-buffer shell
          (should-not agent-shell-observer--pending)
          ;; Direct shell submission fails before touching or queueing input.
          (let ((before (buffer-string)))
            (should (string-match-p
                     "still restoring"
                     (error-message-string
                      (should-error (shell-maker-submit :input "new question")
                                    :type 'user-error))))
            (should (equal before (buffer-string))))
          (should-error (agent-shell--prompt-queue-enqueue :prompt "queued question")
                        :type 'user-error)
          (should-error (agent-shell--send-command :prompt "new question" :shell-buffer shell)
                        :type 'user-error))
        (agent-shell-observer-test--emit shell 'session-restored)
        (agent-shell-observer-test--ask 2)
        (should (= (length agent-shell-observer--entries) 2))
        (should (equal (plist-get (car agent-shell-observer--entries) :status) "Answering…"))))))

(ert-deftest agent-shell-observer-restore-guard-allows-composing-and-other-shells ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((shell (car agent-shell-observer-test--shells))
          (other (agent-shell-observer-test--shell 'codex project)))
      (with-current-buffer shell
        (setq-local agent-shell-observer--restoring '((entry . "Done"))))
      (should-error (agent-shell-observer--guard-insert :submit t :shell-buffer shell)
                    :type 'user-error)
      (should-not (agent-shell-observer--guard-insert :submit nil :shell-buffer shell))
      (should-not (agent-shell-observer--guard-submission :shell-buffer other)))))

(ert-deftest agent-shell-observer-keeps-the-newest-100-in-memory-and-on-disk ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let* ((agent-shell-observer-history-limit 100)
           (base (car agent-shell-observer--entries))
           (shell (plist-get base :shell)))
      ;; An old file can be unsorted; clock strings alone cannot order days.
      (setq agent-shell-observer--entries
            (cl-loop for id from 1 to 105
                     collect (let ((entry (copy-sequence base)))
                               (plist-put entry :id id)
                               (plist-put entry :time (if (= id 1) "23:59" "00:00"))
                               entry)))
      (agent-shell-observer--save-history)
      (should (= (length agent-shell-observer--entries) 100))
      (should (equal (mapcar (lambda (e) (plist-get e :id)) agent-shell-observer--entries)
                     (number-sequence 105 6 -1)))
      (agent-shell-observer--load-history)
      (should (= (length agent-shell-observer--entries) 100))
      (should (= agent-shell-observer--next-id 105))
      (should (= (plist-get (car (last agent-shell-observer--entries)) :id) 6))
      (should (buffer-live-p shell)))))

(ert-deftest agent-shell-observer-history-limit-applies-to-loaded-and-new-questions ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--ask 2)
    (agent-shell-observer-test--ask 3)
    ;; Simulate legacy storage in ascending order with a lower configured cap.
    (let ((data (with-temp-buffer
                  (insert-file-contents agent-shell-observer-history-file)
                  (read (current-buffer))))
          (agent-shell-observer-history-limit 2))
      (plist-put data :entries (reverse (plist-get data :entries)))
      (with-temp-file agent-shell-observer-history-file (prin1 data (current-buffer)))
      (agent-shell-observer--load-history)
      (should (equal (mapcar (lambda (e) (plist-get e :id)) agent-shell-observer--entries) '(3 2)))
      (agent-shell-observer-test--ask 1)
      (should (equal (mapcar (lambda (e) (plist-get e :id)) agent-shell-observer--entries) '(4 3)))
      (agent-shell-observer--load-history)
      (should (equal (mapcar (lambda (e) (plist-get e :id)) agent-shell-observer--entries) '(4 3))))))

(ert-deftest agent-shell-observer-history-restores-and-reconnects ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1 "왜 이 코드인가요?")
    (let ((id (plist-get (car agent-shell-observer--entries) :id))
          started)
      (should (= (logand (file-modes agent-shell-observer-history-file) #o777) #o600))
      (dolist (shell agent-shell-observer-test--shells) (kill-buffer shell))
      (setq agent-shell-observer-test--shells nil
            agent-shell-observer--entries nil
            agent-shell-observer--next-id 0)
      (agent-shell-observer--load-history)
      (should-not agent-shell-observer-test--shells)
      (let ((entry (car agent-shell-observer--entries)))
        (should (equal (plist-get entry :title) "왜 이 코드인가요?"))
        (should (equal (plist-get entry :status) "Stopped (Emacs restarted)"))
        (should (equal (plist-get entry :cwd) project))
        (should-not (plist-get entry :shell))
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest args)
                     (setq started args)
                     (agent-shell-observer-test--shell
                      (map-elt (plist-get args :config) :identifier) project)))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest _) (ert-fail "Restoring must not resubmit the question"))))
          (agent-shell-observer--revive entry))
        (should (equal (plist-get started :session-id) "session"))
        (should (equal (plist-get started :cwd) project))
        (agent-shell-observer-test--emit (plist-get entry :shell) 'session-restored))
      (agent-shell-observer-test--ask 2)
      (should (> (plist-get (car agent-shell-observer--entries) :id) id)))))

(ert-deftest agent-shell-observer-history-keeps-completed-status-and-deletions ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--emit (car agent-shell-observer-test--shells)
                                'turn-complete :stop-reason "end_turn")
    (agent-shell-observer--load-history)
    (should (equal (plist-get (car agent-shell-observer--entries) :status) "Done"))
    (with-current-buffer (agent-shell-observer--list-buffer)
      (tabulated-list-print t)
      (goto-char (point-min))
      (agent-shell-observer-list-delete))
    (agent-shell-observer--load-history)
    (should-not agent-shell-observer--entries)))

(ert-deftest agent-shell-observer-restored-questions-share-one-shell ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--ask 2)
    (dolist (shell agent-shell-observer-test--shells) (kill-buffer shell))
    (setq agent-shell-observer-test--shells nil)
    (agent-shell-observer--load-history)
    (let ((shell (agent-shell-observer--revive (car agent-shell-observer--entries))))
      (should (eq shell (agent-shell-observer--revive (cadr agent-shell-observer--entries))))
      (should (= (length agent-shell-observer-test--shells) 1)))))

(ert-deftest agent-shell-observer-history-preserves-unreadable-file ()
  (agent-shell-observer-test-with-agents
    (dolist (contents '("(" "(:version 99 :entries nil)"
                        "(:version 1 :entries ((:id 1)))"))
      (with-temp-file agent-shell-observer-history-file (insert contents))
      (agent-shell-observer--load-history)
      (should-not agent-shell-observer--history-readable)
      (agent-shell-observer--save-history)
      (should (equal contents (with-temp-buffer
                               (insert-file-contents agent-shell-observer-history-file)
                               (buffer-string)))))))

(ert-deftest agent-shell-observer-retry-replaces-session-id-before-queueing ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let* ((entry (car agent-shell-observer--entries))
           (shell (agent-shell-observer-test--shell 'codex project)))
      (with-current-buffer shell
        (setf (alist-get :id (alist-get :session agent-shell--state)) "codex-session"))
      (cl-letf (((symbol-function 'agent-shell--can-insert-into-prompt-p) #'ignore)
                ((symbol-function 'agent-shell-prompt-queue) #'ignore))
        (agent-shell-observer--dispatch entry shell))
      (agent-shell-observer--load-history)
      (should (eq (plist-get (car agent-shell-observer--entries) :agent) 'codex))
      (should (equal (plist-get (car agent-shell-observer--entries) :session-id)
                     "codex-session")))))

;;; Scroll positions

(ert-deftest agent-shell-observer-history-open-marker-is-independent-of-cursor ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (agent-shell-observer-test--ask 2)
      (let ((opened (car agent-shell-observer--entries)))
        (agent-shell-observer--show opened)
        (with-current-buffer (agent-shell-observer--list-buffer)
          (goto-char (point-min))
          (forward-line 1)
          (let ((this-command 'next-line) (last-command-event ?n))
            (run-hooks 'post-command-hook))
          (should (eq opened agent-shell-observer--shown))
          (save-excursion
            (goto-char (overlay-start agent-shell-observer--active-overlay))
            (should (eq (tabulated-list-get-id) (plist-get opened :id))))
          (agent-shell-observer--refresh-list)
          (save-excursion
            (goto-char (overlay-start agent-shell-observer--active-overlay))
            (should (eq (tabulated-list-get-id) (plist-get opened :id))))
          ;; Its session's row stands out too, with a bar, not a triangle
          ;; that would read as the session's arrow.
          (save-excursion
            (goto-char (overlay-start agent-shell-observer--session-overlay))
            (should (equal (tabulated-list-get-id)
                           (cons 'session (plist-get opened :shell)))))
          (should (eq (overlay-get agent-shell-observer--session-overlay 'face)
                      'agent-shell-observer-open))
          (should (equal (get-text-property
                          0 'display
                          (overlay-get agent-shell-observer--session-overlay 'before-string))
                         '(left-fringe vertical-bar success))))))))

(ert-deftest agent-shell-observer-history-only-a-double-click-opens ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (agent-shell-observer-test--ask 2)
      (with-current-buffer (agent-shell-observer--list-buffer)
        (let* ((second (cadr (agent-shell-observer-test--question-lines)))
               opened)
          (cl-letf (((symbol-function 'mouse-set-point)
                     (lambda (line) (goto-char (point-min)) (forward-line line)))
                    ((symbol-function 'agent-shell-observer-list-open)
                     (lambda () (push (tabulated-list-get-id) opened))))
            ;; Clicks one by one, however many, only move to the line.
            (dotimes (_ 3) (agent-shell-observer-list-click second))
            (should-not opened)
            (should (= (line-number-at-pos) (1+ second)))
            (agent-shell-observer-list-click 0)
            (agent-shell-observer-list-click 0)
            (should-not opened)
            ;; A double click opens the line: a question, or a session.
            (agent-shell-observer-list-double-click second)
            (should (integerp (car opened)))
            (agent-shell-observer-list-double-click 0)
            (should (eq (car-safe (car opened)) 'session))
            ;; The details below a session stand for it.
            (agent-shell-observer-list-double-click 1)
            (should (eq (car-safe (car opened)) 'detail))))))))

(ert-deftest agent-shell-observer-the-arrow-opens-and-closes-the-details ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (with-current-buffer (agent-shell-observer--list-buffer)
        (agent-shell-observer--refresh-list)
        (goto-char (point-min))
        ;; The arrow is a button of its own.
        (should (eq (lookup-key (get-text-property (point) 'keymap) [mouse-1])
                    #'agent-shell-observer-list-click-arrow))
        ;; The list's follow-link makes a click on it mouse-2.
        (should (eq (lookup-key (get-text-property (point) 'keymap) [mouse-2])
                    #'agent-shell-observer-list-click-arrow))
        ;; A double click elsewhere on the line opens the session at once.
        (should (eq (lookup-key agent-shell-observer-list-mode-map [double-mouse-1])
                    #'agent-shell-observer-list-double-click))
        (let (opened)
          (cl-letf (((symbol-function 'mouse-set-point)
                     (lambda (line) (goto-char (point-min)) (forward-line line)))
                    ((symbol-function 'agent-shell-observer-list-open)
                     (lambda () (setq opened (tabulated-list-get-id)))))
            (agent-shell-observer-list-double-click 0))
          (should (eq (car-safe opened) 'session)))
        (goto-char (point-min))
        (should (equal (substring-no-properties (aref (tabulated-list-get-entry) 0) 0 1)
                       "▼"))
        (let ((opens 0))
          (cl-letf (((symbol-function 'mouse-set-point)
                     (lambda (line) (goto-char (point-min)) (forward-line line)))
                    ((symbol-function 'agent-shell-observer-list-open)
                     (lambda () (setq opens (1+ opens)))))
            (agent-shell-observer-list-click-arrow 0)
            (should-not (agent-shell-observer-test--question-lines))
            (should (equal (substring-no-properties
                            (aref (save-excursion (goto-char (point-min))
                                                  (tabulated-list-get-entry))
                                  0)
                            0 1)
                           "▶"))
            (agent-shell-observer-list-click-arrow 0)
            (should (agent-shell-observer-test--question-lines))
            ;; Opening and closing opens no conversation.
            (should (= opens 0))))))))

(ert-deftest agent-shell-observer-history-movement-does-not-open-conversations ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (agent-shell-observer-test--ask 2)
      (with-current-buffer (agent-shell-observer--list-buffer)
        (let ((calls 0))
          (cl-letf (((symbol-function 'agent-shell-observer--show)
                     (lambda (&rest _) (setq calls (1+ calls)))))
            (goto-char (point-min))
            (while (not (eobp))
              (run-hooks 'post-command-hook)
              (forward-line 1))
            (should (= calls 0))
            (goto-char (point-min))
            (agent-shell-observer-list-open)
            (should (= calls 1))))))))

(ert-deftest agent-shell-observer-history-header-follows-horizontal-scroll ()
  (with-temp-buffer
    (agent-shell-observer-list-mode)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (set-window-hscroll nil 0)
      (should (= (string-match "Project" (agent-shell-observer--list-header)) 32))
      (should (= (string-match "Last" (agent-shell-observer--list-header)) 14))
      (set-window-hscroll nil 20)
      ;; The fringe inset is independent of the horizontal text offset.
      (should (= (string-match "Project" (agent-shell-observer--list-header)) 12))
      (set-window-hscroll nil 33)
      (should (string-prefix-p "roject" (agent-shell-observer--list-header)))
      ;; The filters follow the column names.
      (set-window-hscroll nil 0)
      (let ((agent-shell-observer--filter 'waiting)
            (agent-shell-observer--filter-terms '("prowj")))
        (should (equal (substring-no-properties (agent-shell-observer--filter-label))
                       "[waiting for you] [/prowj]"))
        (should (string-search "agent-shell-observer--filter-label"
                               (format "%S" header-line-format)))))))

(ert-deftest agent-shell-observer-history-keeps-long-file-paths-scrollable ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (let ((path (concat "/project/" (make-string 90 ?a) "/source/code.el:123")))
        (plist-put (car agent-shell-observer--entries) :location path)
        (agent-shell-observer--refresh-list)
        (with-current-buffer (agent-shell-observer--list-buffer)
          ;; The question's line has the whole path, to scroll to.
          (should (string-search path (buffer-string)))
          (should (eq horizontal-scroll-bar 'bottom)))))))

(ert-deftest agent-shell-observer-file-link-preserves-the-conversation-window ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (let* ((entry (car agent-shell-observer--entries))
             (shell (plist-get entry :shell)))
        (agent-shell-observer-test--fill-shell shell)
        (agent-shell-observer--show entry)
        (let* ((chat-window (agent-shell-observer--shell-window))
               (view (agent-shell-observer-test--scroll 25 0)))
          (select-window chat-window)
          (let ((target (agent-shell-markdown-visit-file :file file :line-start 2)))
            (should (window-live-p target))
            (should-not (eq target chat-window))
            (should-not (window-parameter target 'window-side))
            (should (equal (buffer-file-name (window-buffer target)) file))
            (should (= (line-number-at-pos (window-point target)) 2)))
          (should (eq (window-buffer chat-window) shell))
          (should (equal (agent-shell-observer-test--view) view)))))))

(defun agent-shell-observer-test--fill-shell (shell)
  "Give SHELL enough long lines to scroll in both directions."
  (with-current-buffer shell
    (goto-char (point-max))
    (dotimes (line 150)
      (insert (format "%03d %s\n" line (make-string 150 ?x))))))

(defun agent-shell-observer-test--scroll (line hscroll)
  "Move the shell window to LINE, remembering a cursor just below it."
  (let* ((window (agent-shell-observer--shell-window))
         (start (with-current-buffer (window-buffer window)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line line)
                    (point))))
         (point (with-current-buffer (window-buffer window)
                  (save-excursion (goto-char start) (forward-line 1) (point)))))
    (set-window-point window point)
    (set-window-start window start t)
    (set-window-hscroll window hscroll)
    (list start point hscroll)))

(defun agent-shell-observer-test--view ()
  "Return the shell window's scroll, cursor, and horizontal positions."
  (let ((window (agent-shell-observer--shell-window)))
    (list (window-start window) (window-point window) (window-hscroll window))))

(ert-deftest agent-shell-observer-scroll-survives-switching-shells ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (agent-shell-observer-test--ask 2)
      (let* ((first (cadr agent-shell-observer--entries))
             (second (car agent-shell-observer--entries))
             (other (agent-shell-observer-test--shell 'codex project)))
        (plist-put second :shell other)
        (with-current-buffer other (insert (plist-get second :prompt)))
        (agent-shell-observer-test--fill-shell (plist-get first :shell))
        (agent-shell-observer-test--fill-shell other)
        (agent-shell-observer--show first)
        (let ((first-view (agent-shell-observer-test--scroll 25 7)))
          (agent-shell-observer--show second)
          (let ((second-view (agent-shell-observer-test--scroll 60 3)))
            (with-current-buffer (agent-shell-observer--list-buffer)
              (agent-shell-observer--show first)
              (should (equal first-view (agent-shell-observer-test--view)))
              (agent-shell-observer--show second)
              (should (equal second-view (agent-shell-observer-test--view))))))))))

(ert-deftest agent-shell-observer-toggle-includes-conversation-without-list ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (let ((entry (car agent-shell-observer--entries)))
        (agent-shell-observer-test--fill-shell (plist-get entry :shell))
        (agent-shell-observer--show entry)
        (let ((view (agent-shell-observer-test--scroll 25 0)))
          (delete-window (get-buffer-window agent-shell-observer-list-buffer))
          (agent-shell-observer-toggle)
          (should-not (agent-shell-observer--shell-window))
          (should-not (get-buffer-window agent-shell-observer-list-buffer))
          (agent-shell-observer-toggle)
          (should (get-buffer-window agent-shell-observer-list-buffer))
          (should (equal view (agent-shell-observer-test--view))))))))

(ert-deftest agent-shell-observer-scroll-is-per-question-in-a-shared-shell ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (agent-shell-observer-test--ask 2)
      (let ((first (cadr agent-shell-observer--entries))
            (second (car agent-shell-observer--entries)))
        (should (eq (plist-get first :shell) (plist-get second :shell)))
        (agent-shell-observer-test--fill-shell (plist-get first :shell))
        (agent-shell-observer--show first)
        (let ((first-view (agent-shell-observer-test--scroll 25 0)))
          (agent-shell-observer--show second)
          (let ((second-view (agent-shell-observer-test--scroll 60 0)))
            (agent-shell-observer--show first)
            (should (equal first-view (agent-shell-observer-test--view)))
            (agent-shell-observer--show second)
            (should (equal second-view (agent-shell-observer-test--view)))
            ;; Reopening the same item and hiding/showing the panel keep it.
            (agent-shell-observer--show second)
            (should (equal second-view (agent-shell-observer-test--view)))
            (agent-shell-observer-hide)
            (agent-shell-observer-toggle)
            (should (equal second-view (agent-shell-observer-test--view)))
            (agent-shell-observer--save-history)
            (agent-shell-observer--load-history)
            (should-not (plist-get (car agent-shell-observer--entries) :view))))))))

(ert-deftest agent-shell-observer-scroll-markers-follow-buffer-edits ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (agent-shell-observer-test--ask 2)
      (let* ((first (cadr agent-shell-observer--entries))
             (second (car agent-shell-observer--entries))
             (shell (plist-get first :shell)))
        (agent-shell-observer-test--fill-shell shell)
        (agent-shell-observer--show first)
        (let ((view (agent-shell-observer-test--scroll 25 0)))
          (agent-shell-observer--show second)
          (with-current-buffer shell
            (goto-char (point-min))
            (insert "prefix\n"))
          (agent-shell-observer--show first)
          (should (equal (agent-shell-observer-test--view)
                         (list (+ 7 (nth 0 view)) (+ 7 (nth 1 view)) 0))))))))

;;; Registered prompts

(ert-deftest agent-shell-observer-hook-ends-every-prompt ()
  (agent-shell-observer-test-with-agents
    (let* ((agent-shell-observer-prompt-hook "Answer in Korean.")
           (context (agent-shell-observer--context (point-min) (line-end-position) project)))
      (should (string-suffix-p "```\n\nAnswer in Korean." (agent-shell-observer--prompt "Why?" context)))
      (let ((agent-shell-observer-prompt-hook nil))
        (should (string-suffix-p "first line\n```" (agent-shell-observer--prompt "Why?" context)))))))

(ert-deftest agent-shell-observer-prompts-are-in-english ()
  (should (> (length agent-shell-observer-prompts) 5))
  (dolist (prompt agent-shell-observer-prompts)
    (should (string-match-p "\\`[[:ascii:]]+\\'" (car prompt)))
    (should (string-match-p "\\`[[:ascii:]]+\\'" (cdr prompt))))
  ;; So is what `agent-shell-observer-explain' and `-ask' send; the hook sets the answer's language.
  (dolist (text (list agent-shell-observer-question agent-shell-observer-document-question
                      agent-shell-observer-answer-style))
    (should (string-match-p "\\`[[:ascii:]]+\\'" text))))

(ert-deftest agent-shell-observer-prompt-sends-the-picked-prompt-under-its-name ()
  (agent-shell-observer-test-with-agents
    (let ((agent-shell-observer-prompt-hook "Answer in Korean.")
          sent)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   ;; The table answers what the picker asks of it.
                   (should (eq (completion-metadata-get
                                (funcall collection "" nil 'metadata) 'category)
                               'agent-shell-observer-prompt))
                   (should (member "review: bugs" (all-completions "review" collection)))
                   "review: bugs"))
                ((symbol-function 'agent-shell-insert)
                 (lambda (&rest args) (setq sent (plist-get args :text)))))
        (goto-char (point-min))
        (forward-line 1)
        (call-interactively #'agent-shell-observer-prompt))
      (should (string-prefix-p (alist-get "review: bugs" agent-shell-observer-prompts nil nil #'equal)
                               sent))
      (should (string-search "code.el:2\n```emacs-lisp\nsecond line\n```" sent))
      (should (string-suffix-p "Answer in Korean." sent))
      (should (equal (nth 4 (car (agent-shell-observer-test--rows))) "review: bugs")))))

;;; Sessions

(defun agent-shell-observer-test--question-lines ()
  "Return the line numbers, from 0, of the questions in the list."
  (with-current-buffer agent-shell-observer-list-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((line 0) lines)
        (while (not (eobp))
          (when (integerp (tabulated-list-get-id)) (push line lines))
          (forward-line 1)
          (setq line (1+ line)))
        (nreverse lines)))))

(defun agent-shell-observer-test--session-rows ()
  "Return the session rows of the list, as lists of their columns, marks aside."
  (with-current-buffer agent-shell-observer-list-buffer
    (save-excursion
      (goto-char (point-min))
      (let (rows)
        (while (not (eobp))
          (when (eq (car-safe (tabulated-list-get-id)) 'session)
            (push (mapcar #'substring-no-properties
                          (cdr (append (tabulated-list-get-entry) nil)))
                  rows))
          (forward-line 1))
        (nreverse rows)))))

(defun agent-shell-observer-test--run (shell)
  "Give the fake SHELL a running process, so its session counts as running."
  (with-current-buffer shell
    (make-process :name "fake-shell" :buffer shell :noquery t
                  :command '("sleep" "60"))))

(ert-deftest agent-shell-observer-a-session-holds-its-shell-questions ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--ask 2)
    (with-current-buffer (agent-shell-observer--list-buffer)
      (agent-shell-observer--refresh-list)
      ;; One shell, one session, its questions newest first below it.
      (should (= (length (agent-shell-observer-test--session-rows)) 1))
      (should (equal (mapcar (lambda (row) (nth 3 row)) (agent-shell-observer-test--rows))
                     (let ((name (file-name-nondirectory (directory-file-name project))))
                       (list (concat name "/code.el:2") (concat name "/code.el:1")))))
      ;; Where it works on one line, and its questions counted.
      (should (string-search (concat (abbreviate-file-name project) "  session session")
                             (buffer-string)))
      (should (string-search "Questions (2)" (buffer-string))))))

(ert-deftest agent-shell-observer-a-session-opens-and-closes ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (with-current-buffer (agent-shell-observer--list-buffer)
      (agent-shell-observer--refresh-list)
      ;; A question on the go opens its session by itself.
      (should (agent-shell-observer-test--question-lines))
      (goto-char (point-min))
      (agent-shell-observer-list-toggle-session)
      (should-not (agent-shell-observer-test--question-lines))
      (should (eq (car-safe (tabulated-list-get-id)) 'session))
      ;; Closed by hand, it stays closed when its question moves on.
      (agent-shell-observer-test--emit (car agent-shell-observer-test--shells)
                                    'permission-request)
      (should-not (agent-shell-observer-test--question-lines))
      (goto-char (point-min))
      (agent-shell-observer-list-toggle-session)
      (should (agent-shell-observer-test--question-lines)))))

(ert-deftest agent-shell-observer-ret-on-a-session-opens-its-newest-question ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--ask 2)
    (with-current-buffer (agent-shell-observer--list-buffer)
      (agent-shell-observer--refresh-list)
      (let (shown)
        (cl-letf (((symbol-function 'agent-shell-observer--show)
                   (lambda (entry &rest _) (setq shown entry))))
          (goto-char (point-min))
          (agent-shell-observer-list-open)
          (should (eq shown (car agent-shell-observer--entries)))
          ;; Showing a session leaves its drop-down as it is.
          (dolist (entry agent-shell-observer--entries) (plist-put entry :status "Done"))
          (clrhash agent-shell-observer--opened)
          (with-current-buffer (agent-shell-observer--list-buffer)
            (agent-shell-observer--refresh-list)
            (should-not (agent-shell-observer-test--question-lines))
            (goto-char (point-min))
            (save-window-excursion (agent-shell-observer-list-open)))
          (with-current-buffer (agent-shell-observer--list-buffer)
            (agent-shell-observer--refresh-list)
            (should-not (agent-shell-observer-test--question-lines))
            ;; With a space between the arrow and the mark.
            (goto-char (point-min))
            (should (string-match-p "\\`[▶▼] [●○]"
                                    (substring-no-properties
                                     (aref (tabulated-list-get-entry) 0))))))))))

(ert-deftest agent-shell-observer-closed-sessions-keep-their-questions-together ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (agent-shell-observer-test--ask 2)
    (dolist (shell agent-shell-observer-test--shells) (kill-buffer shell))
    (setq agent-shell-observer-test--shells nil)
    (with-current-buffer (agent-shell-observer--list-buffer)
      (agent-shell-observer--refresh-list)
      (let ((rows (agent-shell-observer-test--session-rows)))
        (should (= (length rows) 1))
        (should (equal (car (car rows)) "Closed"))))))

(ert-deftest agent-shell-observer-session-state-follows-the-shell ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let ((shell (car agent-shell-observer-test--shells))
          (state (lambda ()
                   (with-current-buffer (agent-shell-observer--list-buffer)
                     (agent-shell-observer--refresh-list)
                     (car (car (agent-shell-observer-test--session-rows)))))))
      (agent-shell-observer-test--run shell)
      (should (equal (funcall state) "Working"))
      (agent-shell-observer-test--emit shell 'permission-request
                                    :tool-call '((:title . "Run: make test")))
      (should (equal (funcall state) "Waiting"))
      (should (equal (buffer-local-value 'agent-shell-observer--shell-detail shell)
                     "Run: make test"))
      (should (string-search "⚑1" agent-shell-observer--mode-line-string))
      ;; The drop-down says what it waits for, on a line of its own.
      (with-current-buffer (agent-shell-observer--list-buffer)
        (should (string-search "⚑ Waiting for Run: make test" (buffer-string))))
      (agent-shell-observer-test--emit shell 'permission-response)
      (should (equal (funcall state) "Working"))
      (should (string-search "▶1" agent-shell-observer--mode-line-string))
      (agent-shell-observer-test--emit shell 'turn-complete :stop-reason "end_turn")
      (should (equal (funcall state) "Idle"))
      (should-not agent-shell-observer--mode-line-string)
      ;; A conversation being replayed is no news.
      (with-current-buffer shell
        (setf (alist-get :pending-restore agent-shell--state) t))
      (agent-shell-observer-test--emit shell 'input-submitted :prompt "old")
      (should (equal (funcall state) "Idle")))))

(ert-deftest agent-shell-observer-notifies-what-a-shell-needs ()
  (agent-shell-observer-test-with-agents
    (let ((agent-shell-observer-notify-states '(waiting done error))
          (agent-shell-observer--notifications nil)
          sent focused)
      (cl-letf (((symbol-function 'notifications-notify)
                 (lambda (&rest args) (push args sent) (length sent)))
                ((symbol-function 'frame-focus-state) (lambda (&rest _) focused)))
        (agent-shell-observer-test--ask 1)
        (let ((shell (car agent-shell-observer-test--shells)))
          (agent-shell-observer-test--emit shell 'permission-request
                                        :tool-call '((:title . "Run: make test")))
          (agent-shell-observer-test--emit shell 'permission-response)
          (agent-shell-observer-test--emit shell 'turn-complete :stop-reason "end_turn")
          (should (equal (mapcar (lambda (n) (plist-get n :title)) (reverse sent))
                         '("antigravity needs Run: make test" "antigravity is done")))
          ;; A later notification takes the place of the earlier one.
          (should (equal (plist-get (car sent) :replaces-id) 1))
          ;; Nothing while the shell is the window in front.
          (setq sent nil focused t)
          (switch-to-buffer shell)
          (agent-shell-observer-test--emit shell 'input-submitted :prompt "more")
          (agent-shell-observer-test--emit shell 'turn-complete :stop-reason "end_turn")
          (should-not sent))))))

(ert-deftest agent-shell-observer-filters-narrow-the-sessions ()
  (agent-shell-observer-test-with-agents
    (agent-shell-observer-test--ask 1)
    (let* ((first (car agent-shell-observer-test--shells))
           (other (agent-shell-observer-test--shell 'codex project)))
      (agent-shell-observer-test--run first)
      (agent-shell-observer-test--run other)
      (with-current-buffer other (agent-shell-observer-watch-shell))
      (agent-shell-observer-test--emit other 'turn-complete :stop-reason "end_turn")
      (with-current-buffer (agent-shell-observer--list-buffer)
        (agent-shell-observer--refresh-list)
        (should (= (length (agent-shell-observer-test--session-rows)) 2))
        ;; f: every session, those waiting for you, those working.
        (agent-shell-observer-list-cycle-filter)
        (should-not (agent-shell-observer-test--session-rows))
        (agent-shell-observer-list-cycle-filter)
        (should (equal (mapcar (lambda (row) (nth 2 row))
                               (agent-shell-observer-test--session-rows))
                       '("antigravity")))
        (agent-shell-observer-list-cycle-filter)
        ;; /: every term picked, any case.
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) '("CODEX"))))
          (agent-shell-observer-list-filter))
        (should (equal (mapcar (lambda (row) (nth 2 row))
                               (agent-shell-observer-test--session-rows))
                       '("codex")))
        (should (assoc "antigravity" (progn (setq agent-shell-observer--filter-terms nil)
                                            (agent-shell-observer--filter-candidates))))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) (signal 'quit nil))))
          (setq agent-shell-observer--filter-terms '("codex"))
          (agent-shell-observer-list-filter))
        (should (equal agent-shell-observer--filter-terms '("codex")))))))

;;; Titles

(ert-deftest agent-shell-observer-a-session-is-named-by-a-title-of-its-first-question ()
  (agent-shell-observer-test-with-agents
    (let ((agent-shell-observer-summarize-titles t)
          asked)
      (cl-letf (((symbol-function 'agent-shell-observer--request-title)
                 (lambda (text callback)
                   (push text asked)
                   (funcall callback (if (string-search "first line" text)
                                         "Read the first line"
                                       "Read the second line")
                            nil))))
        (agent-shell-observer-test--ask 1 "What is on this line?")
        (agent-shell-observer-test--ask 2 "And this one?")
        (with-current-buffer (agent-shell-observer--list-buffer)
          (agent-shell-observer--refresh-list)
          ;; Each question once, without what every prompt adds.
          (should (= (length asked) 2))
          (should-not (seq-some (lambda (text)
                                  (string-search agent-shell-observer-answer-style text))
                                asked))
          (should (equal (plist-get (cadr agent-shell-observer--entries) :summary)
                         "Read the first line"))
          ;; The session goes by its first question's title.
          (should (equal (nth 4 (car (agent-shell-observer-test--session-rows)))
                         "Read the first line"))
          ;; An agent's title that only names the session does not win.
          (let ((shell (car agent-shell-observer-test--shells)))
            (agent-shell-observer-test--emit shell 'session-title-changed
                                          :title "Session 67bfd5e2")
            (should (equal (nth 4 (car (agent-shell-observer-test--session-rows)))
                           "Read the first line"))
            (agent-shell-observer-test--emit shell 'session-title-changed
                                          :title "Fix the reader")
            (should (equal (nth 4 (car (agent-shell-observer-test--session-rows)))
                           "Fix the reader")))
          ;; Titles stay with the history.
          (agent-shell-observer--load-history)
          (should (equal (plist-get (cadr agent-shell-observer--entries) :summary)
                         "Read the first line"))
          (agent-shell-observer--refresh-list)
          (should (= (length asked) 2)))))))

(ert-deftest agent-shell-observer-titles-wait-while-ollama-is-away ()
  (agent-shell-observer-test-with-agents
    (let ((agent-shell-observer-summarize-titles t)
          (asked 0))
      (cl-letf (((symbol-function 'agent-shell-observer--request-title)
                 (lambda (_text callback)
                   (setq asked (1+ asked))
                   (funcall callback nil 'unreachable))))
        (agent-shell-observer-test--ask 1)
        (agent-shell-observer-test--ask 2)
        (agent-shell-observer--refresh-list)
        (agent-shell-observer-test--settle 0.05)
        ;; One try, then nothing until a while has passed.
        (should (= asked 1))
        (should (> agent-shell-observer--title-resume-at (float-time)))
        ;; Meanwhile the question's own words name the session.
        (with-current-buffer (agent-shell-observer--list-buffer)
          (should (string-search (file-name-nondirectory (directory-file-name project))
                                 (nth 4 (car (agent-shell-observer-test--session-rows))))))
        (setq agent-shell-observer--title-resume-at 0)
        (agent-shell-observer--refresh-list)
        (should (= asked 2))))))

(ert-deftest agent-shell-observer-a-model-reply-becomes-a-clean-title ()
  (should (equal (agent-shell-observer--clean-title "\"init.py 경로 확인\".\nmore")
                 "init.py 경로 확인"))
  (should (equal (agent-shell-observer--clean-title "**Fix the build**") "Fix the build"))
  (should-not (agent-shell-observer--clean-title "  \n"))
  (should-not (agent-shell-observer--meaningful-title "Session 67bfd5e2"))
  (should (equal (agent-shell-observer--meaningful-title "Fix it\nmore") "Fix it")))

(ert-deftest agent-shell-observer-paths-in-the-drop-down-go-where-they-say ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 2)
      (with-current-buffer (agent-shell-observer--list-buffer)
        (agent-shell-observer--refresh-list)
        (let (opened visited)
          (cl-letf (((symbol-function 'agent-shell-observer--open-file)
                     (lambda (file) (setq opened file)))
                    ((symbol-function 'agent-shell-observer--visit)
                     (lambda (file line) (setq visited (list file line)))))
            ;; The session's directory.
            (goto-char (point-min))
            (search-forward (abbreviate-file-name project))
            (backward-char 1)
            (should (eq (key-binding [mouse-1]) #'agent-shell-observer-follow-link))
            (agent-shell-observer-follow-link)
            (should (equal opened project))
            ;; The code a question is about, at its line.
            (goto-char (point-min))
            (search-forward "Questions (")
            (search-forward "/code.el:2")
            (backward-char 1)
            (agent-shell-observer-follow-link)
            (should (equal visited (list file 2)))
            ;; Elsewhere on a line, RET still opens the line.
            (beginning-of-line)
            (should (eq (key-binding (kbd "RET")) #'agent-shell-observer-list-open))))))))

(ert-deftest agent-shell-observer-the-conversation-goes-where-the-layout-says ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (delete-other-windows)
      (set-frame-size (selected-frame) 200 50)
      (agent-shell-observer-test--ask 1)
      (let ((entry (car agent-shell-observer--entries))
            (edges (lambda (window) (window-edges window))))
        (dolist (layout '(right left below))
          (let ((agent-shell-observer-layout layout))
            (agent-shell-observer-hide)
            (agent-shell-observer-toggle)
            (let ((list (get-buffer-window agent-shell-observer-list-buffer))
                  (shell (agent-shell-observer--shell-window)))
              (should (window-live-p list))
              (should (window-live-p shell))
              (should (eq (window-buffer shell) (plist-get entry :shell)))
              (pcase layout
                ;; Side by side: the conversation's left edge is the
                ;; list's right one, or the other way round.
                ('right (should (= (nth 2 (funcall edges list))
                                   (nth 0 (funcall edges shell)))))
                ('left (should (= (nth 2 (funcall edges shell))
                                  (nth 0 (funcall edges list)))))
                ;; Below: one above the other, in the same columns.
                ('below (should (= (nth 3 (funcall edges list))
                                   (nth 1 (funcall edges shell))))))
              ;; Other buffers do not take the side-by-side windows.
              (when (memq layout '(right left))
                (should (window-dedicated-p list))
                (should (window-dedicated-p shell))))))
        ;; Hiding takes them all away.
        (agent-shell-observer-hide)
        (should-not (get-buffer-window agent-shell-observer-list-buffer))
        (should-not (agent-shell-observer--shell-window))
        ;; L goes below, right, left, and back.
        (let ((agent-shell-observer-layout 'below))
          (agent-shell-observer-toggle)
          (agent-shell-observer-list-cycle-layout)
          (should (eq agent-shell-observer-layout 'right))
          (should (window-parameter (agent-shell-observer--shell-window)
                                    'agent-shell-observer-role))
          (agent-shell-observer-list-cycle-layout)
          (should (eq agent-shell-observer-layout 'left))
          (agent-shell-observer-list-cycle-layout)
          (should (eq agent-shell-observer-layout 'below))
          (should (eq (window-parameter (agent-shell-observer--shell-window) 'window-side)
                      'right)))))))

(ert-deftest agent-shell-observer-bringing-a-session-back-leaves-it-closed ()
  (agent-shell-observer-test-with-agents
    (save-window-excursion
      (agent-shell-observer-test--ask 1)
      (agent-shell-observer-test--emit (car agent-shell-observer-test--shells)
                                    'turn-complete :stop-reason "end_turn")
      (dolist (shell agent-shell-observer-test--shells) (kill-buffer shell))
      (setq agent-shell-observer-test--shells nil)
      (clrhash agent-shell-observer--opened)
      (with-current-buffer (agent-shell-observer--list-buffer)
        (agent-shell-observer--refresh-list)
        (should-not (agent-shell-observer-test--question-lines))
        ;; Opening the closed session brings its conversation back...
        (goto-char (point-min))
        (save-window-excursion (agent-shell-observer-list-open))
        (should (equal (plist-get (car agent-shell-observer--entries) :status) "Restoring…")))
      ;; ...and leaves its drop-down closed, the row saying so meanwhile.
      (with-current-buffer (agent-shell-observer--list-buffer)
        (agent-shell-observer--refresh-list)
        (should-not (agent-shell-observer-test--question-lines))
        (should (equal (car (car (agent-shell-observer-test--session-rows))) "Restoring"))
        (goto-char (point-min))
        (agent-shell-observer-list-toggle-session)
        (should (string-search "Bringing the conversation back" (buffer-string))))
      ;; Once it is back, the row says what the shell is doing.
      (agent-shell-observer-test--emit (plist-get (car agent-shell-observer--entries) :shell)
                                    'session-restored)
      (with-current-buffer (agent-shell-observer--list-buffer)
        (agent-shell-observer--refresh-list)
        (should-not (equal (car (car (agent-shell-observer-test--session-rows)))
                           "Restoring"))))))


;;; agent-shell-observer-test.el ends here
