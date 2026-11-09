;;; agent-shell-observer-test-helpers.el --- Fake agent shells for tests -*- lexical-binding: t -*-

;; The shells are fakes: a buffer with the state an agent-shell keeps,
;; whose events the tests send themselves.  Commands built on
;; agent-shell-observer can use them for their own tests.

;;; Code:

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
  (file-name-directory (directory-file-name (file-name-directory
                                            (or load-file-name buffer-file-name)))))
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
  "Return what the list keeps of each prompt, session by session.
Each is (TIME STATUS AGENT LOCATION TITLE), newest first within its
session; the list shows sessions, not these."
  (with-current-buffer (agent-shell-observer--list-buffer)
    (agent-shell-observer--refresh-list))
  (mapcan (lambda (session)
            (mapcar (lambda (entry)
                      (list (plist-get entry :time)
                            (agent-shell-observer--short-status (plist-get entry :status))
                            (format "%s" (or (plist-get entry :agent) ""))
                            (or (plist-get entry :location) "")
                            (or (plist-get entry :title) (plist-get entry :summary) "")))
                    (plist-get session :entries)))
          (seq-filter #'agent-shell-observer--visible-p (agent-shell-observer--sessions))))

(defmacro agent-shell-observer-test-with-agents (&rest body)
  "Run BODY with fake shells for antigravity and codex in a fresh list."
  (declare (indent 0))
  `(let* ((project (file-name-as-directory (make-temp-file "agent-shell-observer-" t)))
          (file (expand-file-name "code.el" project))
          (agent-shell-observer-start-seconds 0.2)
          (agent-shell-observer--entries nil)
          (agent-shell-observer--next-id 0)
          (agent-shell-observer-history-file (expand-file-name "history.eld" project))
          (agent-shell-observer--history-readable t)
          (agent-shell-observer--shown nil)
          (agent-shell-observer--opened (make-hash-table :test #'equal))
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

(defconst agent-shell-observer-test--agents '(antigravity codex)
  "The agents `agent-shell-observer-test--ask' sends to, most wanted first.")

(defun agent-shell-observer-test--project-shell ()
  "Return the project's shell of the most wanted agent, or start one."
  (or (car (seq-sort-by (lambda (shell)
                          (seq-position agent-shell-observer-test--agents
                                        (agent-shell-observer--agent-of shell)))
                        #'<
                        (seq-filter (lambda (shell)
                                      (memq (agent-shell-observer--agent-of shell)
                                            agent-shell-observer-test--agents))
                                    (agent-shell-project-buffers))))
      (agent-shell--start :config (agent-shell--resolve-config-designator
                                   (car agent-shell-observer-test--agents))
                          :no-focus t :new-session t)))

(defun agent-shell-observer-test--ask (line &optional question)
  "Send a prompt about LINE of the current buffer, as a command would.
QUESTION, when given, opens the prompt and names it in the list."
  (goto-char (point-min))
  (forward-line (1- line))
  (let ((location (format "code.el:%d" line)))
    (agent-shell-observer-submit
     (agent-shell-observer-test--project-shell)
     (format "%s\n\n%s\n```emacs-lisp\n%s\n```" (or question "Explain this code.") location
             (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
     :file (buffer-file-name) :line line :location location :title question)))

(defun agent-shell-observer-test--settle (seconds)
  "Let timers run for SECONDS."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end)
      (accept-process-output nil 0.02))))

(provide 'agent-shell-observer-test-helpers)
;;; agent-shell-observer-test-helpers.el ends here
