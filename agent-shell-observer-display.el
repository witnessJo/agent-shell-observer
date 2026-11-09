;;; agent-shell-observer-display.el --- Display Markdown in user messages -*- lexical-binding: t -*-

;; Copyright (C) 2026 witnessjo

;; Author: witnessjo <jongyoungcha@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Render committed user input with agent-shell's own Markdown engine, in
;; the buffer itself, as agent-shell renders the agent's replies: the
;; rendered text is text, so it can be selected as it is seen, and the
;; engine keeps each span's source for copying.  Input is rendered only
;; once it has been sent and taken into comint's history.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'map)

(declare-function agent-shell--render-markdown "agent-shell")
(declare-function agent-shell-markdown--emphasize-span "agent-shell-markdown")
(declare-function agent-shell-markdown--straddles-avoid-range-p "agent-shell-markdown")
(declare-function agent-shell-markdown-in-avoid-range-p "agent-shell-markdown")
(declare-function agent-shell-markdown-sort-ranges "agent-shell-markdown")
(declare-function shell-maker--find-marker "shell-maker")
(declare-function shell-maker--re-search-forward-prompt "shell-maker")
(defvar comint-last-input-start)
(defvar comint-last-input-end)
(defvar comint-prompt-regexp)

(defvar agent-shell-observer-user-blank-line-height 0.45
  "Height of blank lines in rendered user messages, relative to normal text.")

(defvar agent-shell-observer-line-spacing 2
  "Extra pixels between lines in agent conversation buffers.")

(defun agent-shell-observer--compact-line-spacing ()
  "Use compact line spacing locally in an agent conversation."
  (setq-local line-spacing agent-shell-observer-line-spacing))

(defvar agent-shell-observer--rendering-user-copy nil
  "Non-nil only while rendering a user message.")

(defun agent-shell-observer--hangul-p (character)
  "Whether CHARACTER is a Hangul letter or syllable."
  (and character (eq (aref char-script-table character) 'hangul)))

(defun agent-shell-observer--render-hangul-emphasis (regexp face avoid-ranges)
  "Render REGEXP emphasis next to Hangul, using FACE outside AVOID-RANGES.
The native engine handles escaping and supplies marker ranges protecting
code.  Its emphasis operation also preserves Markdown reconstruction."
  (let (changed destinations)
    (save-excursion
      ;; Destinations and bare URLs are data, even when a path contains stars.
      (goto-char (point-min))
      (while (re-search-forward
              "\\](\\([^)\n]*\\))\\|\\(?:https?://\\|www\\.\\)[^ \t\n<>]+" nil t)
        (push (cons (copy-marker (or (match-beginning 1) (match-beginning 0)))
                    (copy-marker (or (match-end 1) (match-end 0))))
              destinations))
      (setq avoid-ranges (agent-shell-markdown-sort-ranges avoid-ranges destinations))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((start (match-beginning 0)) (end (match-end 0))
              (body-start (match-beginning 2)) (body-end (match-end 2))
              (delimiter (match-string-no-properties 1)))
          (when (and (or (agent-shell-observer--hangul-p (char-before start))
                         (agent-shell-observer--hangul-p (char-after end)))
                     (not (memq (char-after body-start) '(?\s ?\t ?* ?_)))
                     (not (memq (char-before body-end) '(?\s ?\t ?* ?_)))
                     (not (agent-shell-markdown-in-avoid-range-p start end avoid-ranges))
                     (not (agent-shell-markdown--straddles-avoid-range-p start end avoid-ranges)))
            (agent-shell-markdown--emphasize-span
             :markup-start start :markup-end end :content-start body-start :content-end body-end
             :face (if (equal delimiter "***")
                       '(agent-shell-markdown-bold agent-shell-markdown-italic) face))
            (setq changed t)))))
    changed))

(defun agent-shell-observer--extend-bold-boundaries (original &rest args)
  "Supplement ORIGINAL's word boundaries while rendering user copies."
  (let ((changed (apply original args)))
    (if agent-shell-observer--rendering-user-copy
        (or (agent-shell-observer--render-hangul-emphasis
             "\\(\\*\\*\\*\\|\\*\\*\\|__\\)\\([^\n]+?\\)\\1"
             'agent-shell-markdown-bold (plist-get args :avoid-ranges)) changed)
      changed)))

(defun agent-shell-observer--extend-italic-boundaries (original &rest args)
  "Supplement ORIGINAL's word boundaries while rendering user copies."
  (let ((changed (apply original args)))
    (if agent-shell-observer--rendering-user-copy
        (or (agent-shell-observer--render-hangul-emphasis
             "\\([*_]\\)\\([^*_\n]+?\\)\\1"
             'agent-shell-markdown-italic (plist-get args :avoid-ranges)) changed)
      changed)))

(defun agent-shell-observer--compact-user-spacing ()
  "Collapse extra blank padding and shorten the remaining gaps in this copy.
Blank lines inside source blocks are left intact."
  (save-excursion
    (goto-char (point-min))
    (let ((blank-before nil))
      (while (< (point) (point-max))
        (let* ((start (point))
               (end (line-beginning-position 2))
               (blank (and (looking-at-p "[ \t]*$")
                           (not (get-text-property start 'agent-shell-markdown-source-block-body)))))
          (if (and blank blank-before)
              (delete-region start end)
            (when blank
              (put-text-property start end 'line-height agent-shell-observer-user-blank-line-height))
            (setq blank-before blank)
            (goto-char end)))))))

(defun agent-shell-observer--render-user-input (start end)
  "Render the Markdown of committed user input between START and END.
In the buffer itself, as agent-shell renders replies, so the rendering
can be selected and copied like a reply.  Input already rendered is
left alone, and input made read-only by `shell-maker--freeze-submitted-input'
stays so.  A failure leaves the input as written: it has been sent."
  (when (and (< start end)
             (not (get-text-property start 'agent-shell-observer-rendered)))
    (let ((start (copy-marker start))
          (end (copy-marker end))
          (read-only (get-text-property start 'read-only)))
      (condition-case err
          (let ((inhibit-read-only t)
                (buffer-undo-list t)
                (agent-shell-observer--rendering-user-copy t))
            (save-excursion
              (save-restriction
                (narrow-to-region start end)
                (agent-shell--render-markdown :complete t)
                (agent-shell-observer--compact-user-spacing)
                (put-text-property (point-min) (point-max) 'agent-shell-observer-rendered t)
                (when read-only
                  (add-text-properties (point-min) (point-max)
                                       '(read-only t front-sticky (read-only)))))))
        (error
         (message "Could not render user Markdown: %s" (error-message-string err))))
      (set-marker start nil)
      (set-marker end nil))))

(defun agent-shell-observer--display-submitted-markdown ()
  "Render just-committed input after comint has captured its raw text."
  (when (and (derived-mode-p 'agent-shell-mode)
             (markerp comint-last-input-start) (markerp comint-last-input-end))
    (save-excursion
      (agent-shell-observer--render-user-input comint-last-input-start comint-last-input-end))))

(defun agent-shell-observer-render-user-history ()
  "Render user messages already present in the current agent shell.
Use shell-maker's real prompt boundaries and end markers, so agent
answers, permission titles, and the editable live input are untouched."
  (interactive)
  (when (derived-mode-p 'agent-shell-mode)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (shell-maker--re-search-forward-prompt comint-prompt-regexp)
          (let* ((start (point))
                 (next (save-excursion
                         (if (shell-maker--re-search-forward-prompt comint-prompt-regexp)
                             (match-beginning 0) (point-max))))
                 (end (shell-maker--find-marker "<shell-maker-end-of-prompt>" next)))
            (when end (agent-shell-observer--render-user-input start (car end)))
            (goto-char next)))))))

(defun agent-shell-observer--display-restored-markdown (state)
  "Render committed user messages after STATE's history has been replayed."
  (when-let* ((buffer (map-elt state :buffer)) ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (agent-shell-observer-render-user-history))))

(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-mode-hook #'agent-shell-observer--compact-line-spacing)
  ;; Shells opened before this file was loaded: render their input too.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (agent-shell-observer-render-user-history))))
  (advice-add 'agent-shell-markdown--replace-bolds :around
              #'agent-shell-observer--extend-bold-boundaries)
  (advice-add 'agent-shell-markdown--replace-italics :around
              #'agent-shell-observer--extend-italic-boundaries)
  (advice-add 'shell-maker--freeze-submitted-input :after
              #'agent-shell-observer--display-submitted-markdown)
  (advice-add 'agent-shell--render-pending-restore :after
              #'agent-shell-observer--display-restored-markdown))

(provide 'agent-shell-observer-display)
;;; agent-shell-observer-display.el ends here
