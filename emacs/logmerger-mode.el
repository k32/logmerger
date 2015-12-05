(defconst logmerger-re-begin-entry
  "^~~~ ")

(defconst logmerger-re-date
  "[0-9]\\{4,\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")

(defconst logmerger-re-time
  "\\([0-9]\\{2\\}:\\)\\{2\\}[0-9]\\{2\\}")

(defconst logmerger-re-origin
  "(.*)")

(defun test-re ()
  "Don't mind me"
  (interactive)
  (re-search-forward (concat "\\(?1:" logmerger-re-time "\\)")))

(defconst logmerger-re-entry-header
  (concat logmerger-re-begin-entry
          "\\(?1:" ; Beginning of date-time
          logmerger-re-date
          " "
          logmerger-re-time
          "\\)"    ; End of date-time
          " "
          "\\(?2:" logmerger-re-origin "\\)"
          " ~~~$"))

(defun logmerger-current-entry-info ()
  "Find out date, time and origin of the log entry at the point"
  (save-excursion (re-search-backward logmerger-re-entry-header)
                  (let ((date (parse-time-string (match-string 1)))
                        (origin (match-string 2)))
                    (list date origin))))

(defun logmerger-format-mode-line ()
  "Format mode line according to the current log entry"
  (interactive)
  (let* ((date-origin (logmerger-current-entry-info))
         (date (car date-origin))
         (origin (car (cdr date-origin))))
    (print date)))

(defvar logmerger-highlights
  '((logmerger-re-begin-entry . font-lock-string-face)
    (logmerger-re-line-begin . font-lock-comment-face)))

(defun logmerger-to-next-entry (n)
  "Jump to the next log entry"
  (interactive "p")
  (re-search-forward logmerger-re-entry-header)
  (recenter))

(defun logmerger-to-prev-entry (n)
  "Jump to the previous log entry"
  (interactive "p")
  (re-search-backward logmerger-re-entry-header)
  (recenter))

(define-derived-mode logmerger-mode text-mode "Log"
  "Major mode for viewing LogMerger output"
  (read-only-mode t)
  (setq font-lock-defaults '(() nil nil nil logmerger-highlights)))

(define-key logmerger-mode-map
  (kbd "SPC") 'logmerger-to-next-entry)

(define-key logmerger-mode-map
  [backspace] 'logmerger-to-prev-entry)

(provide logmerger-mode)
