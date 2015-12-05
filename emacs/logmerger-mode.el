(defconst logmerger-re-begin-entry
  "^~~~ ")

(defconst logmerger-re-any-date
  "[0-9]\\{4,\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")

(defconst logmerger-re-any-time
  "\\([0-9]\\{2\\}:\\)\\{2\\}[0-9]\\{2\\}")

(defconst logmerger-re-any-origin
  ".*")

(defun logmerger-make-header-re (date time origin)
  "Construct a regular expression matching specific or any entry header"
  (concat logmerger-re-begin-entry
          "\\(?1:" ; Beginning of date-time
          date " " time
          "\\)"    ; End of date-time
          " (\\(?2:" origin "\\))"
          " ~~~$"))

(defconst logmerger-re-entry-header
  (logmerger-make-header-re logmerger-re-any-date
                            logmerger-re-any-time
                            logmerger-re-any-origin))

(defun logmerger-current-entry-info ()
  "Find out date, time and origin of the log entry at the point"
  (save-excursion
    (end-of-line)
    (re-search-backward logmerger-re-entry-header)
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

(defun logmerger-jump (direction n)
  "Jump to the next or previous entry"
  (funcall direction logmerger-re-entry-header)
  (recenter))

(defun logmerger-jump-same-origin (direction n)
  "Jump to the next or previous log entry with the same origin"
  (let* ((date-time-origin (logmerger-current-entry-info))
         (origin (car (cdr date-time-origin))))
    (funcall direction (logmerger-make-header-re
                       logmerger-re-any-date
                       logmerger-re-any-time
                       origin)))
  (recenter))

(defun logmerger-to-next-entry (n)
  "Jump to the next log entry"
  (interactive "p")
  (logmerger-jump 're-search-forward n))

(defun logmerger-to-prev-entry (n)
  "Jump to the previous log entry"
  (interactive "p")
  (logmerger-jump 're-search-backward n))

(defun logmerger-to-next-same-origin (n)
  "Jump to the next log entry"
  (interactive "p")
  (logmerger-jump-same-origin 're-search-forward n))

(defun logmerger-to-prev-same-origin (n)
  "Jump to the previous log entry"
  (interactive "p")
  (logmerger-jump-same-origin 're-search-backward n))

(define-derived-mode logmerger-mode text-mode "Log"
  "Major mode for viewing LogMerger output"
  (read-only-mode t)
  (setq font-lock-defaults '(() nil nil nil logmerger-highlights)))

(define-key logmerger-mode-map
  (kbd "SPC") 'logmerger-to-next-entry)

(define-key logmerger-mode-map
  (kbd "S-SPC") 'logmerger-to-next-same-origin)

(define-key logmerger-mode-map
  (kbd "S-<backspace>") 'logmerger-to-prev-same-origin)

(define-key logmerger-mode-map
  [backspace] 'logmerger-to-prev-entry)

(provide 'logmerger-mode)
