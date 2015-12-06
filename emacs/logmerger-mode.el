(require 'cl)

(defconst logmerger-re-begin-entry
  "^~~~ ")

(defconst logmerger-re-any-date
  "[0-9]\\{4,\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")

(defconst logmerger-re-any-time
  "\\([0-9]\\{2\\}:\\)\\{2\\}[0-9]\\{2\\}")

(defconst logmerger-re-any-origin
  ".*")

(cl-defstruct logmerger-entry timestamp origin)

(cl-defstruct logmerger-origin (boring nil) face)

(defmacro logmerge-transform-record (getter setter field transform)
  )

(defmacro logmerger-conf (table key &optional field area transform)
  "Get value from the ``table''. Table may be default or buffer-local.\
  \Buffer local bindings override default ones unless ``area'' is\
  \specified explicitly."
  (let* ((table-s (concat "logmerger-" (symbol-name table)))
         (table-prefixed (intern table-s))
         (accessor (intern (concat table-s "-" (symbol-name field))))
         (trans (if transform
                    transform
                  `(lambda (a) a)))
         (access (if field
                     accessor
                   `(lambda (a) a)))
         (read-tab-local `(cdr (assoc-string ,key ,table-prefixed)))
         (read-tab-default `(cdr (assoc-string ,key (default-value (quote ,table-prefixed)))))
         (read-table (pcase area
                       (`nil  `(let ((local ,read-tab-local)
                                     (default ,read-tab-default))
                                 (if local
                                     local
                                   default)))
                       (`default read-tab-default)
                       (`local read-tab-local)))
         (only-access `(let ((val ,read-table))
                         (if val
                             (,access val))))
         (transform-local `())
         (do-transform ()))
    (if transform
        do-transform
      only-access)))
   
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
      (make-logmerger-entry :timestamp date :origin origin))))

(defvar logmerger-highlights
  '((logmerger-re-begin-entry . font-lock-string-face)
    (logmerger-re-line-begin . font-lock-comment-face)))

;;;; Moving around ;;;;

(defmacro logmerger-go (direction stopp n)
  "Jump to the next or previous entry"
  (let ((jump (pcase direction
                (`forward `(progn (end-of-line)
                                  (re-search-forward logmerger-re-entry-header)))
                (`backward `(progn (beginning-of-line)
                                   (re-search-backward logmerger-re-entry-header))))))
    `(progn
       (let 
           ((old-state (logmerger-current-entry-info)))
         ,jump
         (dotimes (i ,n)
           (while (not (funcall ,stopp old-state (logmerger-current-entry-info)))
             ,jump))
         (recenter)))))

(defmacro logmerger-defgo (fun-name description key direction stopp)
  (let ((full-name (intern (concat "logmerger-go-" (symbol-name direction) fun-name))))
    `(progn 
       (defun ,full-name (n)
         ,(concat "Go " (symbol-name direction) description)
         (interactive "p")
         (logmerger-go ,direction ,stopp n))
       ,(when key
          `(define-key logmerger-mode-map (kbd ,key) (quote ,full-name))))))

;; Boring origins
(defun logmerger-boring-origin-set (p)
  "Declare current origin as ``boring''."
  (interactive "p")
  (if (= p 1)
      ()))
  

;; Just go back and forth

(logmerger-defgo "" "" "SPC" forward (lambda (a b) t))
(logmerger-defgo "" "" "<backspace>" backward (lambda (a b) t))

;; Go to the same origin
(defun logmerger-same-origin (a b)
  (equal (logmerger-entry-origin b) (logmerger-entry-origin a)))

(logmerger-defgo "-origin" " to an entry with the same origin" "RET"
                 forward 'logmerger-same-origin)
(logmerger-defgo "-origin" " to an entry with the same origin" "S-RET"
                 backward 'logmerger-same-origin)

;; Time
(defmacro logmerger-go-dt (direction dt)
  `(lambda (a b)
     (let* ((ta (apply 'encode-time (logmerger-entry-timestamp a)))
            (tb (apply 'encode-time (logmerger-entry-timestamp b)))
            (this-dt ,(pcase direction
                        (`forward `(- tb ta))
                        (`backward `(- ta tb)))))
       (>= this-dt ,dt))))

(logmerger-defgo "-minute" "" "m" forward (logmerger-go-dt forward 60))

(define-derived-mode logmerger-mode text-mode "Log"
  "Major mode for viewing LogMerger output"
  (read-only-mode t)
  (setq font-lock-defaults '(() nil nil nil logmerger-highlights)))

(provide 'logmerger-mode)
