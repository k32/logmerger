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

(defmacro logmerger-read-table-record (table field key)
  (let ((val (make-symbol "val")))
      `(let ((,val (cdr (assoc-string ,key ,table))))
         (if ,val
             (,field val)
           nil))))

(defmacro logmerger-update-table-record (table field key function)
  (let* ((lookuper `(cdr (assoc-string ,key ,table)))
         (accessor `(,field ,lookuper))
         (entry (make-symbol "entry"))
         (entry2 (make-symbol "entry2")))
    `(let* ((,entry ,lookuper)
            (,entry2 (if entry
                         (funcall ,function (,field ,entry))
                       (funcall ,function nil))))
       (if ,entry
           (setf ,accessor ,entry2)))))

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
      (unless (assoc-string origin logmerger-origins)
        (add-to-list 'logmerger-origins `(,origin ,(make-logmerger-origin
                                                    :boring nil
                                                    :face nil))))
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
                                   (re-search-backward logmerger-re-entry-header)))))
        (old-state (make-symbol "old-state")))
    `(progn
       (let 
           ((,old-state (logmerger-current-entry-info)))
         ,jump
         (dotimes (i ,n)
           (while (not (funcall ,stopp ,old-state (logmerger-current-entry-info)))
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
(defun logmerger-toggle-boring-origin (p)
  "Declare current origin as `boring'"
  ;; Nasty hack: current-entry-info initializes origin entry for us
  ;; update-table-record is a better place for this
  (interactive "p")
  (let* ((origin (logmerger-entry-origin (logmerger-current-entry-info)))
         (update-fun (lambda (current-status)
                       (if current-status
                           (progn (message "%s is not boring anymore" origin)
                                  nil)
                         (progn (message "%s is now boring" origin)
                                t)))))
    (logmerger-update-table-record logmerger-origins
                                   logmerger-origin-boring
                                   origin
                                   update-fun)))

(define-derived-mode logmerger-mode text-mode "Log"
  "Major mode for viewing LogMerger output"
  (read-only-mode t)
  (setq font-lock-defaults '(() nil nil nil logmerger-highlights))
  (setq logmerger-origins '()))

(define-key logmerger-mode-map (kbd "b") 'logmerger-toggle-boring-origin)

;; Just go back and forth skipping "boring" entries

(defun logmerger-go-not-boring(a o)
  "Match entries which are not boring")

(logmerger-defgo "" "" "SPC" forward
                 (lambda (a b) t))
(logmerger-defgo "" "" "<backspace>" backward
                 (lambda (a b) t))

;; Go to the same origin
(defun logmerger-same-origin (a b)
  (equal (logmerger-entry-origin b) (logmerger-entry-origin a)))

(logmerger-defgo "-origin" " to an entry with the same origin" "]"
                 forward 'logmerger-same-origin)
(logmerger-defgo "-origin" " to an entry with the same origin" "["
                 backward 'logmerger-same-origin)

;; Time
;; (defmacro logmerger-go-dt (direction dt)
;;   (let ((ta (make-symbol "ta"))
;;         (tb (make-symbol "tb")))
;;   `(lambda (a b)
;;      (let* ((,ta (apply 'encode-time (logmerger-entry-timestamp a)))
;;             (,tb (apply 'encode-time (logmerger-entry-timestamp b)))
;;             (this-dt ,(pcase direction
;;                         (`forward `(- ,tb ,ta))
;;                         (`backward `(- ,ta ,tb)))))
;;        (>= this-dt ,dt)))))

(logmerger-defgo "-minute" "" "m" forward (logmerger-go-dt forward 60))

(provide 'logmerger-mode)
