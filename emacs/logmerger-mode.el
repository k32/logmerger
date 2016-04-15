(require 'cl)
(require 'dash)

(defconst logmerger-re-begin-entry
  "^~~~ ")

(defconst logmerger-re-any-date
  "[0-9]\\{4,\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")

(defconst logmerger-re-any-time
  "\\([0-9]\\{2\\}:\\)\\{2\\}[0-9]\\{2\\}")

(defconst logmerger-re-any-origin
  ".*")

(defconst logmerger-re-line-begin
  "^|[^|]*|")

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

;; (defvar logmerger-highlights
;;    (list (list logmerger-re-begin-entry '(1 font-lock-string-face))
;;          (list logmerger-re-line-begin '(1 font-lock-comment-face))))

;(defcustom logmerger-severity-levels


(defface logmerger-error-face
  '((((class color) (min-colors 8))
     :background "red" :foreground "white")
    (t :inverse-video t))
  "Errors in the logs"
  :group 'logmerger)

(defvar logmerger-error-face 'logmerger-error-face
  "Face to display errors")

(defvar logmerger-line-begin-face 'font-lock-comment-face
  "Face to display ine beginnings")

(defface logmerger-severe-accident-face
  '((((class color) (min-colors 8))
     :background "yellow" :foreground "red")
    (t :inverse-video t))
  "Severe incident"
  :group 'logmerger)

(defvar logmerger-severe-accident-face 'logmerger-severe-accident-face
  "Face to display severe accidents")

(defface logmerger-user-intervention-face
  '((((class color) (min-colors 8))
     :foreground "green")
    (t :inverse-video t))
  "User intervention to the node"
  :group 'logmerger)

(defvar logmerger-user-intervention-face 'logmerger-user-intervention-face
  "Face to display user intervention")

(defface logmerger-entry-header-face
  '((((class color) (min-colors 8))
     :background "gray" :foreground "black")
    (t :inverse-video t))
  "Beginning of an entry"
  :group 'logmerger)

(defvar logmerger-entry-header-face 'logmerger-entry-header-face
  "Entry header")

(defcustom logmerger-user-intervention
  '("gsh:<[0-9]+.[0-9]+.[0-9]+>,.*"
    "\\(REPLY\\|REQUEST\\) message-id=\".*")
  "User intervention"
  :type '(repeat string)
  :group 'logmerger)

(defcustom logmerger-severe-accidents
  '("\\(node\\|small\\|large\\)_restart.*"
    "restart_node.*"
    "restart_dpe.*"
    "isp\\.log"
    "capsule_failure.*"
    "ncb_failover.*"
    )
  "Severe incidents"
  :type '(repeat string)
  :group 'logmerger)

(defcustom logmerger-errors
  '("\\(ERROR\\|CRASH\\) +\\(REPORT\\).*"
    "error")
  "Regex matching severe errors"
  :type '(repeat string)
  :group 'logmerger)

(defun logmerger-merge-regexes (l)
  (reduce
   (lambda (a b) (concat a "\\|" b))
   (mapcar
    (lambda (a) (concat "\\(" a "\\)"))
    l)))

(setq logmerger-highlights
      `((,logmerger-re-line-begin . logmerger-line-begin-face)
        (,(concat logmerger-re-begin-entry ".*") . logmerger-entry-header-face)
        (,(logmerger-merge-regexes logmerger-errors) . logmerger-error-face)
        (,(logmerger-merge-regexes logmerger-severe-accidents) . logmerger-severe-accident-face)
        (,(logmerger-merge-regexes logmerger-user-intervention) . logmerger-user-intervention-face)))

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
  (setq-local font-lock-defaults
              '(logmerger-highlights))
  (setq-local logmerger-origins '())
  (make-local-variable 'logmerger-symbols))

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

(defun logmerger-go-next-error (n)
  (interactive "p")
  (dotimes (i n)
    (re-search-forward (logmerger-merge-regexes logmerger-errors))))


(define-key logmerger-mode-map (kbd "e") 'logmerger-go-next-error)

(defun logmerger-go-prev-error (n)
  (interactive "p")
  (dotimes (i n)
    (re-search-backward (logmerger-merge-regexes logmerger-errors))))

(define-key logmerger-mode-map (kbd "E") 'logmerger-go-prev-error)

(defun logmerger-create-initialize-buffer (name hook)
  (unless (get-buffer name)
    (let ((prev-buffer (buffer-name)))
      (get-buffer-create name)
      (set-buffer name)
      (funcall hook)
      (set-buffer prev-buffer))))

(define-derived-mode logmerger-pattern-mode text-mode "Pattern"
  "Major mode for editing patterns")

(defun logmerger-pattern-run ()
  (interactive)
  (let* ((lines (split-string (buffer-string) "\n" t))
         (regexpp (lambda (str)
                    (string-match "^\\\\(.*\\\\)$" str)))
         (stringpp (lambda (str)
                     (string-match "^[^#]" str)))
         (simple-words (-filter (lambda (x) 
                                  (and (funcall stringpp x) (not (funcall regexpp x))))
                                lines))
         (regexes (-filter regexpp lines))
         (regex1 (regexp-opt simple-words))
         (regex2 (logmerger-merge-regexes (cons regex1 regexes))))
    (message "Regexes: %S" regexes)
    (message "Strings: %S" simple-words)
    (message "Regex2: %s" regex2)
    (set-buffer orig-buffer)
    (occur regex2)))
         
(define-key logmerger-pattern-mode-map (kbd "C-c C-c") 'logmerger-pattern-run)

(defun logmerger-occur ()
  (interactive)
  (let ((buf-name (concat "*" (buffer-name) "-pattern*"))
        (orig-buffer2 (buffer-name)))
    (logmerger-create-initialize-buffer buf-name
                                        (lambda ()
                                          (logmerger-pattern-mode)
                                          (setq-local orig-buffer orig-buffer2)
                                          (message orig-buffer)))
    (switch-to-buffer-other-window buf-name)))

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

(defun logmerger-add-symbol-to-overview ()
  (interactive)
  ())

(provide 'logmerger-mode)
