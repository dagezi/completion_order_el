;;; completion-order.el
;;; customize the order of file name list on completion
;;; This should work read-file-name, 
;;; dagezi@gmail.com (SASAKI, Takesi)

(defvar completion-sort-function nil)

;;; Override minibuffer-completion-help to customise order
(defun minibuffer-completion-help ()
  "Display a list of possible completions of the current minibuffer contents."
  (interactive)
  (message "Making completion list...")
  (lexical-let* ((start (field-beginning))
                 (end (field-end))
		 (string (field-string))
		 (completions (completion-all-completions
			       string
			       minibuffer-completion-table
			       minibuffer-completion-predicate
			       (- (point) (field-beginning)))))
    (message nil)
    (if (and completions
             (or (consp (cdr completions))
                 (not (equal (car completions) string))))
        (let* ((last (last completions))
               (base-size (cdr last))
               ;; If the *Completions* buffer is shown in a new
               ;; window, mark it as softly-dedicated, so bury-buffer in
               ;; minibuffer-hide-completions will know whether to
               ;; delete the window or not.
               (display-buffer-mark-dedicated 'soft))
          (with-output-to-temp-buffer "*Completions*"
            ;; Remove the base-size tail because `sort' requires a properly
            ;; nil-terminated list.
            (when last (setcdr last nil))
            (setq completions 
		  (if completion-sort-function 
		      (funcall completion-sort-function completions)
		      (sort completions 'string-lessp)))
            (when completion-annotate-function
              (setq completions
                    (mapcar (lambda (s)
                              (let ((ann
                                     (funcall completion-annotate-function s)))
                                (if ann (list s ann) s)))
                            completions)))
            (with-current-buffer standard-output
              (set (make-local-variable 'completion-base-position)
                   (list (+ start base-size)
                         ;; FIXME: We should pay attention to completion
                         ;; boundaries here, but currently
                         ;; completion-all-completions does not give us the
                         ;; necessary information.
                         end)))
            (display-completion-list completions)))

      ;; If there are no completions, or if the current input is already the
      ;; only possible completion, then hide (previous&stale) completions.
      (minibuffer-hide-completions)
      (ding)
      (minibuffer-message
       (if completions "Sole completion" "No completions")))
    nil))

(defadvice file-name-all-completions (after add-directory-propeties 
					    (file directory))
  "Adds default directory to returned strings as properties"
  (let ((dir 
	 (expand-file-name 
	  (save-match-data
	    (string-match "/*$" directory)
	    (substring directory 0 (match-beginning 0))))))
    (mapcar #'(lambda (filename) 
		(add-text-properties 0 (length filename)
				     (list 'completion-directory dir)
				   filename)
		filename)
	  ad-return-value)))
(ad-activate 'file-name-all-completions)

(defadvice read-file-name (around completion-sort-by-modtime)
  (let ((completion-sort-function #'completion-order-sort-by-modtime)
	(completion-annotate-function #'completion-order-annotate-modtime))
    ad-do-it))
(ad-activate 'read-file-name)

(defun get-modtime (file)
  (float-time (nth 5 (file-attributes file))))

(defun completion-order-sort-by-modtime (completions)
  "Sort file name completion candidates by its last modification time."
  ;; Completions has only basename. How to get directory?
  (let
      ((comps-with-modtime 
	(mapcar 
	 #'(lambda (comp)
	     (let ((dir (get-text-property 0 'completion-directory comp)))
	       (cons comp 
		     (if dir (get-modtime (concat dir "/" comp)) 0))))
	 completions)))
    (mapcar #'car 
	    (sort comps-with-modtime
		  #'(lambda (first second)
		      (> (cdr first) (cdr second)))))))

(defun format-period (seconds)
    (cond
     ((< seconds 0) "(-)")
     ((< seconds 60) (format "(%ds)" seconds))
     ((< seconds (* 60 60)) (format "(%dm)" (/ seconds 60)))
     ((< seconds (* 24 60 60)) (format "(%dh)" (/ seconds 60 60)))
     ((< seconds (* 7 24 60 60)) (format "(%dd)" (/ seconds 60 60 24)))
     ((< seconds (* 365.0 24 60 60)) (format "(%dw)" (/ seconds 60 60 24 7)))
     (t (format "(%dy)" (/ seconds 60 60 24 365)))))
(when nil
  (and (equal "(-)" (format-period -1))
       (equal "(5s)" (format-period 5.5))
       (equal "(3m)" (format-period 190))
       (equal "(4h)" (format-period (+ (* 4 3600) 1234)))
       (equal "(5d)" (format-period (+ (* 5 24 3600) 72334)))
       (equal "(1w)" (format-period (+ (* 7.0 24 3600) 72334)))
       (equal "(2y)" (format-period (+ (* 2.0 365 24 3600) 72334)))
       ))

(defun completion-order-annotate-modtime (s)
  "Annotates filename complete candidates with its modtime"
  (let* ((dir (get-text-property 0 'completion-directory s)))
    (if dir 
	(concat " " (format-period
		     (- (float-time) (get-modtime (concat dir "/" s)) -1)))
      "")))
