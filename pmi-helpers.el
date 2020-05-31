;;; serialize
(defun pmi--serialize (filename data)
  (with-temp-file filename
    (erase-buffer) ; clean buffer (contains default newline)
    (insert (prin1-to-string data)) ; serialize data into currently open file
  )
)

;;; deserialize
(defun pmi--deserialize (filename)
  (when (file-exists-p filename)
	(with-temp-buffer
      (insert-file-contents filename)
        (cl-first
          (read-from-string (buffer-substring-no-properties (point-min) (point-max)))))))
     
     
;;; hash-table
(defun pmi--hashtable-find (map findfn)
  (cl-loop for key in (hash-table-keys map)
           for value = (gethash key map)
           for matchresult = (funcall findfn key value)
           until matchresult
           finally return (if matchresult key nil)))
(defun pmi--hashtable-find-value (map findfn) (cdr (pmi--hashtable-find map findfn)))
(defun pmi--hashtable-find-key (map findfn) (car (pmi--hashtable-find map findfn)))

(defun pmi--hashtable-filtermap (map mapfn)
  (let ((resultseq nil))
    (cl-loop for key in (hash-table-keys map)
             for value = (gethash key map)
             for mapresult = (funcall mapfn key value)
             do (when mapresult (push mapresult resultseq))
             finally return resultseq)))


;;; pmi-log
(defun pmi--log (loglevel msgtpl args)
  (apply 'message (concat "[PMI:%s] " msgtpl) loglevel args)
)
(defun pmi--log-debug (msgtpl &rest args) (pmi--log "D" msgtpl args))
(defun pmi--log-info (msgtpl &rest args) (pmi--log "I" msgtpl args))

;;; path handling
(defun pmi--folder-name (folder)
  (file-name-nondirectory (directory-file-name folder))
)

(defun pmi--buffer-directory-path (&optional buffer-or-name)
  "Return the directory-path associated with BUFFER or nil."
  (when (null buffer-or-name) (setq buffer-or-name (current-buffer)))
  (with-current-buffer buffer-or-name default-directory))

;;; string-manipulation
(defun pmi--strjoin (sep lst) (mapconcat 'identity lst sep))

(provide 'pmi-helpers)
