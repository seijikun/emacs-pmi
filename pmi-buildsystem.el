;; All variables / structs / methods used to declare a buildsystemtype

;;; Base-Configuration structure that stores all functions that define a buildsystemtype
(cl-defstruct (pmi-fntbl-buildsystem
               (:constructor pmi-fntbl-buildsystem-new))
  add-configuration ;; (project configuration)
  init-configuration ;; (project configuration)
  build-configuration ;; (project configuration)
  directory-p ;; (directory-path)
  )

(provide 'pmi-buildsystem)
