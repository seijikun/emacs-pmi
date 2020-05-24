;; All variables / structs / methods used to declare a buildsystemtype

;;; Base-Configuration structure that stores all functions that define a buildsystemtype
(cl-defstruct (pmi-fntbl-buildsystemtype
  (:constructor pmi-fntbl-buildsystemtype-new))
  add-configuration init-configuration build-configuration
)

(provide 'pmi-buildsystemtype)
