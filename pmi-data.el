(cl-defstruct (pmi-data-project
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-project-new (rootpath type) :named)
               )
  rootpath
  type
  (configurations (make-hash-table :test 'equal) ))

(cl-defstruct (pmi-data-configuration
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-configuration-new (name buildfolder))
               )
  name buildfolder settings runconfigurations)
(cl-defstruct (pmi-data-runconfiguration
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-runconfiguration-new (name execpath &optional args environment)))
  name execpath args environment)

(provide 'pmi-data)
