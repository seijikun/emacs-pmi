(cl-defstruct (pmi-data-project
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-project-new (rootpath type &optional active-configuration))
              )
  rootpath
  type
  active-configuration
  (configurations (make-hash-table :test 'equal)))

(cl-defstruct (pmi-data-configuration
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-configuration-new (name buildfolder &optional settings active-runconfiguration))
               )
  name
  buildfolder
  settings
  active-runconfiguration
  (runconfigurations (make-hash-table :test 'equal)))

(cl-defstruct (pmi-data-runconfiguration
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-runconfiguration-new (name execpath &optional args environment)))
  name execpath args environment)

(provide 'pmi-data)
