(cl-defstruct (pmi-data-project
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-project-new (rootpath type &optional active-config-key))
              )
  rootpath
  type
  active-config-key
  (configurations (make-hash-table :test 'equal)))

(cl-defstruct (pmi-data-configuration
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-configuration-new (name buildfolder &optional settings active-runconfig-key))
               )
  name
  buildfolder
  settings
  active-runconfig-key
  (runconfigurations (make-hash-table :test 'equal)))

(cl-defstruct (pmi-data-runconfiguration
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-runconfiguration-new (name execpath &optional args environment)))
  name execpath args environment)

(provide 'pmi-data)
