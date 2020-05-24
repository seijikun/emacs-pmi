(cl-defstruct (pmi-data-project
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-project-new (name type rootpath) :named)
               )
  name type rootpath configurations
)
(cl-defstruct (pmi-data-configuration
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-configuration-new (name buildfolder))
               )
  name buildfolder settings runconfigurations)
(cl-defstruct (pmi-data-runconfiguration
               (:constructor nil) ; no default constructor
               (:constructor pmi-data-runconfiguration-new (name execpath &optional args &optional environment)))
  name execpath args environment)
  

(provide 'pmi-data)
