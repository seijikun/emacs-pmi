(require 'pmi-helpers)
(require 'pmi-data)
(require 'pmi-buildsystemtype)

;; ################ CONFIGURATION ################

(defcustom pmi-conf-workspace-file (expand-file-name (locate-user-emacs-file ".pmi-workspace"))
  "Where to persist the workspace"
  :group 'pmi
  :type 'file)
  
(defcustom pmi-conf-project-folder (expand-file-name (locate-user-emacs-file ".emacs-pmi"))
  "Foldername that is used within each project-root, to store PMI data"
  :group 'pmi
  :type 'file)


;; ################# GLOBALS #################

;; hashmap of open projects (name => project)
(defvar pmi--var-projects)

;; mapping between project name and cnt of open files (name => int)
(defvar pmi--var-projects-open-file-cnts)


;; ################# API-Surface #################

(defun pmi-current-project ())
(defun pmi-current-project-type ())

(defun pmi-project-add-configuration ())		; add new configuration to project
(defun pmi-project-select-configuration ())		; switch current active configuration
(defun pmi-project-configure ())				; run configure (cmake ..)
(defun pmi-project-remove-coniguration ())		; remove configuration from project

(defun pmi-project-build ())
(defun pmi-project-run ())


;; ################# Internal-API #################

(defun pmi--init ()
	(pmi--log-info "Initializing")
	; register event handlers
	(add-hook 'after-load-functions #'pmi--evt-file-opened)
	(add-hook 'kill-buffer-hook #'pmi--evt-file-closed)
	
	(pmi--load-workspace)
	
	;TODO: load workspace
	;TODO: populate pmi--var-projects-open-file-cnts with zeros
)
(defun pmi--deinit ()
	(pmi--log-info "Deinitializing")
	(remove-hook 'after-load-functions #'pmi--evt-file-opened)
	(remove-hook 'kill-buffer-hook #'pmi--evt-file-closed)
)

(defun pmi--load-workspace ()
  (let (projectroots (pmi--deserialize pmi-conf-workspace-file) )
    (setq pmi--var-projects (make-hash-table :test 'equal))
    (if projectroots
      (progn ; then
        (cl-loop for projectroot in projectroots
                 for project = (pmi--load-project projectroot)
                 do (puthash (pmi-data-project-name project) project pmi--var-projects)
        )
        (print pmi--var-projects) ; TODO: remove (& test)
      )
      (progn ; else
        (pmi--log-debug "No workspace found, starting empty")
      )
))
)
(defun pmi--load-project (projectroot)
  (let (projectfile (concat projectroot "/" pmi-conf-project-folder "/project.el"))
    (pmi--deserialize projectfile)
  )
)
(defun pmi--save-workspace ()
	; TODO
)

;; ################# Event-Handlers #################
;; see: https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html#Standard-Hooks

(defun pmi--evt-file-opened (filepath)
	;TODO: if pmi-projects-open-file-cnts == 0, load project
	;TODO: increase pmi-projects-open-file-cnts
)

(defun pmi--evt-file-closed ()
	;TODO: decrease pmi-projects-open-file-cnts
	;TODO: save / unload project if cnt == 0
)



;;;###autoload
(define-minor-mode pmi-mode
  "Minor mode to assist project management and navigation.
When called interactively, toggle `pmi-mode'.
When called from Lisp, enable `pmi-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `pmi-mode'.
Otherwise behave as if called interactively."
  :group 'pmi
  :require 'pmi
  :global t
  (if pmi-mode
    (pmi--init)		; then
    (pmi--deinit)	; else
  )
)


(provide 'pmi)
