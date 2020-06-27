;;; pmi.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2020-2020 Markus Ebner <info@ebner-markus.de>

;; Author: Markus Ebner <info@ebner-markus.de>
;; URL: https://github.com/seijikun/emacs-pmi
;; Keywords: project, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (pkg-info "0.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; TODO: Some commentary
;;
;;; Code:

(require 'pmi-helpers)
(require 'pmi-data)
(require 'pmi-buildsystem)

;; ################ CONFIGURATION ################

(defcustom pmi-conf-workspace-filepath (expand-file-name (locate-user-emacs-file ".pmi-workspace"))
  "Where to persist the workspace."
  :group 'pmi
  :type 'file)
  
(defcustom pmi-conf-projectsave-foldername ".emacs-pmi"
  "Foldername that is used within each project-root, to store PMI data."
  :group 'pmi
  :type 'string)

(defcustom pmi-conf-projectsave-filename "project.el"
  "Filename of the file, that will store a pmi project within a projects PMI folder."
  :group 'pmi
  :type 'string)


(defcustom pmi-buildsystem-autoload-list
  '(pmi-buildsystem-cmake)
  "List of the buildsystem implementations to be automatically required."
  :group 'pmi
  :type '(repeat symbol))


;; ################# GLOBALS #################

;; hashmap of open projects (projectroot => project)
(eval-when-compile
  ;; hashmap of known buildsystems (name => buildsystem struct)
  (defvar pmi--var-buildsystems)
  ;; hashmap of open projects (projectroot => project)
  (defvar pmi--var-projects)
  ;; mapping between project projectroot and cnt of open files (name => int)
  (defvar pmi--var-projects-open-file-cnts))
(setq pmi--var-buildsystems (make-hash-table :test 'equal))
(setq pmi--var-projects-open-file-cnts (make-hash-table :test 'equal))

;; ################# API-Surface #################
;; #### Buildsystem

(defun pmi-buildsystem-register (name fntbl)
  "Register a buildsystem with name (as NAME), and the implemented function table data-fntbl-buildsystem (as FNTBL)."
  (puthash name fntbl pmi--var-buildsystems))

;; ############### User-API Surface ##############

;; #### PROJECT
(defun pmi-add-project ()
  "Add an existing projectroot as new project to the PMI workspace."
	(interactive)
	(let* ((projectroot (read-directory-name "Projectroot: " nil "" t)))
      ; add project
      (pmi--add-project projectroot)))

(defun pmi-project-root ()
  "Get projectroot directory of the project, to which the current file belongs."
  (let* ((dir (pmi--buffer-directory-path))
         (projectroots (hash-table-keys pmi--var-projects))
         (best-match nil)
         (best-match-len 0))

    (cl-loop for projectroot in projectroots
             for match-len = (length projectroot)
             do (when (and (string-prefix-p projectroot dir) (> match-len best-match-len))
                  (setq best-match projectroot)
                  (setq best-match-len match-len))
             finally return best-match)))

(defun pmi-project ()
  "Get the currently active project."
  (let* ((projectroot (pmi-project-root)))
    (gethash projectroot pmi--var-projects)))

(defun pmi-project-type ()
  "Get type (buildsystem) of the project, to which the current file belongs."
  (let* ((projectroot (pmi-project-root)))
    (when projectroot (pmi-data-project-type (gethash projectroot pmi--var-projects)))))

(defun pmi-project-buildsystem ()
  "Get the buildsystem functiontable of the currently active project."
  (let* ((projecttype (pmi-project-type)))
    (gethash projecttype pmi--var-buildsystems)))

(defun pmi-project-active-configuration ()
  "Get the current project's active configuration."
  (let* ((project (pmi-project))
         (config-key (pmi-data-project-active-configuration project))
         (configs (pmi-data-project-configurations project)))
    (gethash config-key configs)))

(defun pmi-project-active-runconfiguration ()
  "Get the current project's active runconfiguration (depends on its active configuration)."
  (let* ((config (pmi-project-active-configuration))
         (runconfig-key (pmi-data-configuration-active-runconfiguration config))
         (runconfigs (pmi-data-configuration-runconfigurations config)))
    (gethash runconfig-key runconfigs)))

(defun pmi-project-info ()
  "Print some information about the project, to which the curent file belongs."
  (interactive)
  (message "Project-Root: %s | Project-Type: %s" (pmi-project-root) (pmi-project-type)))

(defun pmi-project-open ()
  "Open a file of a choosen known project."
  (interactive)
  (let ((new-root (completing-read "PMI switch project: " (hash-table-keys pmi--var-projects))))
    (read-file-name "Choose file to open: " new-root)))

(defun pmi-project-add-configuration ()
  "Open the add-configuration wizard for the currently open project."
  (interactive)
  (let* ((project (pmi-project))
         (configurations (pmi-data-project-configurations project))
         (projectbuildsystem (pmi-project-buildsystem))
         (configname "")
         (buildsystem-add-configuration (pmi-fntbl-buildsystem-add-configuration projectbuildsystem)))
    (when buildsystem-add-configuration
      ; ask user for a configuration name, until he picks one that's not yet in use.
      (loop (setq configname (read-string "New Configuration Name: "))
            (if (or (not (gethash configname (pmi-data-project-configurations project)))
                    (y-or-n-p "A configuration with this name already exists. Overwrite? "))
                (return configname)))
      (let* ((buildfolder (read-directory-name "New Buildfolder: " nil nil t))
             (configuration (pmi-data-configuration-new configname buildfolder)))
        (funcall buildsystem-add-configuration project configuration)
        (when (= (hash-table-count configurations) 0)
          (pmi--log-info "Changing default configuration to: %s" configname)
          (setf (pmi-data-project-active-configuration project) configname))
        (puthash configname configuration (pmi-data-project-configurations project))
        (pmi--project-save project)))))

(defun pmi-project-remove-configuration ()
  "Remove a configuration from the current project."
  (interactive)
  (let* ((project (pmi-project))
         (active-config (pmi-data-project-active-configuration project))
         (configurations (pmi-data-project-configurations project))
         (confignames (hash-table-keys configurations))
         (chosen-config (completing-read "Configuration to remove: " confignames nil t)))
    (pmi--log-info "Removing configuration: %s" configname)
    (remhash chosen-config configurations)
    (setf (pmi-data-project-active-configuration project) nil)
    (when (equal active-config chosen-config)
      (pmi-project-select-configuration))))

(defun pmi-project-select-configuration ()
  "Switch current active configuration."
  (interactive)
  (let* ((project (pmi-project))
         (configs (pmi-data-project-configurations project))
         (config-key (completing-read "Choose configuration:" (hash-table-keys configs))))
    (setf (pmi-data-project-active-configuration project) config-key)))

(defun pmi-project-select-runconfiguration ()
  "Select a runconfiguration for the active configuration of the active project."
  (interactive)
  (let* ((project (pmi-project))
         (configuration (pmi-data-project-active-configuration project))
         (runconfigs (pmi-data-configuration-runconfigurations configuration))
         (runconfig-key (completing-read "Choose configuration:" (hash-table-keys runconfigs))))
    (setf (pmi-data-configuration-active-runconfiguration configuration) runconfig-key)))

(defun pmi-project-configure ()
  "Run the configure step for the active configuration of the current project. (Many modern buildsystems don't have one though)."
  (interactive)
  (let* ((project (pmi-project))
         (buildsystem (pmi-project-buildsystem))
         (active-configuration (pmi-project-active-configuration)))
    (pmi--log-info "Launching Configure-Step for active project.")
    (funcall (pmi-fntbl-buildsystem-init-configuration buildsystem) project active-configuration)))

(defun pmi-project-build ()
  "Build the active configuration of the current project."
  (interactive)
  (let* ((project (pmi-project))
         (buildsystem (pmi-project-buildsystem))
         (active-configuration (pmi-project-active-configuration)))
    (pmi--log-info "Launching Build-Step for active project.")
    (funcall (pmi-fntbl-buildsystem-build-configuration buildsystem) project active-configuration)))

(defun pmi-project-run ()
  "Run the active configuration's selected runconfiguration of the current project."
  (interactive)
  (message "TODO"))

(file-name-directory (buffer-name (current-buffer)))
;; ################# Internal-API #################
(defun pmi--load-buildsystems ()
  (pmi--log-debug "Autoloading buildsystems %s" pmi-buildsystem-autoload-list)
  (seq-do (lambda (package)
            (unless (featurep package)
              (require package nil t)
              (pmi--log-debug "Loading buildsystem implementation: %s" package)))
          pmi-buildsystem-autoload-list))

(defun pmi--init ()
	(pmi--log-info "Initializing")
  ; register event handlers
	(add-hook 'after-load-functions #'pmi--evt-file-opened)
	(add-hook 'kill-buffer-hook #'pmi--evt-file-closed)

  (pmi--load-buildsystems)
	(pmi--workspace-load))

(defun pmi--deinit ()
	(pmi--log-info "Deinitializing")
	(remove-hook 'after-load-functions #'pmi--evt-file-opened)
	(remove-hook 'kill-buffer-hook #'pmi--evt-file-closed)
)

(defun pmi--workspace-load ()
  (let ((projectroots (pmi--deserialize pmi-conf-workspace-filepath)))
    (setq pmi--var-projects (make-hash-table :test 'equal))
    (if projectroots
      (progn ; then
        (cl-loop for projectroot in projectroots
                 for project = (pmi--project-load projectroot)
                 do (puthash projectroot project pmi--var-projects)
                 do (puthash projectroot 0 pmi--var-projects-open-file-cnts)))
      ; else
      (pmi--log-debug "No workspace found, starting empty"))))

(defun pmi--workspace-save ()
  (pmi--log-debug "Save workspace")
  (let* ((projectroots (hash-table-keys pmi--var-projects)))
    (pmi--serialize pmi-conf-workspace-filepath projectroots)))

(defun pmi--projectsave-folder (project-or-root)
  (let* ((projectroot project-or-root))
    (when (pmi-data-project-p project-or-root)
      (setq projectroot (pmi-data-project-rootpath project-or-root)))
    (file-name-as-directory (concat projectroot pmi-conf-projectsave-foldername))))

(defun pmi--projectsave-filepath (project-or-root)
  (concat (pmi--projectsave-folder project-or-root) pmi-conf-projectsave-filename))

(defun pmi--project-load (projectroot)
  (let ((projectfile (pmi--projectsave-filepath projectroot)))
    (pmi--deserialize projectfile)))

(defun pmi--project-save (project)
  (pmi--log-debug "Saving project: %s" (pmi--projectsave-filepath project))
  ; ensure the proejct's projectsave-folder is created within projectroot
  (make-directory (pmi--projectsave-folder project) t)
  (pmi--serialize (pmi--projectsave-filepath project) project))

(defun pmi--detect-projecttypes (projectroot)
  "Get a list of buildsystems that think they could handle the project in the given PROJECTROOT."
  (pmi--hashtable-filtermap pmi--var-buildsystems (lambda (buildsystem-name buildsystem)
                                                    (when (funcall (pmi-fntbl-buildsystem-directory-p buildsystem) projectroot)
                                                      buildsystem-name))))

(defun pmi--add-project (projectroot)
  (when (not (gethash projectroot pmi--var-projects))
    (let ((detected-projecttypes (pmi--detect-projecttypes projectroot)))
      (when (not detected-projecttypes) (error "No supported projecttypes / buildsystems found"))
      (let* ((projecttype (completing-read "Buildsystem-Type: " detected-projecttypes))
             (project (pmi-data-project-new projectroot projecttype)))
        (pmi--log-debug "Adding project: %s" projectroot)
        (puthash projectroot project pmi--var-projects)
        (puthash projectroot 0 pmi--var-projects-open-file-cnts)
        (pmi--project-save project)
        (pmi--workspace-save)))))

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
