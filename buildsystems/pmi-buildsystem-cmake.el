;;; pmi.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2020-2020 Markus Ebner <info@ebner-markus.de>

;; Author: Marius Mrfka <marius.mrfka@gmail.com>
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

(cl-defstruct (pmi-buildsystem-cmake-settings
               (:constructor nil)
               (:constructor pmi-data-cmake-settings-new ())
               (:constructor pmi-data-cmake-settings-new (configure-arguments build-arguments &optional (variables (make-hash-table :test 'equal))) :named))
  variables configure-arguments build-arguments
  )

(defun pmi-buildsystem-cmake-add-configuration (project configuration)
  ""
  (let* ((settings (pmi-data-configuration-settings configuration))
         (new-settings (if (pmi-buildsystem-cmake-settings-p settings) settings (pmi-data-cmake-settings-new)))
         (variables (read-string "Cmake variables: " (pmi-buildsystem-cmake-settings-variables new-settings)))
         (configure-arguments (read-string "Cmake configuration arguments: " (pmi-buildsystem-cmake-settings-configure-arguments new-settings)))
         (build-arguments (read-string "Build arguments: " (pmi-buildsystem-cmake-settings-build-arguments new-settings))))
    (setf (pmi-buildsystem-cmake-settings-variables new-settings) variables)
    (setf (pmi-buildsystem-cmake-settings-configure-arguments new-settings) configure-arguments)
    (setf (pmi-buildsystem-cmake-settings-build-arguments new-settings) build-arguments)
    (setf (pmi-data-configuration-settings configuration) new-settings)))

(defun pmi-buildsystem-cmake-init (project configuration) ; TODO: use buffer from parameter
  ""
  (let* ((projectRoot (pmi-data-project-rootpath project))
         (buildFolder (pmi-data-configuration-buildfolder configuration))
         (settings (pmi-data-configuration-settings configuration))
         (variables (pmi-buildsystem-cmake-settings-variables settings))
         (configure-args (pmi-buildsystem-cmake-settings-configure-arguments settings))
         (cmake-command (pmi--strjoin " " (list "cmake" "-S" projectRoot "-B" buildFolder variables configure-args)))
         (default-directory buildFolder))
  (start-process-shell-command "cmake-config-process" "*PMI-Configuration*" cmake-command)
  ))

(defun pmi-buildsystem-cmake-build (project configuration)
  ""
  (interactive)
  (let* ((default-directory (pmi-data-configuration-buildfolder configuration))
         (settings (pmi-data-configuration-settings configuration))
         (build-arguments (pmi-buildsystem-cmake-settings-build-arguments settings))
         (compile-command (concat "cmake " "--build " default-directory " " build-arguments)))
    (compile compile-command)))

(defun pmi-buildsystem-cmake-directory-p (directory-path)
  ""
    (and
     (file-exists-p directory-path)
     (file-directory-p directory-path)
     (not (null (seq-contains (directory-files directory-path) "CMakeLists.txt")))))

(defvar pmi-fntbl-buildsystem-cmake (pmi-fntbl-buildsystem-new))

(setf (pmi-fntbl-buildsystem-add-configuration pmi-fntbl-buildsystem-cmake) 'pmi-buildsystem-cmake-add-configuration)
(setf (pmi-fntbl-buildsystem-init-configuration pmi-fntbl-buildsystem-cmake) 'pmi-buildsystem-cmake-init)
(setf (pmi-fntbl-buildsystem-build-configuration pmi-fntbl-buildsystem-cmake) 'pmi-buildsystem-cmake-build)
(setf (pmi-fntbl-buildsystem-directory-p pmi-fntbl-buildsystem-cmake) 'pmi-buildsystem-cmake-directory-p)

(provide 'pmi-buildsystem-cmake)
;;; pmi-buildsystem-type-cmake.el ends here
