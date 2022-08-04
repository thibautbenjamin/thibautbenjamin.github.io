;;; site-builder.el --- Builder for my website       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  thibaut

;; Author: thibaut <thibaut@thibaut-Precision-5550>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defcustom site-builder-sidepanel-pic "" nil)
(defcustom site-builder-sidepanel-description "" nil)
(defcustom site-builder-site-name "" nil)
(defcustom site-builder-author-name "" nil)
(defcustom site-builder-sidepanel-infos "" nil)
(defcustom site-builder-base-directory "" nil)
(defcustom site-builder-publishing-directory "" nil)
(defcustom site-builder-menu-order "" nil)
(defcustom site-builder-layout "" nil)

(require 'htmlize)
(require 'ox-publish)


(setq org-html-validation-link nil)
(setq org-html-head-include-default-style nil)

(defun site-builder-set-head ()
  (setq org-html-head
        (concat
        "<link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\" /> \n"
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/css/all.min.css\"> \n"
        "<link rel=\"stylesheet\" href=\"style.css\" /> \n"
        (when (equal site-builder-current-layout "talks") "<link rel=\"stylesheet\" href=\"talks.css\" />")
        )))


(setq org-html-head-include-scripts t)
(setq org-html-scripts
      "<script>
         function show(id) {
          var x = document.getElementById(id);
          if (x.className.indexOf(\"w3-show\") == -1) {
            x.className += \" w3-show\";
          } else {
            x.className = x.className.replace(\" w3-show\", \"\");
          }
        }

        function showPopup(id) {
          let container = document.getElementById(id);
          container.classList.add('show');
            container.addEventListener('click', (event) => {
            if (event.target.matches('.popupContainer')) {
              container.classList.remove('show');
            }
          });
        }
       </script>")

(setq org-html-content-class "content w3-container")

(customize-set-variable 'org-html-toplevel-hlevel 1)


(require 'preamble)
(require 'postamble)
(require 'formatting)
(require 'gen-bib)
(require 'infobar)

(defun site-builder-layout ()
  (let ((layout
         (save-excursion
           (goto-char 0)
           (when (re-search-forward "^# site\\-builder\\-layout:" nil t)
             (forward-word)
             (current-word)))))
    (or layout "default")))

(defun site-builder-set-format (backend)
  (setq site-builder-current-layout (site-builder-layout))
  (when (eq backend 'html)
    (customize-set-variable 'org-html-postamble-format (list (list "en" (site-builder-footer))))
    (site-builder-set-head)))

(customize-set-variable 'org-export-use-babel t)

(defun site-builder-build-site ()

  (setq org-publish-project-alist
        (list
         (list "org-site:main"
               :recursive t
               :base-directory site-builder-base-directory
               :publishing-function 'org-html-publish-to-html
               :publishing-directory site-builder-publishing-directory
               :with-author t
               :with-creator t
               :with-toc nil
               :section-numbers nil
               :time-stamp-file nil)))


  (customize-set-variable 'org-html-preamble t)
  (customize-set-variable 'org-html-preamble-format (list (list "en" (site-builder-menu))))

  (customize-set-variable 'org-html-postamble t)
  (add-hook 'org-export-before-processing-hook 'site-builder-set-format)

  (site-builder-set-head)

  (org-publish-all t))


(provide 'site-builder)
;;; site-builder.el ends here
