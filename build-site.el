;;; build-site.el --- Building my website            -*- lexical-binding: t; -*-

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

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)

(add-to-list 'load-path (concat default-directory "partials/"))
(require 'site-builder)

(customize-set-variable 'org-confirm-babel-evaluate nil)
(customize-set-variable 'org-babel-result-wrap "%s")

(customize-set-variable 'site-builder-base-directory "./content")
(customize-set-variable 'site-builder-denote-directory "./content/notes/")
(customize-set-variable 'site-builder-publishing-directory "./public")
(customize-set-variable 'site-builder-extras-directory "./files")

(customize-set-variable 'site-builder-menu-order '("notes.org" "software.org" "talks.org" "research.org"))

(customize-set-variable 'site-builder-site-name "THIBAUT BENJAMIN")
(customize-set-variable 'site-builder-author-name "Thibaut Benjamin")
(customize-set-variable 'site-builder-sidepanel-pic "pic.jpeg")
(customize-set-variable 'site-builder-sidepanel-description "Post doctorate researcher in computer science at CEA LIST")
(customize-set-variable
 'site-builder-sidepanel-infos
 '(("cv" "cv.pdf" "fa-solid fa-file-lines")
   ("email" "mailto:thibaut.benjamin@polytechnique.edu" "fa-solid fa-envelope")
   ("thibautbenjamin" "http://www.github.com/thibautbenjamin" "fa-brands fa-github")
   ("Thibaut Benjamin" "https://www.researchgate.net/profile/Thibaut-Benjamin" "fa-brands fa-researchgate")
   ("0000-0002-9481-1896" "https://orcid.org/0000-0002-9481-1896" "fa-brands fa-orcid")))


(customize-set-variable 'site-builder-infobar-items
                        '(("gitlab" . (icon . "fa-brands fa-gitlab"))
                          ("github" . (icon . "fa-brands fa-github"))
                          ("interactive" . (icon . "fa-solid fa-terminal"))
                          ("slides" . (icon . "fa-solid fa-display"))
                          ("youtube" . (icon . "fa-brands fa-youtube"))))

(site-builder-build-site)

(message "build complete")

(provide 'build-site)
;;; build-site.el ends here
