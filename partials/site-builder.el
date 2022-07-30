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


(require 'htmlize)
(require 'ox-publish)


(setq org-html-validation-link nil)
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head
      "<link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\" />
       <link rel=\"stylesheet\" href=\"style.css\" />")

(require 'preamble)
(require 'postamble)
(require 'formatting)


(provide 'site-builder)
;;; site-builder.el ends here
