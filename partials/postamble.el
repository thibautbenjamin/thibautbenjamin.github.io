;;; postamble.el --- Postamble config for my website  -*- lexical-binding: t; -*-

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

(require 'mk-html)

(customize-set-variable 'org-html-metadata-timestamp-format "%Y")

(defun site-builder-footer-text ()
  (mk-html "p"
           :class "footer"
           :body (concat "Copyright (c) %T " site-builder-author-name)))

(defun site-builder-footer-info-entry (list)
  (let ((name (car list))
        (link (cadr list))
        (icon (caddr list)))
    (mk-html "div"
             :class "w3-bar-item w3-button w3-hover-none"
             :body
             (concat
              (when icon
                (mk-html "a"
                         :href link
                         :body
                         (mk-html "i"
                                  :class icon)))
              (mk-html "a"
                       :href link
                       :body name)))))

(defun site-builder-footer-icons ()
  (mk-html "div"
           :class "w3-bar"
           :body
           (mapconcat #'site-builder-footer-info-entry
                      site-builder-sidepanel-infos
                      "\n")))

(defun site-builder-footer-full ()
  (mk-html "footer"
           :class "w3-footer w3-center w3-white w3-border-top"
           :body
           (concat
            (mk-html "div"
                     :class "w3-container w3-text-gray"
                     :body (concat site-builder-site-name
                                   ",&emsp;"
                                   site-builder-sidepanel-description))
            (site-builder-footer-icons)
            (site-builder-footer-text))))

(defun site-builder-footer-simple ()
  (mk-html "footer"
           :class "w3-footer w3-center w3-white"
           :body
           (site-builder-footer-text)))

(defun site-builder-footer ()
  (if (site-builder-layout-default)
      (site-builder-footer-full)
    (site-builder-footer-simple)))

(provide 'postamble)
;;; postamble.el ends here
