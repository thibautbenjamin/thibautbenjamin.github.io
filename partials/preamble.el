;;; preamble.el --- Preamble config for my website   -*- lexical-binding: t; -*-

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

(defun site-builder-link-to-main-page (name)
  (let* ((name (file-name-sans-extension name))
         (display (if (equal name "index") "Home" (capitalize name))))
    (mk-html "a"
             :class "w3-bar-item w3-button w3-large w3-hover-none w3-border-white w3-bottombar w3-hover-border-indigo w3-right"
             :href (concat name ".html")
             :body display)))

(defun site-builder-main-pages ()
  (let ((directory-content (directory-files site-builder-base-directory nil "\\.org$"))
        (order-list (append site-builder-menu-order '("index.org"))))
    (append
     (seq-filter (lambda (item) (not (member item order-list))) directory-content)
     order-list)))

(defun site-builder-left-menu ()
  (mapconcat 'site-builder-link-to-main-page (site-builder-main-pages) "\n"))

(defun site-builder-menu-content ()
  (concat
   (mk-html "a"
            :class "w3-bar-item w3-button w3-large w3-hover-none w3-text-indigo"
            :href "/"
            :body (mk-html "b" :body site-builder-site-name))
   (site-builder-left-menu)))

(defun site-builder-menu ()
  (mk-html "div"
           :class "w3-top w3-white topbar"
           :body
           (mk-html "div"
                    :class "w3-topbar w3-border-indigo w3-white"
                    :body
                    (mk-html "div"
                             :class "w3-container w3-bar w3-border-bottom"
                             :body (site-builder-menu-content)))))

(provide 'preamble)
;;; preamble.el ends here
