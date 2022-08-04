;;; infobar.el --- Info bar with icons for my website  -*- lexical-binding: t; -*-

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

(defcustom site-builder-infobar-items "" nil)

(defun site-builder-infobar-icon (link icon)
  (mk-html "a"
           :class "w3-bar-item w3-button w3-hover-none w3-large"
           :href link
           :body
           (mk-html "i" :class icon)))

(defun site-builder-infobar-image (link image)
  (mk-html "a"
           :class "w3-bar-item w3-button w3-hover-none w3-large"
           :href link
           :body
           (concat "<image src=\"" image "\">")))

(defun site-builder-infobar-text (link text)
  (mk-html "a"
           :class "w3-bar-item w3-button w3-hover-none w3-large"
           :href link
           :body text))

(defun site-builder-infobar-icon-inline (link icon)
  (mk-html-inline "a"
           :class "w3-bar-item w3-hover-none w3-large inline-info"
           :href link
           :body
           (mk-html-inline "i" :class icon)))

(defun site-builder-infobar-image-inline (link image)
  (mk-html-inline "a"
           :class "w3-bar-item w3-button w3-hover-none w3-large inline-info"
           :href link
           :body
           (concat "<image src=\"" image "\">")))

(defun site-builder-infobar-text-inline (link text)
  (mk-html-inline "a"
           :class "w3-bar-item w3-button w3-hover-none w3-large inline-info"
           :href link
           :body text))

(defun site-builder-infobar-item (item)
  (let ((type (cdr (assoc (car item) site-builder-infobar-items))))
    (cond
     ((equal (car type) 'icon) (site-builder-infobar-icon (cdr item) (cdr type)))
     ((equal (car type) 'image) (site-builder-infobar-image  (cdr item) (cdr type)))
     ((equal (car type) 'text) (site-builder-infobar-text  (cdr item) (cdr type))))))

(defun site-builder-infobar-item-inline (item)
  (let ((type (cdr (assoc (car item) site-builder-infobar-items))))
    (cond
     ((equal (car type) 'icon) (site-builder-infobar-icon-inline (cdr item) (cdr type)))
     ((equal (car type) 'image) (site-builder-infobar-image-inline  (cdr item) (cdr type)))
     ((equal (car type) 'text) (site-builder-infobar-text-inline  (cdr item) (cdr type))))))

(defun site-builder-infobar (info-alist)
  (mk-html "span"
   :class "w3-bar infobar"
   :body
   (mapconcat #'site-builder-infobar-item info-alist "\n")))

(defun site-builder-infobar-inline (info-alist)
   (mapconcat #'site-builder-infobar-item-inline info-alist "&nbsp;"))


(provide 'infobar)
;;; infobar.el ends here
