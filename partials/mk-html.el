;;; mk-html.el --- macros to write html code         -*- lexical-binding: t; -*-

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

(defun mk-html-arg-string (arg value)
  (if value
      (concat arg "=\"" value "\" ")
    ""))

(cl-defun mk-html (name &key class id style onclick href body)
  (let ((class-string (mk-html-arg-string "class" class))
        (id-string (mk-html-arg-string "id" id))
        (style-string (mk-html-arg-string "style" style))
        (onclick-string (mk-html-arg-string "onclick" onclick))
        (href-string (mk-html-arg-string "href" href)))
  (concat "<"name " "
          class-string
          id-string
          style-string
          onclick-string
          href-string
          ">\n"
          body
          "\n</"name">")))

(cl-defun mk-html-inline (name &key class id style onclick href body)
  (let ((class-string (mk-html-arg-string "class" class))
        (id-string (mk-html-arg-string "id" id))
        (style-string (mk-html-arg-string "style" style))
        (onclick-string (mk-html-arg-string "onclick" onclick))
        (href-string (mk-html-arg-string "href" href)))
  (concat "<"name " "
          class-string
          id-string
          style-string
          onclick-string
          href-string
          ">"
          body
          "</"name">")))

(provide 'mk-html)
;;; mk-html.el ends here
