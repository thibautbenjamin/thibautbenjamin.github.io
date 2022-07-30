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

(customize-set-variable 'org-html-postamble t)
(customize-set-variable 'org-html-metadata-timestamp-format "%Y")


(customize-set-variable
 'org-html-postamble-format
 (list
  (list
   "en"
   (mk-html "footer"
            :class "w3-footer w3-center w3-white"
            :body
            (mk-html "p"
                     :class "footer"
                     :body "Copyright (c) %T %a")))))


(provide 'postamble)
;;; postamble.el ends here
