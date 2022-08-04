;;; formatting.el --- Format the pages for my website  -*- lexical-binding: t; -*-

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
(require 'sidepanel)

(defvar site-builder-current-format)

(defun tb/site-format-page-content (contents)
  (mk-html "div"
           :class "w3-row"
           :body
           (concat
            (mk-html "div"
                     :class "w3-col s1 m1 l1"
                     :body "&nbsp;")
            (mk-html "div"
                     :class "w3-col s10 m10 l10"
                     :body contents)
            (mk-html "div"
                     :class "w3-col s1 m1 l1"
                     :body "&nbsp;"))))

(defun tb/site-format-main-page (contents)
  (concat
   (mk-html "div"
            :class "w3-sidebar w3-bar-block w3-hide-small"
            :style "width:25%;"
            :body (mk-sidepanel))
   (mk-html "div"
            :class "w3-container w3-hide-small"
            :style "margin-left:25%;"
            :body (tb/site-format-page-content contents))
   (mk-html "div"
            :class "w3-container w3-hide-medium w3-hide-large"
            :body (tb/site-format-page-content contents))
  ))

(defun site-builder-format-dwim (contents)
  (if (equal site-builder-current-layout "index")
      (tb/site-format-main-page contents)
    (tb/site-format-page-content contents)))

(advice-add 'org-html-inner-template :filter-return #'site-builder-format-dwim)

(provide 'formatting)
;;; formatting.el ends here
