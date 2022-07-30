;;; sidepanel.el --- Build the sidepanel for my website  -*- lexical-binding: t; -*-

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

(defun mk-sidepanel-info-entry (list)
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

(defun mk-sidepanel-infos ()
  (mk-html "div"
           :class "w3-bar-block w3-container"
           :body
           (mapconcat #'mk-sidepanel-info-entry
                      site-builder-sidepanel-infos
                      "\n")))

(defun mk-sidepanel ()
  (concat
   (mk-html "div"
            :class "w3-center infos"
            :body "Personal Information")
   (mk-html "center"
            :body
            (concat
             "<img src=\""
             site-builder-sidepanel-pic
             "\" class=\"w3-center w3-circle\" style=\"max-width:60%\">"))
   (mk-html "div"
            :class "w3-container w3-center w3-text-gray"
            :body site-builder-sidepanel-description)
   (mk-html "div"
            :class "w3-container w3-large"
            :body (mk-sidepanel-infos))))

;; <div class="w3-center infos"> Personal Information </div>
;; <center><img src={{ .Site.Params.pic }} class="w3-center w3-circle" style="max-width:60%"></center>
;; <div class="w3-container w3-center w3-text-gray"> <div class="description"> {{ .Site.Params.description }} </div> </div>
;; <div class="w3-container w3-large">
;;   <div class="w3-bar-block w3-container">
;;     {{- range .Site.Params }}
;;     {{- if reflect.IsMap . }}
;;     {{ $name := index . "name" }}
;;     {{ $icon := index . "icon" }}
;;     {{ $link := index . "link" }}
;;     <div class="w3-bar-item w3-button w3-hover-none">
;;       {{- if $icon }}
;;       <a href={{ $link }}><i class={{ $icon }}></i> </a>
;;       {{ end -}}
;;       <a href = {{ $link }}>
;;         {{ $name }}
;;       </a>
;;     </div>
;;     {{ end -}}
;;     {{ end -}}
;;   </div>
;; </div>

(provide 'sidepanel)
;;; sidepanel.el ends here
