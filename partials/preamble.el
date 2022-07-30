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

(setq my/site-title "THIBAUT BENJAMIN")

(setq my/site-menu-content
      (concat
       (mk-html "a"
                :class "w3-bar-item w3-button w3-large w3-hover-none w3-text-indigo"
                :href "/"
                :body (mk-html "b" :body my/site-title))
       (mk-html "a"
                :class "w3-bar-item w3-button w3-large w3-hover-none w3-border-white w3-bottombar w3-hover-border-indigo w3-right"
                :href "/"
                :body "Home")))

(setq my/site-menu
      (mk-html "div"
               :class "w3-top w3-white topbar"
               :body
               (mk-html "div"
                        :class "w3-topbar w3-border-indigo w3-white"
                        :body
                        (mk-html "div"
                                 :class "w3-container w3-bar w3-border-bottom"
                                 :body my/site-menu-content))))

(customize-set-variable 'org-html-preamble t)

(customize-set-variable
 'org-html-preamble-format
 (list
  (list "en" my/site-menu)))

(provide 'preamble)
;;; preamble.el ends here
