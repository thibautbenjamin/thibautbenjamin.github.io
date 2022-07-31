;;; gen-bib.el --- Generate bibliography from bib files for my website  -*- lexical-binding: t; -*-

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
(require 'parsebib)

(defvar gen-bib--bibliography nil)
(defvar gen-bib--entry "")
(defvar gen-bib--tmp-buf nil)
(defvar gen-bib--file nil)
(defvar gen-bib--initial-buffer nil)

(defun gen-bib--get (properties name)
  (cdr (assoc name properties)))

(defun gen-bib--format-element (element prop-name &optional before after format)
  (let ((before (if before (concat before " ") ""))
        (after (if after (concat after " ") " "))
        (format (if format format "span")))
    (concat before
            (mk-html format
                     :class (concat "bib-" prop-name)
                     :body (concat element after)))))

(defun gen-bib--format-mandatory-element (properties prop-name &optional before after format)
  (let ((val (gen-bib--get properties prop-name)))
    (if val
        (gen-bib--format-element val prop-name before after format)
      (error (concat "no " prop-name)))))

(defun gen-bib--format-optional-element (properties prop-name &optional before after format)
  (let ((val (gen-bib--get properties prop-name)))
    (when val
      (gen-bib--format-element val prop-name before after format))))

(defvar gen-bib--server-prefix-alist
  '(("doi" . "https://doi.org/")
    ("arxiv" . "https://arxiv.org/abs/")
    ("hal" . "https://hal.archives-ouvertes.fr/hal-")))

(defvar gen-bib--server-logos-alist
  '(("doi" . "doi.gif")
    ("arxiv" . "arxiv-logo-small.jpg")
    ("hal" . "hal.png")))

(defvar gen-bib--icons-alist
  '(("pdf" . "fa-solid fa-file-pdf")
    ("slides" . "fa-solid fa-window-maximize")))

(defun gen-bib--image-property (properties name image link)
  (let ((val (gen-bib--get properties name)))
    (when val
      (mk-html "a"
               :class "w3-button w3-hover-none"
               :href (concat link val)
               :body
               (concat
                "<img src=\"" image "\" height=30em>")))))

(defun gen-bib--format-server (properties property server)
  (gen-bib--image-property properties
                           property
                           (cdr (assoc server gen-bib--server-logos-alist))
                           (cdr (assoc server gen-bib--server-prefix-alist))))

(defun gen-bib--format-eprint (properties)
  (let ((type (gen-bib--get properties "eprinttype")))
    (if (equal type "arXiv")
        (gen-bib--format-server properties "eprint" "arxiv")
      (when (equal type "hal")
        (gen-bib--format-server properties "eprint" "hal")))))

(defun gen-bib--icon-property (properties name icon)
  (let ((val (gen-bib--get properties name)))
    (when val
      (mk-html "a"
               :class "w3-button w3-hover-none w3-xlarge"
               :href val
               :body
               (mk-html "i" :class icon)))))

(defun gen-bib--format-internal-ressource (properties name)
  (gen-bib--icon-property properties name (cdr (assoc name gen-bib--icons-alist))))

(defun gen-bib--format-abstract (properties)
    (let ((abstract (gen-bib--get properties "abstract")))
    (when abstract
      (let ((id (concat (gen-bib--get properties "=key=") "-abstract" )))
      (concat
       (mk-html "button"
               :onclick (concat "show ('" id "')")
               :class "w3-button w3-text-indigo w3-hover-none"
               :body "Abstract ▾")
       (mk-html "div"
                :id id
                :class "w3-container w3-hide w3-light-gray"
                :body abstract))))))

(defun gen-bib--make-citation()
  (let ((buffer (current-buffer))
        (btype (car (org-property-values "btype")))
        (id (car (org-property-values "custom_id"))))
    (find-file gen-bib--file)
    (goto-line 0)
    (re-search-forward (concat "^@" btype "{" id ","))
    (beginning-of-line)
    (let ((start (point)))
      (forward-char (1+ (length btype)))
      (forward-sexp)
      (let ((bibentry (buffer-substring-no-properties start (point))))
        (switch-to-buffer buffer)
        (concat "@@hugo: {{< bib \"" id "\" >}} {{< highlight bibtex >}}@@"
                bibentry
                "@@hugo: {{< / highlight >}}{{< /bib >}}@@")))))

(defun gen-bib-format-entry (properties)
  (mk-html "li"
           :class "bibentry"
           :body
           (concat
            (mk-html "div"
                     :class "bibinfo"
                     :body
                     (concat
                      (gen-bib--format-mandatory-element properties "title" nil ".")
                      (gen-bib--format-mandatory-element properties "author" nil ",")
                      (gen-bib--format-optional-element properties "journal" "in" "," "i")
                      (gen-bib--format-optional-element properties "publisher" nil "," "i")
                      (gen-bib--format-optional-element properties "pages" "pages:" ",")
                      (gen-bib--format-mandatory-element properties "year" nil ".  \n")))
            (gen-bib--format-optional-element properties "notes" nil "  \n")
            (gen-bib--format-server properties "DOI" "doi")
            (gen-bib--format-eprint properties)
            (gen-bib--format-internal-ressource properties "pdf")
            (gen-bib--format-internal-ressource properties "slides")
            ;; (gen-bib--add-element (gen-bib--make-citation))
            (gen-bib--format-abstract properties))))


(defun gen-bib-import (file)
  (setq gen-bib--file file)
  (let ((entries (parsebib-parse file))
        bib)
    (maphash (lambda (entry properties)
               (setq bib
                     (concat
                      bib
                      (gen-bib-format-entry properties)
                      "\n"))
               bib)
             entries)
    (mk-html "ul"
             :class "biblio"
             :body bib)))



(provide 'gen-bib)
;;; gen-bib.el ends here
