;;; wi-alerta.el --- alerta.io procedures            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Oleg Pykhalov

;; Author: Oleg Pykhalov <go.wigust@gmail.com>
;; Keywords: local, news

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

;; This package provides procedures for alerta.io

;;; Code:

(require 'request)

(defvar alerta-key "")

(defun alerta-elfeed (url code)
  (let* ((obj (url-generic-parse-url url))
         (host (url-host obj))
         (filename (url-filename obj)))
    (request
     "https://alerta.wugi.info/api/alert"
     :type "POST"
     :headers `(("Content-Type" . "application/json")
                ("Authorization" . ,(format "Key %s" alerta-key)))
     :data (json-encode
            `(("event" . ,(concat "elfeed." (if (string-prefix-p "www." host)
                                                (string-remove-prefix "www." host)
                                              host)
                                  (if (string-prefix-p "/feeds/videos.xml" filename)
                                      (concat "." (car (cdr (split-string filename "="))))
                                    "")))
              ("severity" . "major")
              ("service" "emacs")
              ("environment" . "Production")
              ("text" . ,(format "%s: %s" code url))
              ("value" . ,code)
              ("resource" . "elfeed")))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Sent to Alerta"))))))

(provide 'wi-alerta)
;;; wi-alerta.el ends here
