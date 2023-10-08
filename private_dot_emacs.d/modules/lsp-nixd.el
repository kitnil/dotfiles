;;; lsp-nix.el --- lsp-mode nix integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2020 lsp-mode maintainers

;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; Keywords: languages

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

;; Client for the nixd language server.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-nixd nil
  "LSP support for Nix, using nixd-lsp."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nix-community/nixd"))

(defcustom lsp-nixd-server-path "nixd"
  "Executable path for the server."
  :group 'lsp-nixd
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-nixd-server-path))
                  :major-modes '(nix-mode)
                  :server-id 'nixd-lsp))

(lsp-consistency-check lsp-nixd)

(provide 'lsp-nixd)
;;; lsp-nixd.el ends here
