;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (guix packages)
             (guix build-system trivial)
             (guix git-download)
             (guix gexp)
             (ice-9 popen)
             (gnu packages haskell-xyz)
             (ice-9 rdelim)
             (guix build utils)
             ((guix licenses) #:prefix license:))

(define %source-dir (dirname (current-filename)))

(define %org-dir
  (and=> (getenv "HOME")
         (lambda (home)
           (string-append home "/src/gitlab.com/wigust/notes"))))

(define lazyblorg
  (and=> (getenv "HOME")
         (lambda (home)
           (load (string-append home "/src/lazyblorg/guix.scm")))))

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-pipe port)
      (string-trim-right output #\newline))))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))


(define-public lazyblorg-wugi-info
  (let ((commit (current-commit)))
    (package
      (name "lazyblorg-wugi-info")
      (version (git-version "0.0.1" "1" commit))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? (git-predicate %source-dir)))
      (inputs
       `(("lazyblorg" ,lazyblorg)
         ("pandoc" ,pandoc)
         ("org" ,(local-file %org-dir
                             #:recursive? #t
                             #:select? (git-predicate %org-dir)))))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-append
                    (assoc-ref %build-inputs "lazyblorg") "/bin" ":"
                    (assoc-ref %build-inputs "pandoc") "/bin"))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (let ((install-dir (string-append %output "/share/doc/wugi.info")))
             (mkdir-p install-dir)
             (invoke "lazyblorg" "--targetdir" install-dir
                     "--previous-metadata" "./NONEXISTING_-_REPLACE_WITH_YOUR_PREVIOUS_METADATA_FILE.pk"
                     "--new-metadata" "./2del-metadata.pk"
                     "--logfile" "./2del-logfile.org"
                     "--orgfiles"
                     (string-append (assoc-ref %build-inputs "org") "/json.org")
                     (string-append (assoc-ref %build-inputs "org") "/iso.org")
                     "testdata/end_to_end_test/orgfiles/about-placeholder.org"
                     "templates/blog-format.org")
             (install-file "templates/public_voit.css" install-dir))
           #t)))
      (build-system trivial-build-system)
      (home-page "https://blog.wugi.info/")
      (synopsis "Static blog generated by lazyblorg")
      (description "This package builds blog.wugi.info static blog generated
by lazyblorg.")
      (license license:gpl3+))))

lazyblorg-wugi-info
