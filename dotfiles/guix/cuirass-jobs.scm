;;; cuirass-jobs.scm - Cuirass build jobs for Guix.
;;;
;;; Copyright © 2016 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of Cuirass.
;;;
;;; Cuirass is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Cuirass is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

;; Attempt to use Guix modules from git repository.
(eval-when (compile load eval)
  ;; Ignore any available .go, and force recompilation.  This is because our
  ;; checkout in the store has mtime set to the epoch, and thus .go files look
  ;; newer, even though they may not correspond.
  (set! %fresh-auto-compile #t))

(use-modules (guix config)
             (guix store)
             (guix grafts)
             (guix packages)
             (guix profiles)
             (guix derivations)
             (guix monads)
             (guix ui)
             ((guix licenses)
              #:select (gpl3+ license-name license-uri license-comment))
             ((guix utils) #:select (%current-system))
             ((guix scripts system) #:select (read-operating-system))
             (gnu packages)
             (gnu packages commencement)
             (gnu packages guile)
             (gnu packages make-bootstrap)
             (gnu packages package-management)
             (gnu system)
             (gnu system vm)
             ((guix scripts pack)
              #:select (lookup-compressor self-contained-tarball))
             (srfi srfi-1)
             (ice-9 match))

(define (license->alist lcs)
  "Return LCS <license> object as an alist."
  ;; Sometimes 'license' field is a list of licenses.
  (if (list? lcs)
      (map license->alist lcs)
      `((name . ,(license-name lcs))
        (uri . ,(license-uri lcs))
        (comment . ,(license-comment lcs)))))

(define (package-metadata package)
  "Convert PACKAGE to an alist suitable for Hydra."
  `((#:description . ,(package-synopsis package))
    (#:long-description . ,(package-description package))
    (#:license . ,(license->alist (package-license package)))
    (#:home-page . ,(package-home-page package))
    (#:maintainers . ("bug-guix@gnu.org"))
    (#:max-silent-time . ,(or (assoc-ref (package-properties package)
                                         'max-silent-time)
                              3600))      ;1 hour by default
    (#:timeout . ,(or (assoc-ref (package-properties package) 'timeout)
                      72000))))           ;20 hours by default

(define (package-job store job-name package system)
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  (λ ()
    `((#:job-name . ,(string-append (symbol->string job-name) "." system))
      (#:derivation . ,(derivation-file-name
                        (parameterize ((%graft? #f))
                          (package-derivation store package system
                                              #:graft? #f))))
      ,@(package-metadata package))))

(define (package-cross-job store job-name package target system)
  "Return a job called TARGET.JOB-NAME that cross-builds PACKAGE
for TARGET on SYSTEM."
  (λ ()
    `((#:job-name . ,(string-join (list target (symbol->string job-name) system)
                                  "."))
      (#:derivation . ,(derivation-file-name
                        (parameterize ((%graft? #f))
                          (package-cross-derivation store package target system
                                                    #:graft? #f))))
      ,@(package-metadata package))))

(define %core-packages
  ;; Note: Don't put the '-final' package variants because (1) that's
  ;; implicit, and (2) they cannot be cross-built (due to the explicit input
  ;; chain.)
  (append (map specification->package
               '("gcc@4.8" "gcc@4.9" "gcc@5" "glibc" "binutils"
                 "gmp" "mpfr" "mpc" "coreutils" "findutils" "diffutils" "patch" "sed" "grep"
                 "gawk" "gettext" "hello" "zlib" "gzip" "xz"))
          (list guile-2.0
                %bootstrap-binaries-tarball
                %binutils-bootstrap-tarball
                %gcc-bootstrap-tarball
                %guile-bootstrap-tarball
                %bootstrap-tarballs)))

(define %packages-to-cross-build
  %core-packages)

(define %cross-targets
  '("mips64el-linux-gnu"
    "mips64el-linux-gnuabi64"))

(define (tarball-job store system)
  "Return Hydra jobs to build the self-contained Guix binary tarball."
  (define drv
    (mbegin %store-monad
      (set-guile-for-build (default-guile))
      (>>= (profile-derivation (packages->manifest (list guix)))
           (lambda (profile)
             (self-contained-tarball "guix-binary" profile
                                     #:localstatedir? #t
                                     #:compressor
                                     (lookup-compressor "lzip"))))))

  (λ ()
    `((#:job-name . (string-append "binary-tarball." system))
      (#:derivation . ,(derivation-file-name
                        (parameterize ((%graft? #f))
                          (run-with-store store drv
                            #:system system))))
      (#:description . "Stand-alone binary Guix tarball")
      (#:long-description . "This is a tarball containing binaries of Guix
and all its dependencies, and ready to be installed on non-GuixSD
distributions.")
      (#:license . ,(license->alist gpl3+))
      (#:home-page . ,%guix-home-page-url)
      (#:maintainers . ("bug-guix@gnu.org")))))

(define %job-name
  ;; Return the name of a package's job.
  (compose string->symbol package-full-name))

(define package->job
  (let ((base-packages
         (delete-duplicates
          (append-map (match-lambda
                       ((_ package _ ...)
                        (match (package-transitive-inputs package)
                          (((_ inputs _ ...) ...)
                           inputs))))
                      %final-inputs))))
    (lambda (store package system)
      "Return a job for PACKAGE on SYSTEM, or #f if this combination is not
valid."
      (cond ((member package base-packages)
             #f)
            ((supported-package? package system)
             (let ((drv (package-derivation store package system
                                            #:graft? #f)))
               (and (substitutable-derivation? drv)
                    (package-job store (%job-name package)
                                 package system))))
            (else
             #f)))))


;;;
;;; Hydra entry point.
;;;

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define subset
    (match (assoc-ref arguments 'subset)
      ("core" 'core)                    ; only build core packages
      (_ 'all)))                        ; build everything

  (define (cross-jobs system)
    (define (from-32-to-64? target)
      ;; Return true if SYSTEM is 32-bit and TARGET is 64-bit.  This hack
      ;; prevents known-to-fail cross-builds from i686-linux or armhf-linux to
      ;; mips64el-linux-gnuabi64.
      (and (or (string-prefix? "i686-" system)
               (string-prefix? "armhf-" system))
           (string-suffix? "64" target)))

    (define (same? target)
      ;; Return true if SYSTEM and TARGET are the same thing.  This is so we
      ;; don't try to cross-compile to 'mips64el-linux-gnu' from
      ;; 'mips64el-linux'.
      (string-contains target system))

    (define (either proc1 proc2)
      (lambda (x)
        (or (proc1 x) (proc2 x))))

    (append-map (lambda (target)
                  (map (lambda (package)
                         (package-cross-job store (%job-name package)
                                            package target system))
                       %packages-to-cross-build))
                (remove (either from-32-to-64? same?) %cross-targets)))

  ;; Turn off grafts.  Grafting is meant to happen on the user's machines.
  (parameterize ((%graft? #f))
    ;; Return one job for each package, except bootstrap packages.
    (append-map (lambda (system)
                  (case subset
                    ((all)
                     ;; Build everything, including replacements.
                     (let ((pkgs (fold-packages
                                  (lambda (package result)
                                    (if (package-replacement package)
                                        (cons* package
                                               (package-replacement package)
                                               result)
                                        (cons package result)))
                                  '())))
                       (append (filter-map (lambda (pkg)
                                             (and (not (string=? "wine" (package-name pkg)))
                                                  (package->job store pkg system)))
                                           pkgs)
                               (list (tarball-job store system))
                               (cross-jobs system))))
                    ((core)
                     ;; Build core packages only.
                     (let* ((manifest
                             (load* "/home/natsu/dotfiles/guix/user.scm"
                                    (make-user-module
                                     '((guix profiles) (gnu)))))
                            (pkgs
                             (map manifest-entry-item
                                  (manifest-entries manifest))))
                       (filter-map (lambda (pkg)
                                     (and (not (string=? "wine" (package-name pkg)))
                                          (package->job store pkg system)))
                                   pkgs)))
                    (else
                     (error "unknown subset" subset))))
                '("x86_64-linux"))))
