(define-module (wugi manifests vm1)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages java)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (guix profiles)
  #:export (%vm1-manifest))

(define (%vm1-manifest)
  (packages->manifest
   (list curl
         ffmpeg
         git
         gnu-make
         openjdk12
         python
         restic
         rsync
         screen
         tmux)))
