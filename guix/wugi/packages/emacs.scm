(define-module (wugi packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix packages))

(define-public emacs-aider
  (let ((commit "9fc3b047754a4ef3fe1d5c664f84170a86a09a4b")
        (revision "1"))
    (package
      (name "emacs-aider")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tninja/aider.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1407jx9wab2x4sjcnswfxq00wx3g5ap8ssrn0acvyn818gdnz7d9"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-transient))
      (native-inputs (list emacs-helm))
      (home-page "https://github.com/tninja/aider.el")
      (synopsis "Emacs user interface for Aider")
      (description
       "This package provides an interactive interface to communicate with
Aider, an AI pair programming environment in your terminal.")
      (license license:asl2.0))))

(define-public emacs-aidermacs
  (package
    (name "emacs-aidermacs")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MatthewZMD/aidermacs.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15l28149akn1xxcqbqzymyw2r84sr3dafdxil8x7m7cx3260p7ni"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient emacs-compat))
    (home-page "https://github.com/MatthewZMD/aidermacs")
    (synopsis "AI pair programming with Aider")
    (description
     "Aidermacs integrates with Aider (https://aider.chat/) for AI-assisted code
modification in Emacs.  Aider lets you pair program with LLMs to edit code in
your local git repository.  It works with both new projects and existing code
bases, supporting Claude, @code{DeepSeek}, @code{ChatGPT}, and can connect to
almost any LLM including local models.  Think of it as having a helpful coding
partner that can understand your code, suggest improvements, fix bugs, and even
write new code for you.  Whether you're working on a new feature, debugging, or
just need help understanding some code, Aidermacs provides an intuitive way to
collaborate with AI while staying in your familiar Emacs environment.
Originally forked from Kang Tu <tninja@@gmail.com>'s Aider.el.")
    (license license:asl2.0)))
