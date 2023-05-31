;; Build:
;; $ guix build -f nix-ssh-known-hosts-to-file.scm
;;
;; Run:
;; $ /gnu/store/...-run.scm

(use-modules (guix gexp)
             (ice-9 format)
             (gnu packages)
             (guix packages)
             (guix build-system trivial))

(define flake
  "git+https://gitlab.corp1.majordomo.ru/nixos/jenkins")

(define nix-expression-file
  (plain-file
   "expression.nix"
   (call-with-output-string
     (lambda (port)
       (display "with builtins;" port)
       (newline port)
       (format port "with getFlake ~s;" flake)
       (newline port)
       (display "nixosConfigurations.jenkins.config.environment.etc.\"ssh/ssh_known_hosts\".text" port)
       (newline port)))))

(define (package-from-program-file foo)
  (package
    (name (program-file-name foo))
    (version "0.0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      #~(begin
          (mkdir %output)
          (mkdir (string-append %output "/bin"))
          (copy-file #$foo
                     (string-append %output "/bin/" #$(program-file-name foo)))
          (chmod (string-append %output "/bin/" #$(program-file-name foo))
                 #o555)
          #t)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define my-program
  (program-file "run.scm"
                #~(begin
                    (use-modules (ice-9 popen)
                                 (ice-9 rdelim))
                    (let* ((port (open-pipe* OPEN_READ
                                             "nix" "eval" "--raw"
                                             "--file" #$nix-expression-file))
                           (output (read-string port)))
                      (close-port port)
                      (display (string-trim-right output #\newline))))))

(package-from-program-file my-program)

