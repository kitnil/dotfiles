;; Build:
;; $ guix build -f nix-ssh-known-hosts-to-file.scm
;;
;; Run:
;; $ /gnu/store/...-run.scm

(use-modules (guix gexp)
             (ice-9 format))

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

(program-file "run.scm"
              #~(begin
                  (use-modules (ice-9 popen)
                               (ice-9 rdelim))
                  (let* ((port (open-pipe* OPEN_READ
                                           "nix" "eval" "--raw"
                                           "--file" #$nix-expression-file))
                         (output (read-string port)))
                    (close-port port)
                    (display (string-trim-right output #\newline)))))

