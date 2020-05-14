(mapcar (lambda (directory)
          (let* ((parts (split-string directory "/"))
                 (name (first (last parts)))
                 (group (string-remove-prefix "_"
                                              (first (last (delete name
                                                                   parts)))))
                 (name+group (concat "majordomo-" group "-" name)))
            (wi-define-browse-url-git-commit
             name+group directory (lambda (url)
                                    (first (last (split-string url "/")))))))
        (wi-project-candidates-groups-direcotory))
