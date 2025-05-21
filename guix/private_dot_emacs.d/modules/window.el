(defun kill-buffer-and-frame ()
  "Kill the current buffer and delete the selected frame."
  (interactive)
  (let ((buffer-to-kill (current-buffer)))
    (unwind-protect
        (progn
          (if (kill-buffer (current-buffer))
              (delete-frame))))))
