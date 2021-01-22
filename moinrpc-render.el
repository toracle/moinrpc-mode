(require 'moinrpc-common)


(defun moinrpc-render-recent-changes (buffer content wiki)
  (with-current-buffer
      buffer
    (let ((prev-name nil))
      (moinrpc-list-mode)
      (print-current-buffer-local "create-recent-changes-buffer")

      (setq-local moinrpc-buffer-local-list-type :recent-changes)
      (setq-local moinrpc-buffer-local-current-wiki wiki)

      (erase-buffer)
      (insert "Recent Changes:")
      (newline)
      (newline)
      (dolist (entry content)
        (let ((name (cdr (assoc "name" entry)))
              (author (cdr (assoc "author" entry)))
              (version (cdr (assoc "version" entry)))
              (last-modified (cdr (assoc "lastModified" entry))))
          (unless (equal prev-name name)
            (moinrpc-buffer-add-recent-changes-entry name
                                                     author
                                                     version
                                                     last-modified))
          (setf prev-name name)))
      (goto-char 1)
      (read-only-mode))
    (current-buffer)))
