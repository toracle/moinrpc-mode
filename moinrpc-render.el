(require 'moinrpc-common)


(defun moinrpc-render-main-page (buffer content)
  (with-current-buffer
      buffer
    (moinrpc-main-mode)
    (setq-local moinrpc-buffer-local-current-wiki
                (cdr (assoc *moinrpc-current-wiki* *moinrpc-wiki-settings*)))

    (read-only-mode -1)
    (erase-buffer)

    (insert "MoinRPC Wiki List")
    (newline)
    (newline)
    (dolist (wiki-alias content)
      (insert " * ")
      (insert-button wiki-alias
                     'follow-link "\C-m"
                     'action 'moinrpc-buffer-enter-wiki)
      (newline))
    (read-only-mode)))


(defun moinrpc-render-add-recent-changes-entry (name author version last-modified)
  (insert " * ")
  (insert-button name
                 'action '(lambda (overlay)
                            (moinrpc-get-or-create-page-buffer
                             (moinrpc-get-overlay-text overlay))))
  (insert (format " by %s" author))
  (insert (format " [v%s] " version))
  (insert (format-time-string "%F %T" (cadr last-modified)))
  (newline))


(defun moinrpc-render-recent-changes (buffer content wiki)
  (with-current-buffer
      buffer
    (let ((prev-name nil))
      (moinrpc-list-mode)
      (print-current-buffer-local "create-recent-changes-buffer")

      (setq-local moinrpc-buffer-local-list-type :recent-changes)
      (setq-local moinrpc-buffer-local-current-wiki wiki)

      (read-only-mode -1)
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
            (moinrpc-render-add-recent-changes-entry name
                                                     author
                                                     version
                                                     last-modified))
          (setf prev-name name)))
      (goto-char 1)
      (read-only-mode))
    (current-buffer)))


(defun moinrpc-render-list-attachment (buffer pagename content wiki)
  (with-current-buffer
      buffer
    (let ((entries content))
      (moinrpc-attachment-mode)

      (setq-local moinrpc-buffer-local-list-type :attachment-list)
      (setq-local moinrpc-buffer-local-current-wiki wiki)
      (setq-local moinrpc-buffer-local-current-pagename pagename)

      (read-only-mode -1)
      (erase-buffer)

      (insert "Attachment List:")
      (newline)
      (newline)
      (dolist (entry content)
        (insert " * ")
        (insert-button entry)
        (newline)))
    (goto-char 1)
    (read-only-mode)))


(provide 'moinrpc-render)
