(require 'moinrpc-common)


(defmacro moinrpc-insert-decorated-button (prefix label postfix &rest params)
  (declare (indent 3) (debug t))
  `(progn
     (unless (null ,prefix)
       (insert ,prefix))

     (insert-button ,label ,@params)

     (unless (null ,postfix)
       (insert ,postfix))))


(defun moinrpc-render-main-page (buffer content)
  (with-current-buffer
      buffer
    (moinrpc-main-mode)

    (read-only-mode -1)
    (erase-buffer)

    (insert "MoinRPC Wiki List")
    (newline)
    (newline)
    (dolist (wiki-alias content)
      (insert " * ")
      (insert-button wiki-alias
                     'follow-link "\C-m"
                     'action 'moinrpc-wiki-front)
      (newline))
    (read-only-mode)))


(defun moinrpc-render-wiki-front (buffer wiki)
  (with-current-buffer
      buffer
    (moinrpc-front-mode)
    (setq-local moinrpc-current-wiki wiki)

    (read-only-mode -1)
    (erase-buffer)

    (insert (format "Wiki: %s (logged as %s)"
                    (cdr (assoc 'wiki-alias wiki))
                    (cdr (assoc 'username wiki))))
    (newline)
    (newline)
    (moinrpc-insert-decorated-button
        " [" "Recent Changes" "]"
      'follow-link "\C-m"
      'action '(lambda (button)
                 (moinrpc-recent-changes)))

    (moinrpc-insert-decorated-button
        " [" "Find Page" "]"
      'follow-link "\C-m"
      'action '(lambda (button)
                 (moinrpc-helm-find-page)))

    (newline)
    (goto-char 1)
    (read-only-mode)))


(defun moinrpc-render-add-recent-changes-entry (name author version last-modified)
  (moinrpc-insert-decorated-button
      " * " name nil
    'follow-link "\C-m"
    'action '(lambda (overlay)
               (moinrpc-open-page
                (moinrpc-get-overlay-text overlay))))

  (insert (format " by %s" author))
  (insert (format " [v%s] " version))
  (insert (format-time-string "%F %T %Z" (cadr last-modified)))
  (newline))


(defun moinrpc-date-since (days)
  (time-subtract (current-time) (* 3600 24 days)))


(defun moinrpc-render-recent-changes (buffer content wiki)
  (with-current-buffer
      buffer
    (let ((prev-name nil))
      (moinrpc-list-mode)

      (setq-local moinrpc-buffer-local-list-type :recent-changes)
      (setq-local moinrpc-current-wiki wiki)

      (read-only-mode -1)
      (erase-buffer)
      (insert "Recent Changes:")
      (newline)
      (newline)

      (moinrpc-insert-decorated-button
          " [" "7 days" "]"
        'follow-link "\C-m"
        'action '(lambda (button)
                   (moinrpc-recent-changes (moinrpc-date-since 7))))

      (moinrpc-insert-decorated-button
          " [" "30 days" "]"
        'follow-link "\C-m"
        'action '(lambda (button)
                   (moinrpc-recent-changes (moinrpc-date-since 30))))

      (moinrpc-insert-decorated-button
          " [" "90 days" "]"
        'follow-link "\C-m"
        'action '(lambda (button)
                   (moinrpc-recent-changes (moinrpc-date-since 90))))

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
      (setq-local moinrpc-current-wiki wiki)
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


(defun moinrpc-render-page (buffer pagename content wiki)
  (with-current-buffer
      buffer
    (moinrpc-page-mode)

    (setq moinrpc-current-wiki wiki)
    (setq moinrpc-buffer-local-current-pagename pagename)

    (erase-buffer)
    (insert content)
    (set-buffer-modified-p nil)
    (goto-char 1)))


(defun moinrpc-wrap-title-level-1 ()
  (interactive)
  (moinrpc-wrap-title-level-n 1))


(defun moinrpc-wrap-title-level-2 ()
  (interactive)
  (moinrpc-wrap-title-level-n 2))


(defun moinrpc-wrap-title-level-3 ()
  (interactive)
  (moinrpc-wrap-title-level-n 3))


(defun moinrpc-wrap-title-level-4 ()
  (interactive)
  (moinrpc-wrap-title-level-n 4))


(defun moinrpc-wrap-title-level-n (level)
  (let ((m (point-marker)))
    (beginning-of-line)
    (dotimes (_ level)
             (insert "="))
    (insert " ")
    (end-of-line)
    (insert " ")
    (dotimes (_ level)
             (insert "="))
    (goto-char m)))


(defun moinrpc-render-insert-link (pagename)
  (if (moinrpc-wikilink-p pagename)
      (insert pagename)
    (progn
      (insert "[[")
      (insert pagename)
      (insert "]]"))))


(provide 'moinrpc-render)
