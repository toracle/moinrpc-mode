(require 'moinrpc-common)
(require 'moinrpc-xmlrpc)


(defvar *moinrpc-buffer-debug-log* nil)


(defun moinrpc-get-overlay-text (overlay)
  (buffer-substring (overlay-start overlay)
                    (overlay-end overlay)))


(defun print-current-buffer-local (location)
  "Print current buffer local var values of LOCATION for debug."
  (when *moinrpc-buffer-debug-log*
    (message (format "%s: current-wiki=%s"
                     location
                     moinrpc-buffer-local-current-wiki))))


(defun moinrpc-create-page-buffer (wiki pagename)
  "Create a buffer for a WIKI page which has a PAGENAME."
  (with-current-buffer
      (get-buffer-create (moinrpc-buffer-name pagename))
    (moinrpc-page-mode)
    (erase-buffer)
    (setq moinrpc-buffer-local-current-wiki wiki)
    (setq moinrpc-buffer-local-current-pagename pagename)
    (switch-to-buffer (current-buffer))
    (print-current-buffer-local "create-page-buffer")
    (current-buffer)))

(defun moinrpc-fill-page-buffer-content (buffer current-wiki current-pagename)
  "Fill a BUFFER with content from CURRENT-WIKI CURRENT-PAGENAME."
  (with-current-buffer
      buffer
    (let
        ((content (moinrpc-get-page-content current-wiki current-pagename)))
      (insert content)
      (setq moinrpc-buffer-local-current-wiki current-wiki)
      (setq moinrpc-buffer-local-current-pagename current-pagename)
      (print-current-buffer-local "fill-page-buffer-content")
      (set-buffer-modified-p nil))))

(defun moinrpc-get-or-create-page-buffer (pagename)
  "Get a page buffer which have a PAGENAME or create one if there is not exist."
  (let
      ((buffer-name (moinrpc-buffer-name pagename))
       (buffer nil))
    (if
        (not (setq buffer (get-buffer buffer-name)))
	(progn
          (setq buffer (moinrpc-create-page-buffer
                        moinrpc-buffer-local-current-wiki pagename))
          (moinrpc-fill-page-buffer-content
           buffer
           moinrpc-buffer-local-current-wiki pagename))
      (setq buffer (get-buffer (moinrpc-buffer-name pagename))))
      (print-current-buffer-local "get-or-create-page-buffer")
    (switch-to-buffer buffer)))

(defun moinrpc-create-list-buffer (wiki)
  "Create WIKI page list buffer."
  (with-current-buffer
      (get-buffer-create (moinrpc-buffer-name "List Pages"))
    (read-only-mode -1)
    (erase-buffer)
    (setq-local moinrpc-buffer-local-current-wiki wiki)
    (insert (mapconcat
             'concat
             (moinrpc-get-list-content moinrpc-buffer-local-current-wiki)
             "\n"))
    (read-only-mode)
    (current-buffer)))

(defun moinrpc-save-current-buffer ()
  "Save current buffer to remote wiki."
  (interactive)
  (moinrpc-save-page-content moinrpc-buffer-local-current-wiki
                             moinrpc-buffer-local-current-pagename
                             (moinrpc-strip-text-properties (buffer-string)))
  (set-buffer-modified-p nil)
  (print-current-buffer-local "save-current-buffer")
  (current-buffer))


(defun moinrpc-buffer-add-recent-changes-entry (name author version last-modified)
  (insert " * ")
  (insert-button name
                 'action '(lambda (overlay)
                            (moinrpc-get-or-create-page-buffer
                             (buffer-substring (overlay-start overlay)
                                               (overlay-end overlay)))))
  (insert (format " by %s" author))
  (insert (format " [v%s] " version))
  (insert (format-time-string "%F %T" (cadr last-modified)))
  (newline))


(defun moinrpc-create-main-buffer ()
  "Create main page buffer.  List up wiki list."
  (with-current-buffer
      (get-buffer-create "*moinrpc*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (insert (format "%S" *moinrpc-wiki-settings*))
    (read-only-mode)
    (print-current-buffer-local "create-main-buffer")))


(defun moinrpc-buffer-list-attachment (buffer pagename content wiki)
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


(provide 'moinrpc-buffer-helper)
