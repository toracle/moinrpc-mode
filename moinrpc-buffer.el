;;; package --- moinmoin xml-rpc client
;;; Commentary:

;;; Code:

;; Buffer related

(require 'moinrpc-common)
(require 'moinrpc-xmlrpc)


(defvar *moinrpc-buffer-debug-log* nil)


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
        ((get-page-function (cdr (assoc :get-page
                                        *moinrpc-content-provider*)))
         (content nil))
      (setq content (funcall get-page-function
                             current-wiki current-pagename))
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
    (let
        ((list-function (cdr (assoc :list-page
                                    *moinrpc-content-provider*))))
      (read-only-mode -1)
      (erase-buffer)
      (setq-local moinrpc-buffer-local-current-wiki wiki)
      (insert (mapconcat
               'concat
               (funcall list-function moinrpc-buffer-local-current-wiki)
               "\n"))
      (read-only-mode)
      (current-buffer))))

(defun moinrpc-save-current-buffer ()
  "Save current buffer to remote wiki."
  (interactive)
  (let
      ((save-page-function (cdr (assoc :save-page
                                       *moinrpc-content-provider*))))
    (funcall save-page-function
	     moinrpc-buffer-local-current-wiki
	     moinrpc-buffer-local-current-pagename
	     (moinrpc-strip-text-properties (buffer-string)))
    (set-buffer-modified-p nil)
    (print-current-buffer-local "save-current-buffer")
    (current-buffer)))


(defun moinrpc-add-recent-changes-entry (name author version last-modified)
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


(defun moinrpc-recent-changes ()
  (interactive)
  (let ((wiki moinrpc-buffer-local-current-wiki))
    (with-current-buffer
        (get-buffer-create (moinrpc-buffer-name "RecentChanges"))
      (switch-to-buffer (current-buffer))
      (setq-local moinrpc-buffer-local-list-type :recent-changes)
      (moinrpc-list-mode)
      (erase-buffer)
      (print-current-buffer-local "create-recent-changes-buffer")
      (setq-local moinrpc-buffer-local-current-wiki wiki)
      (let ((entries (moinrpc-get-recent-changes wiki))
            (prev-name nil))
        (insert "Recent Changes:")
        (newline)
        (newline)
        (dolist (entry entries)
          (let ((name (cdr (assoc "name" entry)))
                (author (cdr (assoc "author" entry)))
                (version (cdr (assoc "version" entry)))
                (last-modified (cdr (assoc "lastModified" entry))))
            (unless (equal prev-name name)
              (moinrpc-add-recent-changes-entry name
                                                author
                                                version
                                                last-modified))
            (setf prev-name name)))
        (goto-char 1)
        (read-only-mode))
      (current-buffer))))


(defun moinrpc-fill-list-attachment ()
  (interactive)
  (let ((wiki moinrpc-buffer-local-current-wiki)
        (pagename moinrpc-buffer-local-current-pagename))
    (with-current-buffer
        (get-buffer-create (moinrpc-buffer-name (format "%s:attachments"
                                                        pagename)))
        (read-only-mode -1)
      (erase-buffer)
      (print-current-buffer-local "create-attachment-list-buffer")
      (let ((entries (moinrpc-get-attachment-list moinrpc-buffer-local-current-wiki
                                                  moinrpc-buffer-local-current-pagename)))
        (insert "Attachment List:")
        (newline)
        (newline)
        (dolist (entry entries)
          (insert " * ")
          (insert-button entry)
          (newline)))
      (goto-char 1)
      (read-only-mode))))


(defun moinrpc-list-attachment ()
  (interactive)
  (let ((wiki moinrpc-buffer-local-current-wiki)
        (pagename moinrpc-buffer-local-current-pagename))
    (with-current-buffer
        (get-buffer-create (moinrpc-buffer-name (format "%s:attachments"
                                                        pagename)))
      (switch-to-buffer (current-buffer))
      (moinrpc-attachment-mode)
      (setq-local moinrpc-buffer-local-list-type :attachment-list)
      (setq-local moinrpc-buffer-local-current-wiki wiki)
      (setq-local moinrpc-buffer-local-current-pagename pagename)
      (moinrpc-fill-list-attachment))))


(defun moinrpc-read-file-as-base64 (filename)
  "Read a file from PATH and encode it to base64."
  (with-temp-buffer
    (insert-file-contents filename nil nil nil t)
    (buffer-string)))


(defun moinrpc-upload-attachment ()
  (interactive)
  (let* ((filename (read-file-name "Select a file to upload:"))
         (name (file-name-nondirectory filename))
         (content (moinrpc-read-file-as-base64 filename)))
    (moinrpc-put-attachment moinrpc-buffer-local-current-wiki
                            moinrpc-buffer-local-current-pagename
                            name
                            content)
    (moinrpc-fill-list-attachment)))


(defun moinrpc-delete-attachment ()
  (interactive)
  (let* ((overlay (car (overlays-at (point))))
         (name (buffer-substring (overlay-start overlay)
                                 (overlay-end overlay))))
    (moinrpc-xmlrpc-delete-attachment moinrpc-buffer-local-current-wiki
                                      moinrpc-buffer-local-current-pagename
                                      name)
    (moinrpc-fill-list-attachment)))


(defun moinrpc-create-main-buffer ()
  "Create main page buffer.  List up wiki list."
  (with-current-buffer
      (get-buffer-create "*moinrpc*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (insert (format "%S" *moinrpc-wiki-settings*))
    (read-only-mode)
    (print-current-buffer-local "create-main-buffer")))

(defun moinrpc-find-page ()
  "Find a page with name."
  (interactive)
  (let
      ((pagename (read-string "Find page: ")))
    (moinrpc-get-or-create-page-buffer pagename)))

(defun helm-moinrpc-find-page ()
  "Find page using helm."
  (interactive)
  (let
      ((get-list-function (cdr (assoc :get-list
                                      *moinrpc-content-provider*)))
       (all-pages nil))
    (print-current-buffer-local "helm-moinrpc-find-page")
    (setq all-pages (funcall get-list-function
                             moinrpc-buffer-local-current-wiki))
    (helm :sources
          '(((name . "All wiki pages")
	     (candidates . all-pages)
	     (action . (("Open" . moinrpc-get-or-create-page-buffer))))
	    ((name . "fallback")
	     (dummy)
	     (action . (("Create" . moinrpc-get-or-create-page-buffer)))))
	  :prompt "Find Page: "
	  :buffer "*helm-moinrpc-find-pages*"
	  )))

(defun moinrpc-helm-find-page (button)
  "BUTTON."
  (let
      ((wiki-alias (button-label button)))
    (setq moinrpc-buffer-local-current-wiki
          (cdr (assoc wiki-alias *moinrpc-wiki-settings*)))
    (print-current-buffer-local "helm-find-page")
    (helm-moinrpc-find-page)))

(provide 'moinrpc-buffer)
;;; moinrpc-buffer.el ends here
