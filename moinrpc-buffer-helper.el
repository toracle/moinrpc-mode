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
      (get-buffer-create (moinrpc-buffer-name pagename
                                              wiki))
    (moinrpc-page-mode)
    (erase-buffer)
    (setq-local moinrpc-buffer-local-current-wiki wiki)
    (setq-local moinrpc-buffer-local-current-pagename pagename)
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
      ((buffer-name (moinrpc-buffer-name pagename
                                         moinrpc-buffer-local-current-wiki))
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


(defun moinrpc-save-current-buffer ()
  "Save current buffer to remote wiki."
  (interactive)
  (moinrpc-save-page-content moinrpc-buffer-local-current-wiki
                             moinrpc-buffer-local-current-pagename
                             (moinrpc-strip-text-properties (buffer-string)))
  (set-buffer-modified-p nil)
  (print-current-buffer-local "save-current-buffer")
  (current-buffer))


(provide 'moinrpc-buffer-helper)
