(require 'moinrpc-common)
(require 'moinrpc-xmlrpc)


(defvar *moinrpc-buffer-debug-log* nil)


(defun print-current-buffer-local (location)
  "Print current buffer local var values of LOCATION for debug."
  (when *moinrpc-buffer-debug-log*
    (message (format "%s: current-wiki=%s"
                     location
                     moinrpc-buffer-local-current-wiki))))


(provide 'moinrpc-buffer-helper)
