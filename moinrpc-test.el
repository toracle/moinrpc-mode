(add-to-list 'load-path ".")
(add-to-list 'load-path "tests")

(require 'moinrpc-test-fixtures)
(require 'moinrpc-conf-test)
(require 'moinrpc-mode-test)
(require 'moinrpc-xmlrpc-test)
(require 'moinrpc-render-test)
(require 'moinrpc-common-test)
(require 'moinrpc-buffer-test)

(ert t)
