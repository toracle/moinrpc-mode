(require 'moinrpc-common)


(defvar *moinrpc-fixture-common-wiki*
  '((wiki-alias . "wiki-1")))


(ert-deftest moinrpc-buffer-name-should-return-name ()
  (let ((wiki *moinrpc-fixture-common-wiki*))
    (should (equal (moinrpc-buffer-name nil) "*moin*"))
    (should (equal (moinrpc-buffer-name "RecentChanges" wiki)
                   "*moin:wiki-1:RecentChanges*"))))


(provide 'moinrpc-common-test)
