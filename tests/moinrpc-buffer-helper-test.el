(require 'moinrpc-test-fixtures)


(defvar *moinrpc-fixture-recent-changes*
  '((("name" . "Page1")
     ("author" . "user1")
     ("version" . 1)
     ("lastModified" . (:datetime 1611246657.9662006)))
    (("name" . "Page2")
     ("author" . "user2")
     ("version" . 1)
     ("lastModified" . (:datetime 1611246657.9662006)))))


(ert-deftest moinrpc-buffer-recent-changes-should ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (moinrpc-buffer-recent-changes buffer
                                     *moinrpc-fixture-recent-changes*
                                     :wiki)
      (should (s-contains-p "Recent Changes:"
                            (buffer-string)))
      (should (s-contains-p "* Page1 by user1 [v1] 2021-01-22 01:30:57"
                            (buffer-string)))
      (should (s-contains-p "* Page2 by user2 [v1] 2021-01-22 01:30:57"
                            (buffer-string)))
      (should (equal moinrpc-buffer-local-current-wiki :wiki))
      (should (equal moinrpc-buffer-local-list-type :recent-changes)))))


(provide 'moinrpc-buffer-helper-test)
