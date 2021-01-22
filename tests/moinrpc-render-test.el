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


(defvar *moinrpc-fixture-list-attachment*
  '("a.jpg" "b.jpg"))


(ert-deftest moinrpc-render-recent-changes-should ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (moinrpc-render-recent-changes buffer
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


(ert-deftest moinrpc-buffer-list-attachment-should-create-buffer ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (moinrpc-buffer-list-attachment buffer
                                      "TestPage"
                                      *moinrpc-fixture-list-attachment*
                                      :wiki)
      (should (s-contains-p "Attachment List:"
                            (buffer-string)))
      (should (s-contains-p " * a.jpg"
                            (buffer-string)))
      (should (s-contains-p " * b.jpg"
                            (buffer-string)))
      (should (equal moinrpc-buffer-local-current-wiki :wiki))
      (should (equal moinrpc-buffer-local-current-pagename "TestPage"))
      (should (equal moinrpc-buffer-local-list-type :attachment-list)))))


(provide 'moinrpc-render-test)
