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


(defvar *moinrpc-fixture-list-wiki*
  '(("wiki-1"
     (wiki-alias . "wiki-1")
     (xmlrpc-endpoint . "https://wiki-1.net")
     (username . "user-1")
     (xmlrpc-api-token . "token-1"))
    ("wiki-2"
     (wiki-alias . "wiki-2")
     (xmlrpc-endpoint . "https://wiki-2.net")
     (username . "user-2")
     (xmlrpc-api-token . "token-2"))))


(defvar *moinrpc-fixture-single-wiki*
  '((wiki-alias . "wiki-1")
    (xmlrpc-endpoint . "https://wiki-1.net")
    (username . "user-1")
    (xmlrpc-api-token . "token-1")))


(defvar *moinrpc-fixture-list-attachment*
  '("a.jpg" "b.jpg"))


(ert-deftest moinrpc-render-main-page-should-render-buffer ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (content (moinrpc-get-keys *moinrpc-fixture-list-wiki*)))
      (moinrpc-render-main-page buffer content)

      (should (s-contains-p "MoinRPC Wiki List" (buffer-string)))
      (should (s-contains-p " * wiki-1" (buffer-string)))
      (should (s-contains-p " * wiki-2" (buffer-string))))))


(ert-deftest moinrpc-render-wiki-front-should-render-buffer ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (content *moinrpc-fixture-single-wiki*))
      (moinrpc-render-wiki-front buffer content)

      (should (s-contains-p "Wiki: wiki-1" (buffer-string)))
      (should (s-contains-p " [Recent Changes] [Find Page]"
                            (buffer-string))))))


(ert-deftest moinrpc-render-recent-changes-should-render-buffer ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (moinrpc-render-recent-changes buffer
                                     *moinrpc-fixture-recent-changes*
                                     :wiki)

      (should (s-contains-p "Recent Changes:" (buffer-string)))
      (should (or (s-contains-p "* Page1 by user1 [v1] 2021-01-22 01:30:57 KST"
                                (buffer-string))
                  (s-contains-p "* Page1 by user1 [v1] 2021-01-21 16:30:57 UTC"
                                (buffer-string))))
      (should (or (s-contains-p "* Page2 by user2 [v1] 2021-01-22 01:30:57 KST"
                                (buffer-string))
                  (s-contains-p "* Page2 by user2 [v1] 2021-01-21 16:30:57 UTC"
                                (buffer-string))))
      (should (equal moinrpc-current-wiki :wiki))
      (should (equal moinrpc-buffer-local-list-type :recent-changes)))))


(ert-deftest moinrpc-render-list-attachment-should-render-buffer ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (moinrpc-render-list-attachment buffer
                                      "TestPage"
                                      *moinrpc-fixture-list-attachment*
                                      :wiki)

      (should (s-contains-p "Attachment List:"
                            (buffer-string)))
      (should (s-contains-p " * a.jpg"
                            (buffer-string)))
      (should (s-contains-p " * b.jpg"
                            (buffer-string)))
      (should (equal moinrpc-current-wiki :wiki))
      (should (equal moinrpc-buffer-local-current-pagename "TestPage"))
      (should (equal moinrpc-buffer-local-list-type :attachment-list)))))


(ert-deftest moinrpc-render-page-should-render-buffer ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (moinrpc-render-page buffer
                           "TestPage"
                           "Test content"
                           *moinrpc-fixture-single-wiki*)

      (should (s-contains-p "Test content" (buffer-string))))))


(provide 'moinrpc-render-test)
