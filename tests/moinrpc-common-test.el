(require 'moinrpc-common)


(defvar *moinrpc-fixture-common-wiki*
  '((wiki-alias . "wiki-1")))


(ert-deftest moinrpc-buffer-name-should-return-name ()
  (let ((wiki *moinrpc-fixture-common-wiki*))
    (should (equal (moinrpc-buffer-name nil) "*moin*"))
    (should (equal (moinrpc-buffer-name "RecentChanges" wiki)
                   "*moin:wiki-1:RecentChanges*"))))


(ert-deftest moinrpc-regex-bracket-wikilink ()
  (should (moinrpc-bracket-wikilink-p "[[MyWikiLink]]"))
  (should (not (moinrpc-bracket-wikilink-p "[[https://wiki.net|MyWikiLink]]")))
  (should (not (moinrpc-bracket-wikilink-p "MyWikiLink")))
  (should (not (moinrpc-bracket-wikilink-p "Book/MyWikiLink"))))


(ert-deftest moinrpc-regex-wikilink ()
  (should (moinrpc-wikilink-p "MyWikiLink"))
  (should (not (moinrpc-wikilink-p "MyWikiLink2")))
  (should (not (moinrpc-wikilink-p "Book/MyWikiLink"))))


(provide 'moinrpc-common-test)
