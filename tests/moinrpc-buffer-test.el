(require 'moinrpc-common)


(ert-deftest moinrpc-table-p-with-single-line-should-return-true ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (insert "||Col1||Col2||Col3||")
      (should (moinrpc-table-p)))))


(ert-deftest moinrpc-table-p-with-empty-line-should-return-false ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (newline)
      (insert "||Col1||Col2||Col3||")
      (newline)
      (should (not (moinrpc-table-p))))))


(ert-deftest moinrpc-table-parse-line ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (should (equal (moinrpc-table-parse-line "||Col1||Col2||Col3||")
                     '("Col1" "Col2" "Col3"))))))


(ert-deftest moinrpc-table-range-should-return-range ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (insert "||R1-C1||R1-C2||R1-C3||")
      (newline)
      (insert "||R2-C1||R2-C2||R2-C3||")
      (goto-char 3)
      (let ((range (moinrpc-table-range)))
        (should (equal (marker-position (car range)) 1))
        (should (equal (marker-position (cdr range)) 48))))))


(provide 'moinrpc-buffer-test)
