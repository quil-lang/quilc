;;;; cfg-tests.lisp
;;;;
;;;; Author: Aaron Vontell

(in-package #:cl-quil-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TESTS FOR RECONSTITUTE PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun verify-h-cnot-code-with-end (code)

  ;; Make sure there is at least an entry block / code block and an exit block
  (is (< 1 (length code)))

  ;; Make sure that code contains the proper components
  (let ((code-string (with-output-to-string (out)
                       (cl-quil::print-code-list code out))))
    (is (search (format nil "H 0~%CNOT 0 1") code-string))
    (is (search "HALT" code-string))))

(deftest test-print-code-list ()
  "Tests the operations of the code-printing helper method"
  (is (string= (format nil "H 0~%CNOT 0 1~%")
               (with-output-to-string (out)
                 (cl-quil::print-code-list (cl-quil:parsed-program-executable-code
                                         (cl-quil::parse-quil "H 0;CNOT 0 1")) out)))))

(defun assert-cfg-edge-invariant (cfg)
  "Asserts that the outgoing and incoming fields of blocks in CFG are a valid match."
  (dolist (blk (cl-quil::cfg-blocks cfg))
    (dolist (successor (cl-quil::children blk))
      (assert (find blk (cl-quil::incoming successor))))))


(deftest test-reconstitute-no-jumps ()
  "Test that the correct program is parsed from a simple CFG"
  (let* ((p (with-output-to-quil
              "H 0"
              "CNOT 0 1"))
         (cfg (cl-quil::program-cfg p))
         (result (cl-quil::reconstitute-program cfg)))
    (verify-h-cnot-code-with-end (cl-quil:parsed-program-executable-code result))))

(deftest test-reconstitute-unconditional-jump ()
  "Test that the correct program is parsed from a CFG with one edge"
  (let* ((p (cl-quil::parse-quil "H 0;JUMP @NEXT;LABEL @NEXT;X 1"))
         (blk-1 (format nil "H 0~%JUMP"))
         (blk-2 "X 1")
         (cfg (cl-quil::program-cfg p))
         (result (cl-quil::reconstitute-program cfg))
         (string-result (with-output-to-string (out)
                          (cl-quil::print-code-list (cl-quil::parsed-program-executable-code result) out))))
    (is (search blk-1 string-result))
    (is (search blk-2 string-result))))

(deftest test-reconstitute-conditional-jump ()
  (let* ((p (cl-quil::parse-quil "DECLARE ro BIT;H 0;JUMP-UNLESS @NEXT ro[0];LABEL @NEXT;Z 0"))
         (blk-0 (format nil "H 0~%JUMP-WHEN"))
         (blk-1 "LABEL @NEXT")
         (blk-2 "Z 0")
         (cfg (cl-quil::program-cfg p))
         (result (cl-quil::reconstitute-program cfg))
         (string-result (with-output-to-string (out)
                          (cl-quil::print-code-list (cl-quil::parsed-program-executable-code result) out))))

    ;; Essentially checking that @NEXT and Z 0 are in the same block in the correct order,
    ;; and that a JUMP-UNLESS exists for the H 0 block. Once we have more gaurantees of the
    ;; blocks kept through our trimming process, we can further expand on these tests to make
    ;; sure that links are correct
    (is (search blk-0 string-result))
    (is (search blk-1 string-result))
    (is (search blk-2 string-result))
    (is (< (search blk-1 string-result) (search blk-2 string-result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TESTS FOR DEAD CODE ELIMINATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-remove-block ()
  "Tests that removing a block from the CFG preserves the validity of the CFG."
  (let* ((p (cl-quil::parse-quil "X 0;JUMP @END;LABEL @END"))
         (cfg (cl-quil::program-cfg p))
         (to-remove (first (cl-quil::cfg-blocks cfg))))

    ;; Arbitrarily remove a block that is not the exit, and assert that the blocks are still valid
    (cl-quil::remove-block to-remove cfg :update-edges t)
    (assert-cfg-edge-invariant cfg)))

(deftest test-dce-none ()
  "Tests the operation of dead code elimination when the cfg is already optimal."
  (let* ((p (cl-quil::parse-quil "X 0;JUMP @END;LABEL @END"))
         (cfg (cl-quil::program-cfg p :dce t))
         (blocks-result (cl-quil::cfg-blocks cfg)))
    (is (= 2 (length blocks-result)))))

(deftest test-dce-simple-extras ()
  "Tests the operation of dead code elimination when presented with a simple graph with extra labels."
  (let* ((p (cl-quil::parse-quil "X 0;JUMP @END;LABEL @END;Y 0;JUMP @ANOTHER;LABEL @ANOTHER;Z 0;JUMP @END;LABEL @END"))
         (cfg (cl-quil::program-cfg p :dce t))
         (blocks-result (cl-quil::cfg-blocks cfg)))

    ;; We want to ensure that this large graph has been cut down to 3 block (ENTRY, END, EXIT)
    (is (= 2 (length blocks-result)))))

(deftest test-dce-dead-loop ()
  "Tests the operation of dead code elimination when the cfg has a dead loop."
  (let* ((p (cl-quil::parse-quil "JUMP @L;LABEL @D;X 0;JUMP @D;LABEL @L;H 0"))
         (cfg (cl-quil::program-cfg p :dce t))
         (blocks-result (cl-quil::cfg-blocks cfg)))

    ;; Want to ensure that the loop from label D is excised
    (is (= 2 (length blocks-result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TESTS FOR BLOCK FUSION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-block-fusion-maximally-simplified ()
  "Tests the operation of block fusion when the CGF is already maximally simplified."
  (let* ((p (cl-quil::parse-quil "X 0"))
         (cfg (cl-quil::program-cfg p :dce t :simplify t))
         (blocks-result (cl-quil::cfg-blocks cfg))
         (entry (cl-quil::entry-point cfg))
         (string-result (with-output-to-string (out)
                          (cl-quil::print-code-list (cl-quil::basic-block-code entry) out))))

    ;; Check that there is one block, and that the entry has a single command
    (is (= 1 (length blocks-result)))
    (is (string= (string-trim '(#\Newline) string-result) "X 0"))))

(deftest test-block-fusion-simple-contraction ()
  "Tests the operation of block fusion when the CFG has a single edge that can be contracted."
  (let* ((p (cl-quil::parse-quil "X 0;JUMP @NEXT;LABEL @NEXT;JUMP @ANOTHER;LABEL @ANOTHER;JUMP @END;LABEL @END"))
         (cfg (cl-quil::program-cfg p :dce t :simplify t))
         (blocks-result (cl-quil::cfg-blocks cfg)))

    ;; Check that there are only two or less blocks (start, end, and conjoined), although the cfg starts with 4 or more.
    (is (>= 2 (length blocks-result)))))

(deftest test-block-fusion-multiple-contraction-unique-code ()
  "Tests the operation of block fusion when the CFG has multiple edges that can be contracted, where each block has disimilar code."
  (let* ((p (cl-quil::parse-quil "LABEL @START;X 0;JUMP @MID;LABEL @MID;Y 0;JUMP @END;LABEL @END;Z 0"))
         (cfg (cl-quil::program-cfg p :dce t :simplify t))
         (blocks-result (cl-quil::cfg-blocks cfg))
         (merged-blk (first (remove-if (lambda (blk)
                                         (when blk
                                           (not (= 3 (length (cl-quil::basic-block-code blk))))))
                                       blocks-result))))

    ;; Check that there are 2 or less blocks (the original CFG has 6)
    (is (>= 2 (length blocks-result)))

    ;; Check that code has been merged properly (i.e. the block after the first entry has X 0 / Y 0)
    (is (string= (with-output-to-string (out)
                   (cl-quil::print-code-list (cl-quil::basic-block-code merged-blk) out))
                 (format nil "X 0~%Y 0~%Z 0~%")))))

(deftest test-block-fusion-empty-single-unconditional-self-loop ()
  "Tests the operation of block fusion when the CFG has an unconditional self-loop with a single empty block."
  (let* ((p (cl-quil::parse-quil "LABEL @START;H 0;JUMP @LOOPER;LABEL @LOOPER;JUMP @START"))
         (cfg (cl-quil::program-cfg p :dce t :simplify t))
         (blocks-result (cl-quil::cfg-blocks cfg)))

    ;; Although an invalid program, the original CFG features a loop, for a total of three blocks
    ;; The new CFG should have a direct loop and at most 2 blocks
    (is (>= 2 (length blocks-result)))))

(deftest test-block-fusion-nonempty-conditional-multiple-self-loop ()
  "Tests the operation of block fusion when the CFG has a long self loop with code inside."
  (let* ((p (cl-quil::parse-quil "DECLARE ro BIT;LABEL @START;H 0;JUMP-WHEN @LOOPER ro[0];JUMP @END;LABEL @LOOPER;JUMP @LOOPER2;LABEL @LOOPER2;X 0;JUMP @LOOPER3;LABEL @LOOPER3;JUMP @START;LABEL @END"))
         (cfg (cl-quil::program-cfg p :dce t :simplify t))
         (blocks-result (cl-quil::cfg-blocks cfg)))

    ;; Originally this program has an unnecessary jump-when loop with 3 blocks when it could be a single one,
    ;; as well as a trail of blocks to the end. This 8 block program should now be a 4 block program (at most).
    (is (>= 4 (length blocks-result)))))

(deftest test-block-fusion-empty-conditional-self-loop ()
  "Tests the operaton of block fusion when the CFG has an empty self loop with a conditional control."
  (let* ((p (cl-quil::parse-quil "DECLARE ro BIT;LABEL @START;H 0;JUMP-WHEN @LOOPER ro[0];JUMP @END;LABEL @LOOPER;JUMP @LOOPER2;LABEL @LOOPER2;JUMP @LOOPER3;LABEL @LOOPER3;JUMP @START;LABEL @END"))
         (cfg (cl-quil::program-cfg p :dce t :simplify t))
         (blocks-result (cl-quil::cfg-blocks cfg)))

    ;; Originally this program has an unnecessary jump-when loop with 3 blocks when it could be completely removed,
    ;; as well as a trail of blocks to the end. This 8 block program should now be a 3 block program (at most).
    (is (>= 3 (length blocks-result)))))

(deftest test-program-terminated-by-conditional-jump ()
  "Verify that quil programs ending with a JUMP-WHEN or JUMP-UNLESS do not signal an UNBOUND-SLOT error.

This is a regression test for https://github.com/rigetti/quilc/issues/244"
  (dolist (jump-instr '("JUMP-WHEN" "JUMP-UNLESS"))
    (let ((p (cl-quil::parse-quil (format nil "DECLARE ro BIT;LABEL @START;~A @START ro[0]" jump-instr))))
      (not-signals error (cl-quil::program-cfg p :dce t :simplify t)))))

(deftest test-consecutive-reset-blocks-collapse ()
  "Verify that consecutive RESET instructions are collapsed into a single block in the graph."
  (let ((program-and-length
          '(("RESET" . 1)
            ("RESET; RESET; X 0" . 1)
            ("RESET; RESET; X 0; RESET" . 2)
            ("RESET; RESET; X 0; RESET; RESET" . 2))))
    (loop :for (program . length) :in program-and-length
          :for cfg := (cl-quil::program-cfg (parse program)) :do
             (is (= (length (cl-quil::cfg-blocks cfg))
                    length)))))

(deftest test-reset-block-survives-preserve-block-pragma ()
  (let* ((program (parse "RESET; PRAGMA PRESERVE_BLOCK; X 0; PRAGMA END_PRESERVE_BLOCK"))
         (cfg (cl-quil::program-cfg program)))
    (is (typep (cl-quil::entry-point cfg) 'cl-quil::reset-block))))
