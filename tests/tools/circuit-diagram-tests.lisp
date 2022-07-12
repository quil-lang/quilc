;;; circuit-diagram-tests.lisp
;;;
;;; Author: Erik Davis

(in-package #:cl-quil/tools-tests)

;;; We don't actually test the pdflatex calls, but we can ensure that
;;; we generate reasonable-looking latex source.

(deftest test-plot-circuit-raw-latex-example ()
  (let ((pp (cl-quil:parse-quil "DECLARE ro BIT[2]; H 0; CNOT 0 1; RX(pi) 1; CNOT 1 2; MEASURE 0 ro[0]; MEASURE 2 ro[1]"))
         ;; the following has been validated by hand....
         ;; we will need to update this after any changes to latex codegen.
        (expected "\\documentclass[convert={density=300,outext=.png}]{standalone}
\\usepackage[margin=1in]{geometry}
\\usepackage{tikz}
\\usepackage{quantikz}

\\begin{document}
\\begin{tikzcd}
 \\lstick{\\ket{q_0}} &  \\gate[wires=1]{H} &  \\ctrl{1} &  \\qw &  \\qw &  \\meter{} &  \\qw \\\\
 \\lstick{\\ket{q_1}} &  \\qw &  \\targ{} &  \\gate[wires=1]{R_x(\\pi)} &  \\ctrl{1} &  \\qw &  \\qw \\\\
 \\lstick{\\ket{q_2}} &  \\qw &  \\qw &  \\qw &  \\targ{} &  \\meter{} &  \\qw \\\\
\\end{tikzcd}
\\end{document}
"))
    (is (string= expected
                 (cl-quil/tools:plot-circuit pp
                                             :backend :latex
                                             :right-align-measurements t)))))

(deftest test-plot-circuit-loose-measure-sanity-check ()
  (let ((pp (cl-quil:parse-quil "H 0; MEASURE 3"))
        (expected
          "\\documentclass[convert={density=300,outext=.png}]{standalone}
\\usepackage[margin=1in]{geometry}
\\usepackage{tikz}
\\usepackage{quantikz}

\\begin{document}
\\begin{tikzcd}
 \\lstick{\\ket{q_0}} &  \\gate[wires=1]{H} &  \\qw &  \\qw \\\\
 \\lstick{\\ket{q_3}} &  \\qw &  \\meter{} &  \\qw \\\\
\\end{tikzcd}
\\end{document}
"))
    (is (string= expected
                 (cl-quil/tools:plot-circuit pp
                                             :backend :latex
                                             :right-align-measurements t)))))

(deftest test-plot-circuit-nq-gate-layout ()
  (let ((pp (cl-quil:parse-quil "
DEFGATE FOO p q r AS PAULI-SUM:
    X(pi) p
    Y(pi) q
    Z(pi) r

FOO 3 2 1"))
        (expected
          "\\documentclass[convert={density=300,outext=.png}]{standalone}
\\usepackage[margin=1in]{geometry}
\\usepackage{tikz}
\\usepackage{quantikz}

\\begin{document}
\\begin{tikzcd}
 \\lstick{\\ket{q_1}} &  \\gate[wires=3]{FOO} &  \\qw \\\\
 \\lstick{\\ket{q_2}} &  \\qw &  \\qw \\\\
 \\lstick{\\ket{q_3}} &  \\qw &  \\qw \\\\
\\end{tikzcd}
\\end{document}
"))
    (is (string= expected
                 (cl-quil/tools:plot-circuit pp
                                             :backend :latex
                                             :layout-strategy ':increasing)))))
