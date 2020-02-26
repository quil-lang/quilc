(in-package :cl-quil.quilt-tests)

;;; These tests check template waveform generators against their Pidgin
;;; equivalents. Each test involves checking constructed samples against
;;; a complex vector computed via Pidgin, by something akin to:
;;;
;;; from pidgin.pulseshapes import gaussian
;;; pulse = gaussian(duration=2, sample_rate=8, fwhm=1.0, t0=2.5)
;;; def lisp_real(x):
;;;   out = str(x)
;;;   return out.replace('e', 'd') if 'e' in out else (out + 'd0')
;;; print("#(" + " ".join(f"#C({lisp_real(iq.real)} {lisp_real(iq.imag)})" for iq in pulse) + ")")

(defun vec-double= (x y)
  (and (= (length x) (length y))
       (every (lambda (xc yc)
                (and (quil::double= (realpart xc) (realpart yc))
                     (quil::double= (imagpart xc) (imagpart yc))))
              x y)))

(deftest test-gaussian-iqs ()
  (let ((actual (quilt::gaussian-samples 2d0 8 1.0d0))
        (expected #(#C(0.0625d0 0.0d0) #C(0.11970041008732171d0 0.0d0) #C(0.21022410381342865d0 0.0d0) #C(0.33856388673422316d0 0.0d0) #C(0.5d0 0.0d0) #C(0.6771277734684463d0 0.0d0) #C(0.8408964152537145d0 0.0d0) #C(0.9576032806985737d0 0.0d0) #C(1.0d0 0.0d0) #C(0.9576032806985737d0 0.0d0) #C(0.8408964152537145d0 0.0d0) #C(0.6771277734684463d0 0.0d0) #C(0.5d0 0.0d0) #C(0.33856388673422316d0 0.0d0) #C(0.21022410381342865d0 0.0d0) #C(0.11970041008732171d0 0.0d0))))
    (is (vec-double= actual expected))))

(deftest test-gaussian-iqs-shifted ()
  (let ((actual (quilt::gaussian-samples 2d0 8 1d0 2.5d0))
        (expected #(#C(2.98023223876953d-08 0.0d0) #C(1.6143984162055177d-07 0.0d0) #C(8.019413139855534d-07 0.0d0) #C(3.6529666164343784d-06 0.0d0) #C(1.5258789062500007d-05 0.0d0) #C(5.8447465862950075d-05 0.0d0) #C(0.00020529697638030145d0 0.0d0) #C(0.0006612575912777795d0 0.0d0) #C(0.001953125d0 0.0d0) #C(0.005290060730222238d0 0.0d0) #C(0.013139006488339289d0 0.0d0) #C(0.029925102521830435d0 0.0d0) #C(0.0625d0 0.0d0) #C(0.11970041008732171d0 0.0d0) #C(0.21022410381342865d0 0.0d0) #C(0.33856388673422316d0 0.0d0))))
    (is (vec-double= actual expected))))

(deftest test-drag-gaussian ()
  (let ((actual (quilt::drag-gaussian-samples 2d0 4 2d0 0.5d0 0.3d0 1d0))
        (expected #(#C(0.8408964152537145d0 -0.30921947540952777d0) #C(0.9576032806985737d0 -0.17606781212089828d0) #C(1.0d0 0.0d0) #C(0.9576032806985737d0 0.17606781212089828d0) #C(0.8408964152537145d0 0.30921947540952777d0) #C(0.6771277734684463d0 0.3734962316980985d0) #C(0.5d0 0.36772600025441937d0) #C(0.33856388673422316d0 0.31124685974841537d0))))
    (is (vec-double= actual expected))))

(deftest test-drag-gaussian-wide ()
  (let ((actual (quilt::drag-gaussian-samples 2d0 4 100d0 0d0 1d0 1d0))
        (expected #(#C(1.0d0 0.0d0) #C(0.9999826714706267d0 2.2063177686217352d-05) #C(0.9999306876841536d0 4.412406147764937d-05) #C(0.9998440540452087d0 6.618035787699745d-05) #C(0.9997227795604651d0 8.822977418184008d-05) #C(0.99956687683708d0 0.00011027001888183034d0) #C(0.9993763620805105d0 0.00013229880205560424d0) #C(0.9991512550917064d0 0.00015431383576730362d0))))
    (is (vec-double= actual expected))))

(deftest test-hermite-gaussian ()
  (let ((actual (quilt::hermite-gaussian-samples 2d0 4 0.1d0 0.2d0 2d0 0.5d0 0.4d0))
        (expected #(#C(0.7826099173161067d0 -0.2468842828907429d0) #C(0.9410092803457945d0 -0.14606634856817016d0) #C(1.0d0 0.0d0) #C(0.9410092803457945d0 0.14606634856817016d0) #C(0.7826099173161067d0 0.2468842828907429d0) #C(0.5715242018802867d0 0.2787869735329771d0) #C(0.3613705638880109d0 0.2477166625429275d0) #C(0.191892259528446d0 0.18054490595388736d0))))
    (is (vec-double= actual expected))))

(deftest test-hermite-gaussian-all-ones ()
  (let ((actual (quilt::hermite-gaussian-samples 2d0 4 1d0 1d0 1d0 1d0 1d0))
        (expected #(#C(-0.11078679513998632d0 0.04261514410063609d0) #C(-0.11763744708561481d0 -0.06128373771980623d0) #C(0.15342640972002736d0 -0.2883382561283413d0) #C(0.6951801704096952d0 -0.33891317935828236d0) #C(1.0d0 0.0d0) #C(0.6951801704096952d0 0.33891317935828236d0) #C(0.15342640972002736d0 0.2883382561283413d0) #C(-0.11763744708561481d0 0.06128373771980623d0))))
    (is (vec-double= actual expected))))

(deftest test-erf-square ()
  (let ((actual (quilt::erf-square-samples 2d0 4 1d0 0.7d0 0.5d0 nil))
        (expected #(#C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.00043388937934879523d0 0.0d0) #C(0.04794548357123268d0 0.0d0) #C(0.49999999998634925d0 0.0d0) #C(0.9520542227489965d0 0.0d0) #C(0.9991322212413024d0 0.0d0) #C(0.9520542227489965d0 0.0d0) #C(0.49999999998634925d0 0.0d0) #C(0.04794548357123268d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0))))
    (is (vec-double= actual expected))))

(deftest test-erf-square-polarity ()
  (let ((actual (quilt::erf-square-samples 2d0 4 1d0 0.7d0 0.5d0 t))
        (expected #(#C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(-0.00043388937934879523d0 0.0d0) #C(-0.04794548357123268d0 0.0d0) #C(-0.49999999998634925d0 0.0d0) #C(-0.9520542227489965d0 0.0d0) #C(-0.9991322212413024d0 0.0d0) #C(-0.9520542227489965d0 0.0d0) #C(-0.49999999998634925d0 0.0d0) #C(-0.04794548357123268d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0))))
    (is (vec-double= actual expected))))

(deftest test-erf-square-no-padding ()
  (let ((actual (quilt::erf-square-samples 2d0 4 1d0 0d0 0d0 nil))
        (expected #(#C(0.00043388937934879523d0 0.0d0) #C(0.04794548357123268d0 0.0d0) #C(0.49999999998634925d0 0.0d0) #C(0.9520542227489965d0 0.0d0) #C(0.9991322212413024d0 0.0d0) #C(0.9520542227489965d0 0.0d0) #C(0.49999999998634925d0 0.0d0) #C(0.04794548357123268d0 0.0d0))))
    (is (vec-double= actual expected))))
