;;;; options.lisp
;;;;
;;;; Command line arguments for quilc
;;;;

(in-package #:quilc)

(defparameter *option-spec*
  '((("prefer-gate-ladders")
     :type boolean
     :optional t
     :documentation "uses gate ladders rather than SWAPs to implement long-ranged gates")

    (("gate-blacklist")
     :type string
     :optional t
     :documentation "when calculating statistics, ignore these (comma-separated) gates")

    (("gate-whitelist")
     :type string
     :optional t
     :documentation "when calculating statistics, consider only these (comma-separated) gates")

    (("without-pretty-printing")
     :type boolean
     :optional t
     :documentation "turns off pretty-printing features")

    (("print-circuit-definitions")
     :type boolean
     :optional t
     :initial-value nil
     :documentation "in batch mode print the circuit definitions from the input program")

    (("verbose")
     :type boolean
     :optional t
     :documentation "verbose compiler trace output")

    (("isa")
     :type string
     :optional t
     :initial-value "8Q"
     :documentation "set ISA to one of \"8Q\", \"20Q\", \"16QMUX\", \"bristlecone\", \"ibmqx5\", or path to QPU description file")

    (("compile" #\c)
     :type boolean
     :optional t
     :initial-value nil
     :documentation "compile the program to an executable using the specified backend")

    (("backend" #\b)
     :type string
     :optional t
     :documentation "backend used to compile the program. Only has effect when --compile or -c are provided")

    (("backend-option" #\B)
     :type string
     :list t
     :optional t
     :documentation "backend option passed to initialization of backend. Only has effect when --backend or -b are provided. Backend options are supplied in the following format: 'option-name=val1;val2;val3...'")

    (("list-backends")
     :type boolean
     :optional t
     :initial-value nil
     :documentation "list available backends for compilation")

    (("output" #\o)
     :type string
     :optional t
     :documentation "file to write output of compilation to. Only has effect when --compile or -c are provided")

    (("enable-state-prep-reductions")
     :type boolean
     :optional t
     :documentation "assume that the program starts in the ground state")

    (("protoquil" #\P)
     :type boolean
     :optional t
     :documentation "restrict input/output to ProtoQuil")

    (("print-statistics")
     :type boolean
     :optional t
     :documentation "print program statistics.  Requires -P.")

    (("compute-matrix-reps" #\m)
     :type boolean
     :optional t
     :documentation "prints matrix representations for comparison  Requires -P.  This is deprecated and will eventually be removed.")
    
    (("help" #\h)
     :type boolean
     :optional t
     :documentation "print this help information and exit")

    (("server-mode-rpc" #\R #\S)
     :type boolean
     :optional t
     :documentation "run as an RPCQ server")

    (("host")
     :type string
     :initial-value "*"
     :optional t
     :documentation "host on which to run the RPCQ server")

    (("port" #\p)
     :type integer
     :initial-value 5555
     :optional t
     :documentation "port to run the RPCQ server on")

    (("time-limit")
     :type integer
     :initial-value 0
     :documentation "time limit (in seconds) for server requests (0 => unlimited)")

    (("version" #\v)
     :type boolean
     :optional t
     :documentation "print version information")

    (("check-libraries")
     :type boolean
     :optional t
     :documentation "check that foreign libraries are adequate")

    #-forest-sdk
    (("benchmark")
     :type boolean
     :optional t
     :documentation "run benchmarks and print results")

    (("log-level")
     :type string
     :optional t
     :initial-value "info"
     :documentation "maximum logging level (\"debug\", \"info\", \"notice\", \"warning\", \"err\", \"crit\", \"alert\", or \"emerg\") (default \"info\")")

    (("quiet")
     :type boolean
     :optional t
     :initial-value nil
     :documentation "Disable all non-logging output (banner, etc.)")

    (("check-sdk-version")
     :type boolean
     :optional t
     :initial-value t
     :documentation "Check for a new SDK version at launch.")

    (("proxy")
     :type string
     :optional t
     :initial-value nil
     :documentation "Proxy to use when checking for an SDK update.")

    (("safe-include-directory")
     :type string
     :optional t
     :initial-value nil
     :documentation "Prevent programs from including files not within this directory. Any includes are interpreted as relative to this directory.")

    #-forest-sdk
    (("swank-port")
     :type integer
     :optional t
     :documentation "port to start a Swank server on"))
  "Supported and non-deprecated options.")

(defparameter *deprecated-option-spec*
  '((("compute-gate-depth" #\d)
     :type boolean
     :optional t
     :documentation "prints compiled circuit gate depth (longest subsequece of data-sharing compiled instructions).  Requires -P.  This is deprecated and will eventually be removed. See --print-statistics.")

    (("compute-gate-volume")
     :type boolean
     :optional t
     :documentation "prints compiled circuit gate volume (number of gates).  Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")

    (("compute-runtime" #\r)
     :type boolean
     :optional t
     :documentation "prints compiled circuit expected runtime.  Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")

    (("compute-fidelity" #\f)
     :type boolean
     :optional t
     :documentation "prints approximate compiled circuit fidelity.  Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")
    
    (("compute-2Q-gate-depth" #\2)
     :type boolean
     :optional t
     :documentation "prints compiled circuit multiqubit gate depth; ignores white/blacklists. Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")
    
    (("compute-unused-qubits" #\u)
     :type boolean
     :optional t
     :documentation "prints unused qubits.  Requires -P.  This is deprecated and will eventually be removed.  See --print-statistics.")
    
    (("show-topological-overhead" #\t)
     :type boolean
     :optional t
     :documentation "prints the number of SWAPs incurred for topological reasons.  This is deprecated and will eventually be removed.  See --print-statistics."))
  "Supported and deprecated options.")

(defparameter *ignored-option-spec*
  '((("print-logical-schedule" #\s)
     :type boolean
     :optional t
     :documentation "include logically parallelized schedule in output.  Requires -P.  This is inactive and will eventually be removed.")
    
    (("json-serialize" #\j)
     :type boolean
     :optional t
     :documentation "serialize output as a JSON object"))
  "Inactive and deprecated options.")

(defparameter *retired-option-spec*
  '()
  "Invalid options.")
