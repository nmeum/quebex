;; SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (quebex-packages)
  #:use-module (guix) ; XXX: this helps 'current-source-directory'
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages c)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages maths)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public ghc-simple-smt
  (package
    (name "ghc-simple-smt")
    (version "0.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "simple-smt" version))
       (sha256
        (base32 "0imimkpzbd013gadkg7sc05jr70lffaij4ijzk368iw8xgvgxyf9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "simple-smt")))
    (home-page "https://github.com/yav/simple-smt")
    (synopsis "Simple way to interact with an SMT solver process.")
    (description
     "This package provides a simple way to interact with an SMT solver process.")
    (license license:bsd-3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %srcdir
  (string-append (current-source-directory) "/../.."))

(define-public quebex-syntax
  (package
    (name "quebex-syntax")
    (version "0.1.0.0")
    (source (local-file (string-append %srcdir "/quebex-syntax")
                        "git-checkout"
                        #:recursive? #t))
    (build-system haskell-build-system)
    (inputs (list ghc-9.2))
    (native-inputs
      (list
        qbe
        ghc-tasty
        ghc-tasty-hunit
        ghc-tasty-golden))
    (synopsis "Parser library for the QBE intermediate language")
    (description "")
    (home-page "https://git.8pit.net/quebex")
    (license (list license:expat license:bsd-2 license:gpl3))))

(define-public quebex
  (package
    (name "quebex")
    (version "0.1.0.0")
    (source (local-file (string-append %srcdir "/quebex")
                        "git-checkout"
                        #:recursive? #t))
    (build-system haskell-build-system)
    (inputs (list ghc-9.2))
    (native-inputs
      (list
        quebex-syntax
        ghc-tasty
        ghc-tasty-hunit))
    (synopsis "Analysis library for the QBE intermediate language")
    (description "")
    (home-page "https://git.8pit.net/quebex")
    (license (list license:expat license:gpl3))))

(define-public quebex-symex
  (package
    (name "quebex-symex")
    (version "0.1.0.0")
    (source (local-file (string-append %srcdir "/quebex-symex")
                        "git-checkout"
                        #:recursive? #t))
    (build-system haskell-build-system)
    (inputs (list ghc-9.2))
    (propagated-inputs (list bitwuzla)) ;; TODO: Wrapper on quebex-cli?
    (native-inputs
      (list
        quebex
        quebex-syntax
        ghc-tasty
        ghc-tasty-hunit
        ghc-tasty-golden
        ghc-tasty-quickcheck
        ghc-simple-smt))
    (synopsis "Symbolic executor for the QBE intermediate language")
    (description "")
    (home-page "https://git.8pit.net/quebex")
    (license (list license:expat license:gpl3))))

(define-public quebex-cli
  (package
    (name "quebex-cli")
    (version "0.1.0.0")
    (source (local-file (string-append %srcdir "/quebex-cli")
                        "git-checkout"
                        #:recursive? #t))
    (build-system haskell-build-system)
    (inputs (list ghc-9.2))
    (native-inputs
      (list
        quebex
        quebex-syntax
        quebex-symex
        ghc-optparse-applicative))
    (synopsis "Command line utilities for Quebex")
    (description "")
    (home-page "https://git.8pit.net/quebex")
    (license (list license:gpl3))))
