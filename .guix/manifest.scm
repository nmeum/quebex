;; SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(use-modules (guix gexp)
             (guix packages)
             (guix profiles)
             (quebex-packages))

;; This setup here is inspired by the guile-git Guix setup.
;; See https://gitlab.com/guile-git/guile-git/-/tree/v0.10.0/.guix

(concatenate-manifests
  (list (package->development-manifest quebex)
        (package->development-manifest quebex-syntax)
        (package->development-manifest quebex-symex)
        (package->development-manifest quebex-cli)

        ;; Extra packages, useful for development purposes.
        (specifications->manifest
          '("cabal-install"
            "coreutils"
            "hlint"
            "apply-refact"
            "reuse"
            ;; TODO: ormolu
            "cproc"
            "git"))))
