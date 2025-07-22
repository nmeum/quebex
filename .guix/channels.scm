;; SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
;;
;; SPDX-License-Identifier: Unlicense

(list (channel
        (name 'guix)
        (url "https://git.guix.gnu.org/guix.git")
        (branch "master")
        (commit
          "e507899e66b5b274e583671b1bc00d7e6cf8f8c0")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
