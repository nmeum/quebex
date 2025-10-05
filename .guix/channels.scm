;; SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
;;
;; SPDX-License-Identifier: Unlicense

(list (channel
        (name 'guix)
        (url "https://git.guix.gnu.org/guix.git")
        (branch "master")
        (commit
          "b25112491eba874a739ce2916d563ddb6d1b200c")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
