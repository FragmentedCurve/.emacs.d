(require 'gpkg)

;; Remove everything that isn't an elisp file.
(add-to-list 'gpkg-removal ".+[^(\\.el)]$")

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "2.4.4")
 ("elfeed" "https://github.com/skeeto/elfeed"
  "3.4.1")
 ("fasm-mode" "https://github.com/the-little-language-designer/fasm-mode.git"
  "0.1.11")
 ("simpc" "https://github.com/rexim/simpc-mode.git"
  "master")
 ("org-bullets" "https://github.com/integral-dw/org-bullets.git"
  "master")
 ("elfeed-org" "https://github.com/remyhonig/elfeed-org.git"
  "master")

 ;; multiple-cursors dependency
 ("dash" "https://github.com/magnars/dash.el.git"
  "2.19.1")
 ("s" "https://github.com/magnars/s.el.git"
  "1.13.0")
 ("f" "https://github.com/rejeep/f.el.git"
  "v0.20.0")
 ("espuds" "https://github.com/ecukes/espuds.git"
  "v0.3.3")
 ("multiple-cursors" "https://github.com/magnars/multiple-cursors.el.git"
  "1.4.0"))
