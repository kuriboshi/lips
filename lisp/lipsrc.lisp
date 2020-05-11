#
# lipsrc - global initialization file
#

(de home (path)
    (concat (getenv "HOME") "/" path))

# (load (home ".lips/list"))
(load (home ".lips/let"))
(load (home ".lips/dirstack"))
(load (home ".lips/alias"))

(de autoload (file which)
    (mapc which
          (lambda (x)
            (putprop x 'autoload file))))

(autoload (home ".lips/pretty")
          '(pprint pp))
(autoload (home ".lips/edit") '(edit))
(autoload (home ".lips/misc") '(foreach))
