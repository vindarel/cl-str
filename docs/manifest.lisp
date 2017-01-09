;; Common Doc documentation generator.
;; https://commondoc.github.io/codex/docs/tutorial.html#inserting-api-documentation
;; (ql:quickload :codex)

(:docstring-markup-format :scriba
 :systems (:cl-s)
 :documents ((:title "cl-s"
              :authors ("vindarel")
              :output-format (:type :multi-html
                              :template :minima)
              :sources ("manual.scr"))))
