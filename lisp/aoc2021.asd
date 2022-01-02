(defsystem :aoc2021
  :author "Nick Maher"
  :description "Advent of Code 2021"
  :depends-on (:alexandria
               :str
               :split-sequence
               :trivia
               :arrow-macros
               :cl-ppcre
               :metabang-bind)
  :components ((:file "day1" :depends-on ("util"))
               (:file "day2" :depends-on ("util"))
               (:file "day3" :depends-on ("util"))
               (:file "day4" :depends-on ("util"))
               (:file "day5" :depends-on ("util"))
               (:file "util")))