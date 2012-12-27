(ns genetic-programming)

(require 'gp)

(defn run-genetic-programming-system
  [problem-function 
   seed
   maximum-generations
   size-of-population
   & seeded-programs]
  (assert (and (integer? maximum-generations)
               (pos? maximum-generations))
          "Maximum-generations must be a positive integer")
  (assert (and (integer? size-of-population)
               (pos? size-of-population))
          "Size-of-populations must be a positive integer")
  (assert (fn? problem-function)
          "Problem funciton must be a function")
  (assert (number? seed)
          "The randomizer seed must be a number")
  (var-set gp.*seed* seed)
  (var-set gp.*generation-of-best-of-run-individual* 0)
  (var-set gp.*best-of-the-run-individual* nil)
  (let [[function-set-creator
        terminal-set-creator
        fitness-cases-creator
        fitness-function
        parameter-definer
        termination-predicate]
        (problem-function)]
    (let [[functrion-set 
          argument-map]
          (function-set-creator)]
      (parameter-definer)
      (describe-parameters-for-run
        maximum-generations size-of-population)
      (let [terminal-set (terminal-set-creator)])
      (let [population 
            (create-population
              size-of-population function-set argument-map
              terminal-set seeded-programs)]
        (let [fitness-cases (fitness-cases-creator)]
          (var new-programs '())
          (execute-generations 
            population new-programs fitness-cases
            maximum-generations fitness-function
            termination-predicate function-set
            argument-map terminal-set)
          (report-on-run)
          (list population fitness-cases))))))