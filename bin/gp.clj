(ns gp)

(defrecord individual
  [program standardized-fitness adjusted-fitness normalized-fitness hits])

(def number-of-fitness-cases
  "The number of fitness cases")

(def max-depth-for-new-individual
  "The maximum depth for individuals of the initial
	   random genertation")

(def max-depth-for-individuals-after-crossover
  "The maximum depth of new inividuals created by crossover")

(def fitness-proportionate-reproduction-fraction
  "The fraction of the population that will experience fitness
   proportionate reproduction (with reselection) 
   during each generation")

(def crossover-at-any-point-fraction
  "The fraction of the population that will experience 
   crossover at any point in the tree (including terminals)
   during each generation")

(def crossover-at-function-point-fraction 
  "The fraction of the population that will experience 
   crossover at a function (internal) point in the 
   during each generation")

(def max-depth-for-new-subtree-in-mutants 
  "The maximum depth of new subtrees created by mutation")

(def method-of-selection
  "The method of selecting individuals in the population.
   Either :fitness-proportionate, :tournament or 
   :fitness-proportionante-with-over-selection")

(def method-of-generation 
  "Can be any one of :grow, :full, :ramped-half-and-half")

(def *seed* 
  "The seed for the Park-Miller congruential randomizer")

(def *best-of-the-run-individual* 
  "The best individual found during this run")

(def *generation-of-best-of-run-individual*
  "The generation at which the best-of-run individual was found")

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
  (var-set *seed* seed)
  (var-set *generation-of-best-of-run-individual* 0)
  (var-set *best-of-the-run-individual* nil)
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
          (list population fitness-cases))))))s