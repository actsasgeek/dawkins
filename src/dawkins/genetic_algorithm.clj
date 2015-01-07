(ns dawkins.genetic-algorithm)

(defn rand-bit
	"Returns a random bit 0 or 1 with a 50/50 probability."
  []
  (if (< (rand) 0.5) 0 1))

(defn random-genome 
	"Given a genome of length k, returns a vector represenation of random bits"
  [k]
  (into [] (repeatedly k rand-bit)))

;; (random-genome 10)

(defn random-individual 
	"Generate a random individual given a genome of length k and a fitness function f defined over that genome
   as a map with keys :genome and :fitness"
  [k f]
  (let [genome (random-genome k)]
    {:genome genome
     :fitness (f genome)}))

(defn random-population 
	"Generate a random n-sized population of individuals with a genome of length k and fitness f(genome)."
  [n k f]
  (into [] (repeatedly n #(random-individual k f))))

;; (random-population 2 10 (fn [genes] (apply + genes)))

(defn flip-bit 
	"For bit = 0, returns 1; 1, 0."
  [bit]
  (if (= bit 0) 1 0))

(defn mutate 
 	"This is the basic mutation operator for the Genetic Algorithm. It is but one alternative and other implementations are possible. For a 
  specified mutation-rate, pick a locus at random and flip the bit."
  [mutation-rate genome]
  (if (< (rand) mutation-rate)
    (let [mutation-point (rand-int (count genome))]
      (into [] (concat (take mutation-point genome) [(-> genome (nth mutation-point) flip-bit)] (drop (inc mutation-point) genome))))
    genome))

;; (mutate 1.0 [0 0 0 0 0])
;; (mutate 1.0 [0 0 0 0 0])

(defn cross 
	"Given two genomes, the 'front' and the 'back' and a point of crossover, append the point number
  of genes from the front genome to the back genome with the point number of genes removed."
  [front-genome back-genome point]
  (concat (take point front-genome) (drop point back-genome)))

(defn crossover 
	"This is the basic one-point crossover operator for the Genetic Algorithm. It is but one alternative and other implentations are possible. For a 
 specified crossover-rate, either create children as the cross of the two parents or return the two parents unchanged. If actual crossover occurs,
 the 'children' will consist of two new genomes the first being the 'front' part of the dad and 'back' part of the mom and the second being
 the front part of the mom and back part of the dad. See the cross function."
  [dad-genome mom-genome crossover-rate]
  (if (< (rand) crossover-rate)
    (let [point (rand-int (count dad-genome))]
      [(cross dad-genome mom-genome point) (cross mom-genome dad-genome point)])
    [dad-genome mom-genome]))

(defn breed 
	"This function combines the crossover and mutation operators (given two parents) and creates new, fitness-evaluated individuals."
  [dad mom fitness mutation-rate crossover-rate]
  (let [[son dau] (crossover (:genome dad) (:genome mom) crossover-rate)
        son (mutate mutation-rate son)
        dau (mutate mutation-rate dau)]
    [{:genome son :fitness (fitness son)} {:genome dau :fitness (fitness dau)}]))

(defn calculate-sampling-probabilities 
	"Roulette Wheel Selection requires that we calculate the size of the 'slots' in the roulette wheel to be proportionate to the
 relative fitness of each individual. If all individuals had the same fitness, then all individuals would have equal probabilities
 of being selected. With unequal fitness, an individual with a greater than average fitness will have a greater than average probability
 of being selected and an individual with a smaller than average fitness will have a smaller than average probability of being selected."
  [population]
  (let [total-fitness (reduce + (map :fitness population))]
   (mapv #(double (/ (:fitness %) total-fitness)) population)))

(defn roulette-wheel-selection 
	"This function implements roulette wheel selection. It is but one alternative and other implementations are possible. It takes a 
 precalculated probability distribution over the individuals as an argument. See calculate-sampling-probabilities.
 
 The basic idea is that individuals are selected with a probability proportionate to their relative fitness."
  [probabilities]
  (loop [mark (rand) sampled-index 0 probabilities probabilities]
    (if (empty? probabilities)
      sampled-index
      (let [current (first probabilities)]
        (if (< mark current)
         sampled-index
         (recur (- mark current) (inc sampled-index) (rest probabilities)))))))

(defn pair-off-and-breed 
	"This is the main function for creating two (possibly) new individuals from two parents. It selects two parents at random from the population
 using roulette wheel selection and then applies one-point crossover with probability crossover-rate and single locus mutation with probability
 mutation-rate. The individuals are returned in a vector as a tuple with their genomes evaluated for fitness."
  [population probabilities fitness mutation-rate crossover-rate]
  (let [dad (population (roulette-wheel-selection probabilities))
        mom (population (roulette-wheel-selection probabilities))]
    (breed dad mom fitness mutation-rate crossover-rate)))

(defn make-next-generation 
	"Creates the next generation for the genetic algorithm."
  [population n fitness mutation-rate crossover-rate]
  (let [probabilities (calculate-sampling-probabilities population)]
    (into [] (flatten (repeatedly (/ n 2) #(pair-off-and-breed population probabilities fitness mutation-rate crossover-rate))))))

(defn compare-fitness 
	"Compares two individuals a and b using their :fitness values and returns the one with the larger fitness."
  [a b]
  (if (< (:fitness a) (:fitness b))
    b
    a))

(defn statistics 
	"Returns statistics about the population. Currently, it returns only the fittest individual."
  [population n]
  (reduce compare-fitness population))

(defn genetic-algorithm 
	"Applies the genetic algorithm to the problem and returns the best individual generated. The problem is a map that contains the keys:
	 max-generations: the number of generations to run the GA simulation
	 n - the size of the population
	 k - the length of the individual genome
	 fitness - a function that takes a Vector representation of the genome, decodes it appropriately and as needed and returns a scalar fitness
	 	value where larger values are better
	 mutation-rate - the probability of a single locus mutation
	 crossover-rate - the probability that two parents will generate children that are some combination of their genomes."
  [problem]
  (let [{:keys [max-generations n k fitness mutation-rate crossover-rate]} problem
        initial-population (random-population n k fitness)
        solution (statistics initial-population n)
        _ (println "Initial best:" solution)]
   (loop [current-generation initial-population solution solution generation-no 0]
     (if (= generation-no max-generations)
       solution
       (let [next-generation (make-next-generation current-generation n fitness mutation-rate crossover-rate)
             candidate (statistics next-generation n)]
             (recur next-generation (compare-fitness solution candidate) (inc generation-no)))))))

;; (genetic-algorithm {:max-generations 10 :n 5 :k 10 :fitness (fn [genome] (reduce + genome)) :mutation-rate 0.05 :crossover-rate 0.9})


;; for testing later
(defn create-testing-rand-fn [ps]
  (let [source (atom ps)]
    (fn []
      (let [result (first @source)]
        (swap! source rest)
        result))))

(defn create-testing-rand-int-fn [ints]
  (let [source (atom ints)]
    (fn [n]
      (let [result (first @source)]
        (swap! source rest)
        result))))
