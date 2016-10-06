import numpy
import random
from sklearn import svm 
from sklearn.metrics import mean_squared_error
from sklearn.metrics import accuracy_score
import multiprocessing as mp

# Construct the initial population of individuals
def constructPopulation(dimension,popSize,dimBd,penLow,penHigh):
	indiv = []
	for x in range(popSize):
		# Binarized vector to hold selected features
		crossBits = numpy.zeros(dimension)
		s = []
		for y in range(dimension):
			s.append(y)
		while(dimension-len(s) < dimBd):
			crossBits[s.pop(random.randint(0,len(s)-1))] = 1
		cValue = random.uniform(penLow,penHigh)
		# Add the new individual to the population
		indiv.append((crossBits,cValue))
	return indiv

# Crossover + Mutation
def reproduce(indivX,indivY,mutRate,dimBound,penLow,penHigh):
	# Crossover Stage
	crossBits = numpy.copy(indivX[0])
	lowBound = len(indivY[0])/3
	highBound = 2*len(indivY[0])/3
	for x in range(lowBound,highBound):
		crossBits[x] = indivY[0][x]
	cValue = indivY[1]
	# Mutation Stage
	if (random.uniform(0,1) < mutRate):
		spin = random.randint(1,10)
		if (spin <= 9):
			switch = random.randint(1,len(indivX))
			for s in range(switch):
				if (crossBits[random.randint(0,len(crossBits)-1)] == 1):
					crossBits[random.randint(0,len(crossBits)-1)] = 0
				else:
					crossBits[random.randint(0,len(crossBits)-1)] = 1
		else:
			deltaLow = (penHigh-penLow)/10
			deltaHigh = (penHigh-penLow)/2
			if (random.randint(0,1)==1):
				cValue = max(penLow,cValue-random.uniform(deltaLow,deltaHigh))
			else:
				cValue = min(penHigh,cValue+random.uniform(deltaLow,deltaHigh))
	# Remove features if exceeding dimBound
	coll = []
	for itx in range(len(crossBits)):
		if (crossBits[itx]==1):
			coll.append(itx)
	while (len(coll) > dimBound):
		crossBits[coll.pop(random.randint(0,len(coll)-1))] = 0
	# Check if no features have been selected, and add back features
	# if this is the case
	if (len(coll) == 0):
		switch = random.randint(1,5)
		for s in range(switch):
			crossBits[random.randint(0,len(crossBits)-1)] = 1
	return (crossBits,cValue)

# Computes the fitness of an individual
# arg regression = True for regression, False for classification
# - Returns 2 tuple:
# - 1st value : Error Rate over the test set
# - 2nd value : Cost of features used
def fitness(indiv,trainX,trainY,testX,testY,costs,regression):
	# SVM Construction + Classification
	indices = []
	for i in range(len(indiv[0])):
		if (indiv[0][i]==1):
			indices.append(i)
	trainSelectX = (trainX.transpose()[indices]).transpose()
	testSelectX = (testX.transpose()[indices]).transpose()
	error = 0
	if (regression):
		classifier = svm.SVR(C=indiv[1])
		classifier.fit(trainSelectX,trainY)
		y_pred = classifier.predict(testSelectX)
		error = mean_squared_error(testY,y_pred)
	else:
		classifier = svm.SVC(C=indiv[1])
		classifier.fit(trainSelectX,trainY)
		y_pred = classifier.predict(testSelectX)
		error = 1-accuracy_score(testY,y_pred)
	# Cost of selecting particular features
	cost = 0
	for c in range(len(indiv[0])):
		cost += indiv[0][c]*costs[c]
	return (error,cost)

# Returns whether indivX dominates indivY
def dominates(indivX,indivY):
	return (indivX[0] < indivY[0] and indivX[1] <= indivY[1]
		or indivX[1] < indivY[1] and indivX[0] <= indivY[0])

# Performs the non-domination sort required
# for NSGA II. Require population & fitnesses
# to be in 1-1 correspondence
def nonDominationSort(population,fitnesses):
	# List of lists to hold domination tiers
	tiers = []
	# Array to hold domination tiers - 1-1 correspondence with
	# population list
	ranks = numpy.zeros(len(population))
	# Initialize lists to hold domination counts & sets of
	# dominated individuals, and tiers of individuals
	counters = []
	targets = []
	for x in range(len(population)):
		tiers.append([])
		counters.append(0)
		targets.append(set())
	# For every individual in the population, compute
	# domination count & individuals that are dominated
	for p in range(len(population)):
		for q in range(len(population)):
			if (not q == p):
				if (dominates(fitnesses[p],fitnesses[q])):
					targets[p].add(q)
				elif (dominates(fitnesses[q],fitnesses[p])):
					counters[p] += 1
		# Check if p belongs to top rank
		if (counters[p] == 0):
			tiers[0].append(p)
			ranks[p] = 0
	i = 0
	while (len(tiers[i]) > 0):
		for p in tiers[i]:
			for indiv in targets[p]:
				counters[indiv] -= 1
				if (counters[indiv] == 0):
					tiers[i+1].append(indiv)
					ranks[indiv] = i+1
		i += 1
	# Returns the list of lists that places the individuals in tiers, and
	# the list of ranks in 1-1 correspondence with the population
	return (tiers,ranks)

# Updates crowdDistances array for individual specified by indices
# - indices is list of indices of individuals
# - fitnesses is list of tuples of individual fitnesses
# - crowdDistances is the array of crowdDistances to be updated
def crowdDiversity(indices,fitnesses,crowdDistances):
	subFitnesses = []
	fitToIndex = {}
	for i in indices:
		subFitnesses.append(fitnesses[i])
		fitToIndex[fitnesses[i]] = i
	# Compute distances over every objective
	objCount = len(subFitnesses[0])
	for obj in range(objCount):
		maxVal = float('-inf')
		minVal = float('inf')
		for s in subFitnesses:
			maxVal = max(maxVal,s[obj])
			minVal = min(minVal,s[obj])
		sorted(subFitnesses,key=lambda x : x[obj])
		crowdDistances[fitToIndex[subFitnesses[0]]] = float('inf')
		crowdDistances[fitToIndex[subFitnesses[len(subFitnesses)-1]]] = float('inf')
		for i in range(1,len(subFitnesses)-1):
			quantity = (subFitnesses[i+1][obj]-subFitnesses[i-1][obj])/(maxVal-minVal)
			crowdDistances[fitToIndex[subFitnesses[i]]] += quantity

# Sorts individuals based on crowding distance
def partialSort(indiv,distances):
	l = []
	for i in indiv:
		l.append((distances[i],i))
	sorted(l,key=lambda x : x[0],reverse=True)
	out = []
	for s in range(len(l)):
		out.append(l[s][1])
	return out

# Selects a parent based on tiers and crowding distance
# - population is list of indices of individuals
def selectParent(population,indices,ranks,crowd):
	index1 = random.randint(0,len(indices)-1)
	index2 = random.randint(0,len(indices)-1)
	while (index2 == index1):
		index2 = random.randint(0,len(indices)-1)
	pardexA = indices[index1]
	pardexB = indices[index2]
	if (ranks[pardexA] < ranks[pardexB] or ranks[pardexA] == ranks[pardexB] 
		and crowd[pardexA] > crowd[pardexB]):
		return population[pardexA]
	return population[pardexB]

# Degrade the mutation rate
def degrade(g):
	if (g < 20):
		return 0.55
	elif (g < 50):
		return 0.40
	elif (g < 100):
		return 0.25
	elif (g < 150):
		return 0.15
	elif (g < 200):
		return 0.10
	else:
		return 0.05

# Compute fitness and write result to queue as a 
# tuple of the form (fitness,index), where fitness
# is the 2-tuple fitness of the given individual and
# index is the index of the fitness list where the
# result should be written
def processFitness(index,output,indiv,trainX,trainY,testX,testY,costs,regression):
	f = fitness(indiv,trainX,trainY,testX,testY,costs,regression)
	result = (index,f)
	output.put(result)

# Genetic algorithm simulation
# Parameters:
# - trainX : Feature vectors for training data - Every feature vector is a row
# - trainY : Target values - ith entry is for ith vector in trainX
# - testX : Feature vectors for test data - Every feature vector is a row
# - testY : Target values - ith entry is for ith vector in testX
# - costs : Cost vector - jth entry is cost of using jth feature
# - popSize : Size of population, must be even
# - gens : Number of gens to run simulation
# - dimBound : Maximum number of features that can be selected
# - regression : True for a regression task, False for a classification task
def simulate(trainX,trainY,testX,testY,costs,popSize,gens,dimBound,regression):

	# Upper and Lower bounds for penalty in soft margin SVM
	LOWER_PENALTY_BOUND = 0.5
	UPPER_PENALTY_BOUND = 50
	# Probability of mutating a new individual
	mut_rate = degrade(0)
	# Generate the initial population
	population = constructPopulation(len(trainX[0]),popSize,dimBound,
		LOWER_PENALTY_BOUND,UPPER_PENALTY_BOUND)
	# Construct the replacement population
	replacement = []
	p = 0
	while (p < len(population)-1):
		replacement.append(reproduce(population[p],population[p+1],
			mut_rate, dimBound, LOWER_PENALTY_BOUND, UPPER_PENALTY_BOUND))
		replacement.append(reproduce(population[p+1],population[p],
			mut_rate, dimBound, LOWER_PENALTY_BOUND, UPPER_PENALTY_BOUND))
		p += 2
	# Join main population + replacements
	mergedPopulation = population + replacement

	# The fitness vector
	fitnesses = []
	for itx in range(len(mergedPopulation)):
		fitnesses.append(0)
	# Compute the initial fitnesses
	for x in range(len(mergedPopulation)):
		fitnesses[x] = fitness(mergedPopulation[x],trainX,trainY,
			testX,testY,costs,regression)

	# Perform simulation for given number of gens
	for g in range(gens):

		print "Simulation on Generation " + str(g) + "..."

		# Compute tiers & ranks
		tiers,ranks = nonDominationSort(mergedPopulation,fitnesses)
		i = 0
		# The crowding distance vector - used for diversity. In 1-1
		# correspondence with mergedPopulation
		crowdingDistances = numpy.zeros(len(mergedPopulation))
		# Successor population
		successorPopulation = []
		# Replacement fitness vector
		successorFitnesses = []
		for itx in range(len(mergedPopulation)):
			successorFitnesses.append(0) 

		# Parent pool indices - These are the indices of parents which
		# are available for selection/crossover/mutation - The indices
		# are in 1-1 correspondence with mergedPopulation & fitnesses
		parentPoolIndices = []

		# Construct the successor population
		while(len(successorPopulation)+len(tiers[i])<=popSize):
			# Update the crowding distances
			crowdDiversity(tiers[i],fitnesses,crowdingDistances)
			# Add individuals into new population
			for t in tiers[i]:
				parentPoolIndices.append(t)
				successorPopulation.append(mergedPopulation[t])
				successorFitnesses[len(successorPopulation)-1] = fitnesses[t]
			i += 1
		# If room for more individuals, add based on crowding distance metric
		if (len(successorPopulation) < popSize):
			crowdDiversity(tiers[i],fitnesses,crowdingDistances)
			frontierIndiv = partialSort(tiers[i],crowdingDistances)
			k = 0
			while (len(successorPopulation) < popSize):
				parentPoolIndices.append(frontierIndiv[k])
				successorPopulation.append(mergedPopulation[frontierIndiv[k]])
				successorFitnesses[len(successorPopulation)-1] = fitnesses[frontierIndiv[k]]
				k += 1

		# Perform selection/crossover/mutation to build replacement population.
		# Parent pool indices determine which individuals can be used in this stage.
		while (len(successorPopulation) < len(mergedPopulation)-1):	
			# Select the parents via binary tournament selection, using the
			# tiered ranks, with distances acting as tie breakers
			parentA = selectParent(mergedPopulation,
				parentPoolIndices,ranks,crowdingDistances)
			parentB = selectParent(mergedPopulation,
				parentPoolIndices,ranks,crowdingDistances)
			# Crossover+Mutation
			successorPopulation.append(reproduce(parentA,parentB,mut_rate,
				dimBound,LOWER_PENALTY_BOUND,UPPER_PENALTY_BOUND))
			successorPopulation.append(reproduce(parentB,parentA,mut_rate,
				dimBound,LOWER_PENALTY_BOUND,UPPER_PENALTY_BOUND))
			
		# Compute the fitnesses of the individuals in the replacement
		for x in range(popSize,len(successorPopulation)):
			successorFitnesses[x] = fitness(successorPopulation[x],trainX,
				trainY,testX,testY,costs,regression)

		# Population & fitnesses for next stage - Invariant is
		# that they are in 1-1 correspondence, i.e. the ith
		# individual in mergedPopulation has its fitness at the
		# ith index in fitnesses

		# Replace the old population with the new population
		mergedPopulation = successorPopulation
		# Replace the old fitnesses with the new fitnesses
		fitnesses = successorFitnesses
		# Degrade the mutation rate
		mut_rate = degrade(g)

	# Return the population
	tiers = nonDominationSort(mergedPopulation,fitnesses)[0]
	for idx in tiers[0]:
		print mergedPopulation[x]

	return mergedPopulation
