import numpy
import random
import math

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
	while (i < len(tiers) and len(tiers[i]) > 0):
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

# Returns True if and only if the two solutions are identical
def solnEquality(x,y):
	if (not x[1] == y[1]):
		return False
	for z in range(len(x)):
		if (not x[0][z] == y[0][z]):
			return False
	return True

# Scales the fitnesses - Diversity measure
def rescaleFitnesses(population,fitnesses,penFactor):
	# Indices Set
	s = set()
	for x in range(len(population)):
		s.add(x)
	# While there is an indiv. to be processed
	while (len(s) > 0):
		holder = set()
		nxt = s.pop()
		for x in s:
			if (x == nxt):
				continue
			elif (solnEquality(population[x],population[nxt])):
				holder.add(x)
		for x in holder:
			s.remove(x)
		holder.add(nxt)
		power = 0
		for x in holder:
			indiv = fitnesses[x]
			t1 = indiv[0]*math.pow(1.0+penFactor,power)
			t2 = indiv[1]*math.pow(1.0+penFactor,power)
			fitnesses[x] = (t1,t2)
			power += 1

# Normalizes a list of 2-tuples, along each dimension
def normalizeFitnesses(fitnesses):
	xSum = 0.0
	ySum = 0.0
	for f in fitnesses:
		xSum += f[0]
		ySum += f[1]
	norms = []
	for f in fitnesses:
		val1 = f[0]/xSum
		val2 = f[1]/ySum
		norms.append((val1,val2))
	return norms

# Separates individuals into tiers via Non-Domination Sort.
# Input - 2-tuple of lists, first for indiv. & 2nd for fitnesses
#
# Returns 2-Tuple, where 1st item is list of lists of indiv.,
# and 2nd item is list of lists of fitnesses. They are in 1-1
# correspondence, with the outer list being the tier and the 
# inner list being the individuals
def tieredPopulation(pop):

	tieredIndices = nonDominationSort(pop[0],pop[1])[0]

	tieredFitnesses = []
	for t in tieredIndices:
		s = []
		for i in t:
			s.append(pop[1][i])
		tieredFitnesses.append(s)

	tieredIndividuals = []
	for t in tieredIndices:
		s = []
		for i in t:
			s.append(pop[0][i])
		tieredIndividuals.append(s)

	# Return the tiered indiv. & fitnesses in 1-1 correspondence
	return (tieredIndividuals,tieredFitnesses)
