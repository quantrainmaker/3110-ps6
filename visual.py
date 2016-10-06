import pylab

# Plots Pareto frontiers
# levels - A list of sets, where each set contains the 2-tuples
# of fitnesses for that particular Pareto tier, with the 
# tier number in 1-1 correspondence with list index
# First value in 2-tuple is error rate (normalized)
# Second value in 2-tuple is features cost (normalized)
def frontierPlots(levels,count):

	count = min(len(levels),count)

	colorSymbols = ["b","g","r","c","m","y","k","w"]
	curr = 0
	colorDex = 0

	while (curr < count):
		xValues = []
		yValues = []
		for x in levels[curr]:
			xValues.append(x[0])
			yValues.append(x[1])
		pylab.plot(xValues,yValues,colorSymbols[colorDex]+"o",
			label="Tier " + str(curr+1))
		curr += 1
		colorDex += 1
		if (colorDex >= len(colorSymbols)):
			colorDex = 0

	pylab.xlabel("Error rate")
	pylab.ylabel("Total Cost of Features")
	pylab.title("Pareto Tiers")

	pylab.legend(loc='upper right')

	pylab.show()
