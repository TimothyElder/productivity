There are a number of technical hurdles to overcome. Recall that we are defining the productivity score ($P$) for every researcher of interest ($r$) as:

$$P_r = \frac{ \sum_{i=1}^{n} (C_i \times I_j) }{n}$$

For every paper the researcher has published ($i$) we sum the products of the citations that paper has received by the impact factor ($I$) of the journal ($j$) that the article was published in and divide the sum of the products over the total number of articles ($n$) published by the researcher. The impact factor is the measurement for the year that the publication appeared in that journal. I am not sure about that last decision, as it is motivated by the belief that placing aritcles in journals with higher impact factor is a mark of higher status. So for journals with an impact factor $> 1$ this weights the citations higher, where if the impact factor is $<1$ it will down weight those impact factors. In essence this penalizes lots of citations in lower impact factor journals.

# Historical Impact Factors

To calculate an individual researchers productivity score, you need to estimate the impact factor for all the journals that they have published in for the years in which they placed an article in that journal.

So we will recall that the formula for a journals impact factor is:

$\text{Impact Factor}_{y} = \frac{C_{y}(y-1) + C_{y}(y-2)}{N_{y-1} + N_{y-2}}$

That is, the impact factor for year $y$ equals the number of citations received in year $y$ to articles published in years $y-1$ and $y-2$, divided by the total number of articles published in those two years.

So that means that for any given year, for a journal you need to return all the articles published in the previous two years, and then sum the citation of the articles in year $y$.