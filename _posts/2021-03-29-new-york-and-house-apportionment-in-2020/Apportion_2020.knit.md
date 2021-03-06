---
title: "Rounding Errors: New York, Montana, and House Apportionment in 2020"
description: |
  Does apportionment have a small state bias?
author:
  - name: Ian McDonald
    url: http://www.ianrmcdonald.com
date: 04-27-2021
output:
  distill::distill_article:
    self_contained: false
---




**UPDATE Apr 26**

The results are in, and I will post some analysis shortly, but some comments about the discussion below:

- I listened to the Census spokesperson at the news conference who said New York missed keeping ALL of its seats by 89 residents.  I thought she must have meant 89,000.  Later, I learned that in 1970, Oregon came up short by 230 people, and Utah in 2000 was famously was short by 856 (and would have won an extra seat but for the absence of missionaries). But every other year, the miss by the state with the "436th" seat was several thousand. 
I would be surprised if this result is not litigated by New York, but arguably there's no real political payoff challenging it.

- The thesis below (Montana won a seat at New York's expense) was essentially validated, except for the fact that New York missed its 27th seat rather than its 26th.  Montana's quota was 1.42 and they still won the 2nd seat.  New York's quota was 26.56 and only gets 26 seats. New York 2020 is 6th in the all time "screwed by Huntington Hill" list, instead of 2nd as I projected.  

- Ohio is a second populous state that was denied its quota by a significant amount: it gets 15 seats with a quota of 15.51.  No other state was close to New York and Ohio in that regard.

- More widely reported is Rhode Island's surprise success, surprisingly keeping its second seat for another ten years.  Their quota was 1.44, which makes them "Montana-esque".

- New York and New Jersey exceeded the December estimates by more than 4.5 percent.  Rhode Island was higher by 3.78 percent, and Hawaii, Vermont, Alabama, Maryland, and Massachusetts were higher by 2 percent or more. That list, except for Alabama, is sapphire blue. The median was Virginia at +0.7, and Arizona went *down* by 3,54 percent. These estimates came just four months ago.  Very strange and needs to be explained by someone.

Here's the original pre-announcement post:

**Sunday evening, [Dave Wasserman of the Cook Politcal Report](https://twitter.com/Redistrict) tweeted a late estimate of the soon-to-be-revealed reapportionment from the 2020 Census.  Currently, he sees this [breakdown of state gains and losses:](https://twitter.com/Redistrict/status/1386542093703729152)**

**TX +3**
**FL +2**
**AZ, CO, MT, NC, OR +1**

**AL, CA, IL, MI, MN, NY, OH, PA, RI, WV -1**

**His prediction ties with the apportionment we would have seen from the Census's late December estimates, with one exception:  Alabama would remain at 7, while New York would lose two and drop to 25.** 

**I argue that a potential loss of two New York seats, combined with Montana's gain of one seat, demonstrates a persistent and needless small-state bias in the apportionment rounding process that dates back to the 1940 census.**

Last December, [reports appeared](https://www.politico.com/news/2020/12/30/census-bureau-deadline-missed-452584) about delays in final 2020 Census apportionment for the US House, which also changes state distribution of electors for the Electoral College. New population estimates were published on December 22, and they project New York will lose two of its current 27 seats. In 2019, New York was projected to lose one seat, but not two.^[See the December 2020 population estimates at <https://www.census.gov/programs-surveys/popest/data/tables.html>]

Once the Census determines the country's population, apportionment should be simple. Proportionality is the only constitutional requirement, aside from a guarantee of one representative for each state. The Institute for Social Research at the University of Michigan provides an [easy-to-use apportionment calculator]( https://isr.umich.edu/apportionment-calculator-for-us-census/) based on any population input.

Virtually all the controversy about apportionment involves the population count. But rounding also introduces additional problems, and rounding formulas can bias the result. This year, Montana could be the beneficiary of a quirk in the rounding process, while New York could be victimized.  

The currently used formula for the US House, known as the Huntington-Hill Method, was enacted in 1941. This formula creates a small but consistent bias for less populous states.[^1]

[^1]: In a 2001 Brookings Institute report, economist H. Peyton Young writes, "Not only is it unnecessarily complex---involving square root formulas---it demonstrably favors small states at the expense of large states." <https://www.brookings.edu/research/dividing-the-house-why-congress-should-reinstate-an-old-reapportionment-formula/>] See Also <https://www.census.gov/programs-surveys/popest/data/tables.html>]


### The Quota: the Starting Point for Reapportionment

Each state's seat is driven by its *quota*:

$$
Quota_{state} = \frac{\text{State Population}}{\text{US Population}} \times 435
$$

The quota uses a simple and intuitive computation. But it also includes a fractional remainder, and the apportionment scheme must somehow round the result to a whole number.

Using December's population estimate, New York's seat quota would be:

$$
\text{New York Quota} = \frac{19,336,776}{328,771,307} \times 435 \approx 25.59
$$ 

Montana's quota would be:

$$
\text{Montana Quota}  = \frac{1,080,577}{328,771,307} \times 435 \approx 1.43
$$
How does the process round these quotas into whole numbers? We presume that New York will round to either 25 or 26, and Montana rounds to 1 or 2 ^[This expectation has a name:  the Quota Rule, and we can devise a system capable of violating it. See Young, H P , Balinski, M L (2010) *Fair Representation: Meeting the Ideal of One Man, One Vote United States* Brookings Institution Press]

In the ordinary sense of the word "round", we draw a line at .5. Any value with a remainder above the lower integer is rounded up, and a remainder below .5 is rounded down. Using this definition, New York rounds up from 25.59 to 26 and Montana rounds down from 1.43 to 1.

But if we use the current apportionment formula and the December estimate, New York gets only 25 seats, while Montana would get 2. How is that possible?

### Webster, Huntington-Hill, and the Method of Equal Proportions

The apportionment rounding problem is well documented, full of surprising wrinkles, and inherent in any proportional seat allocation method. Solutions for the rounding problem in the U.S. House date back to the 1790's, and were associated with statesmen, including Hamilton, Jefferson, John Quincy Adams, and Daniel Webster.

In the decades before the 1940's, Congress used a formula designed by Webster based on earlier work by Hamilton.^[Among many excellent summaries of the history (Young 2001) a Supreme Court ruling in 1992 lays out the formula variations over time. The Court overturned a lower court ruling that reinstated a second Montana seat from the 1990 census and supported the Huntington Hill formula in use since 1941.]

The Webster formula applies our starting intuition that the allocation should round up when the remainder is greater than .5, and round down if the remainder is less. But the formula must also determine how to guarantee the sum of the rounded allocations equals 435.^[The fractional remainders of quotas should be uniformly distributed between zero and one.  In practice, we can observe evaluate the distribution for uniformity using a chi-squared test.]

The fractional remainders are unpredictable and rounding can throw off the sum. To demonstrate, suppose that the US is comprised of three states instead of 50 (and let's say they are California, Oregon, and Washington). Using the Webster method and the new 2020 estimates, we see this result:

<div class="layout-chunk" data-layout="l-body">

State         Population   Pop Pct   Quota   Seats
-----------  -----------  --------  ------  ------
California    39,368,078      76.7   333.8     334
Oregon         4,241,507       8.3    36.0      36
Washington     7,693,612      15.0    65.2      65
Total         51,303,197     100.0   435.0     435

</div>


In this particular case, the rounded whole numbers add to 435, and we are finished.

But suppose we alter the population figures slightly. If the fractional remainders are distributed differently, the number of seats may add to a number greater or less than 435.

If we alter the quota formula for some adjustment, we can drive the total to the desired level. Some states will give up a seat while others are left alone.

$$
\text{Adjusted Quota}_{state} = \frac{\text{State Population}}{\text{US Population + Adjustment}} \times 435
$$ 

In the example below, the initial quotas generate a seat total of 436. By gradually increasing the denominator (adding to the total population), one state's fractional remainder will eventually drop below .5.  
When the denominator in the quota formula is increased by 26,780, California's quota drops a hair below 334.5.  Washington's quota and Oregon's quota drop too, but their fractional remainders don't cross the .5 threshold.

<div class="layout-chunk" data-layout="l-body">

|State      | Population| Pop Pct| Initial Quota| Initial Seats| Adjustment| Adjusted Quota| Adjusted Seats|
|:----------|----------:|-------:|-------------:|-------------:|----------:|--------------:|--------------:|
|California | 40,479,078|    76.9|        334.67|           335|     26,780|         334.50|            334|
|Oregon     |  4,441,507|     8.4|         36.72|            37|     26,780|          36.70|             37|
|Washington |  7,693,612|    14.6|         63.61|            64|     26,780|          63.58|             64|
|Total      | 52,614,197|   100.0|        435.00|           436|           |               |            435|

</div>


Simple enough, but the Webster method, and the Hamilton method that preceded it, produced some well known paradoxes and quirks that were more apparent when the number of seats routinely increased from census to census.

In 1880, we saw that Alabama would lose seats as a direct result of hypothetically increasing the total number of House seats, a result famously known as the Alabama Paradox. A related problem known as the Population Paradox was observed in 1902 when using the Hamilton formula (which is similar to the Webster formula), Virginia would have lost seats to Maine even though Virginia's population increased by a greater percentage. ^[Among the many explainers available, Vicki Powers published useful slide deck at <http://www.mathcs.emory.edu/~vicki/talks/Apportionment_Sept2012.pdf>.  Also see Nurmi H. (1999) *Paradoxes of Representation. In: Voting Paradoxes and How to Deal with Them.* Springer, Berlin, Heidelberg. <https://doi.org/10.1007/978-3-662-03782-9_9>]

In order to prevent these scenarios, Congress replaced the Webster method in 1941 by a design by statistician Joseph Hill and adapted by Harvard mathematician Edward Huntington.

The Huntington-Hill formula ignores the .5 rounding threshold and instead uses the geometric mean of the two whole numbers closest to the state's quota. The geometric mean of two consecutive integers is the square root of their product, and the rounding threshold ranges between .414 and .5.

$$
\text{For two consecutive whole numbers, the geometric mean =} 
\sqrt{n\times(n+1))}
$$

If the quota exceeds the geometric mean, we round up, and if not, we round down.

Small states have much smaller geometric means, compared to large states.

### Example: A state with a quota of 3.48

We might think of the .5 threshold determined by the arithmetic mean between the two whole numbers surrounding a quota; if a state's quota is 3.48, the two surrounding whole numbers are 3 and 4. 

The arithmetic mean of 3 and 4 is 3.5. The geometric mean of 3 and 4 is approximately 3.46. Again, the Webster method uses the arithmetic mean and Huntington-Hill uses the geometric mean.^[  

If $n$ = the lower of the two whole numbers surrounding the quota.

$$
\text{For two consecutive whole numbers, the arithmetic mean of n and n+1 =} 
(2n + 1)/2
$$
The fractional remainder of the arithmetic mean is always .5.

**Since the quota of 3.48 falls below 3.5, the Webster method would round down to 3 seats.**  

The geometric mean of two consecutive integers is the square root of their product, and the remainder ranges between .414 and .5.

$$
\text{For two consecutive whole numbers, the geometric mean =} 
\sqrt{n\times(n+1))}
$$
The geometric mean of 3 and 4 is 

$$
\sqrt{3\times(4))} = \sqrt{12} \approx 3.464
$$ 

**Since the quota of 3.48 in the example is greater than 3.464, the Huntington-Hill method rounds *up* to 4 seats.**

The Huntington-Hill method reduces the likelihood of the Alabama Paradox and the Population Paradox, even though the difference between the arithmetic vs. geometric means goes away as the number of seats increases and the geometric mean approaches $(n + .5)$  But the difference between arithmetic vs. geometric means for states with smaller numbers of seats can be significant.

<div class="layout-chunk" data-layout="l-body">
<div class="figure">
<img src="Apportion_2020_files/figure-html5/unnamed-chunk-3-1.png" alt="Rounding thresholds for Webster and Huntington-Hill methods.  Quotas with fractional remainders in the shaded region are rounded *down* for Webster and *up* for Huntington-Hill, the method currently in use. The red dot places our example of a 3.48 quota.  The extra shaded region on the left shows the small-state bias in  Huntington-Hill" width="624" />
<p class="caption">(\#fig:unnamed-chunk-3)Rounding thresholds for Webster and Huntington-Hill methods.  Quotas with fractional remainders in the shaded region are rounded *down* for Webster and *up* for Huntington-Hill, the method currently in use. The red dot places our example of a 3.48 quota.  The extra shaded region on the left shows the small-state bias in  Huntington-Hill</p>
</div>

</div>


The shaded area shows the region where a quota would produce different rounding results depending on the method chosen.  Under today's Huntington-Hill method, the threshold is the curved line. The likelihood of rounding up increases as the state's relative population shrinks.

Neither formula guarantees an allocation of 435 seats on the first pass. For example, in the case of the December 2020 estimate, the Huntington-Hill formula described below generates a sum of 437 representatives on before any adjustments. 

Huntington-Hill reduces the total seat count in the same way we saw in the three-state example: gradually increasing the denominator used to determine states' quotas. Eventually, some states above the rounding threshold will fall below it. Thus, two states will lose a representative, which gets us to 435. 

For the December 2020 estimate, the two states losing seats after this adjustment are Minnesota and New York.

By contrast, the Webster method gets it right on the first try with 435. That method assigns Montana 1 seat, while New York gets 26.  Sometimes it gets to 435 without any adjustment, and sometimes it doesn't.

The formula documented by the U.S. Census adapts this logic in a clever way: you can generate a sequence that assigns seats by number, going all the way to 435.  The result is identical and explained more completely in an addendum below.^[See the census brief that explains the apportion formula at https://www.census.gov/prod/cen2010/briefs/c2010br-08.pdf)]

### Since Huntington-Hill was adopted, how many states have been penalized as much as New York might be in the 2020 apportionment?

Starting with 1940, 9 censuses up to 2010 have produced 446 individual state apportionments under Huntington-Hill. Only 16 of these 446 allocations rounded down a state's apportionment with an initial quota residual greater of than .5. 

As the table shows, the largest residual (i.e., the state losing a seat with the biggest rounding error) was California in the 1950 census, with an extremely large fractional remainder of .722.  

If the current estimate holds, New York will produce second greatest seat loss, relative to its residual, since 1940.

Notice that 12 of these 16 unlucky seat allocations had came from states with seat counts of 10 or more.

<div class="layout-chunk" data-layout="l-body">

|State         |Year | Seats|  Quota| Residual|
|:-------------|:----|-----:|------:|--------:|
|New York      |2020 |    25| 25.585|    0.585|
|Massachusetts |1990 |    10| 10.532|    0.532|
|New Jersey    |1990 |    13| 13.536|    0.536|
|New York      |1990 |    31| 31.521|    0.521|
|California    |1980 |    45| 45.584|    0.584|
|Georgia       |1980 |    10| 10.524|    0.524|
|Indiana       |1980 |    10| 10.574|    0.574|
|Connecticut   |1970 |     6|  6.505|    0.505|
|Oregon        |1970 |     4|  4.501|    0.501|
|Illinois      |1960 |    24| 24.559|    0.559|
|Massachusetts |1960 |    12| 12.543|    0.543|
|Missouri      |1960 |    10| 10.524|    0.524|
|Pennsylvania  |1960 |    27| 27.576|    0.576|
|California    |1950 |    30| 30.722|    0.722|
|Kentucky      |1950 |     8|  8.546|    0.546|
|Tennessee     |1950 |     9|  9.553|    0.553|

</div>


Compare this list with luckier states receiving an additional seat with a residual less than .5:^[Nevada is the only state in recent history whose entire quota fell below 0.5 (in 1940 and 1950). The Huntington-Hill formula almost mathematically guarantees such states round up to one seat, and of course, the Constitution guarantees a seat.]

<div class="layout-chunk" data-layout="l-body">

|State          |Year | Seats| Quota|
|:--------------|:----|-----:|-----:|
|Montana        |2020 |     2|  1.43|
|Minnesota      |2010 |     8|  7.48|
|Rhode Island   |2010 |     2|  1.48|
|California     |2000 |    53| 52.45|
|North Carolina |2000 |    13| 12.47|
|Montana        |1970 |     2|  1.50|
|South Dakota   |1970 |     2|  1.44|
|New Hampshire  |1960 |     2|  1.48|
|Nevada         |1950 |     1|  0.46|
|Arkansas       |1940 |     7|  6.47|
|Nevada         |1940 |     1|  0.37|

</div>

<div class="layout-chunk" data-layout="l-body">
<div class="figure">
<img src="Apportion_2020_files/figure-html5/unnamed-chunk-6-1.png" alt="State quotas vs. fractional remainder of seat allocations.  Positive seats-minus-quota means the formula rounded up, and negative seats-minus-quota means it rounded down. The beneficiaries of a significantly positive rounding are less populous states." width="624" />
<p class="caption">(\#fig:unnamed-chunk-6)State quotas vs. fractional remainder of seat allocations.  Positive seats-minus-quota means the formula rounded up, and negative seats-minus-quota means it rounded down. The beneficiaries of a significantly positive rounding are less populous states.</p>
</div>

</div>


Notice that the constitutional guarantee of one seat has rarely assigned to seat to a state with a tiny quota.^[The geometric mean of 0 and 1 equals 1, and in any event, and in any event, Nevada in 1940 and 1950 are the only cases since 1940 where the quota fell below 0.5]

The second most generous seat allocation will be 2020 if the population figures hold: Montana, winning a second seat despite a small residual of .43. Both New York's smaller apportionment and Montana's larger one would be historic.

### How many states have been rewarded with a bonus as big as Montana's? 

Montana will make history for the lowest quota that yields an extra seat from Huntington-Hill. Montana is always kind of on the bubble and its Supreme Court challenge after the 1990 apportionment reflects that. But a quota below 1.5, such as Montana in 2020, rarely produces two seats.

How many times has Huntington-Hill formula differred from the result generated by Webster?

Nine times since 1940, one state received an extra seat thanks to Huntington-Hill while another state lost one. Usually, we see one instance per census. Each time, a seat shifts from a bigger state to a much smaller one. 

<div class="layout-chunk" data-layout="l-body">

State            Year    Quota   Actual   Webster
---------------  -----  ------  -------  --------
New York         2020    25.58       25        26
North Carolina   2010    13.46       13        14
Massachusetts    1990    10.53       10        11
Indiana          1980    10.57       10        11
Connecticut      1970     6.51        6         7
Oregon           1970     4.50        4         5
Massachusetts    1960    12.54       12        13
California       1950    30.72       30        31
Michigan         1940    17.45       17        18

</div>


<div class="layout-chunk" data-layout="l-body">

|State         |Year | Quota| Actual| Webster|
|:-------------|:----|-----:|------:|-------:|
|Montana       |2020 |  1.43|      2|       1|
|Rhode Island  |2010 |  1.48|      2|       1|
|Oklahoma      |1990 |  5.52|      6|       5|
|New Mexico    |1980 |  2.50|      3|       2|
|Montana       |1970 |  1.50|      2|       1|
|South Dakota  |1970 |  1.44|      2|       1|
|New Hampshire |1960 |  1.48|      2|       1|
|Kansas        |1950 |  5.53|      6|       5|
|Arkansas      |1940 |  6.47|      7|       6|

</div>


<div class="layout-chunk" data-layout="l-body">
<div class="figure">
<img src="Apportion_2020_files/figure-html5/unnamed-chunk-9-1.png" alt="Instances since 1940 when Huntington-Hill and Webster assign different results by state and year" width="624" />
<p class="caption">(\#fig:unnamed-chunk-9)Instances since 1940 when Huntington-Hill and Webster assign different results by state and year</p>
</div>

</div>


Eighteen of the 446 apportions show a different result between Huntington-Hill vs. Webster from 1940-2020.  The states rewarded by Huntington-Hill are all smaller than the states punished by it.

Ironically, Montana contested the 1990 apportionment in court, arguing that the Huntington-Hill method unfairly denied them a second seat in 1990. Montana claimed the process should be based instead on the never-used "Dean" method.^[https://www.maa.org/press/periodicals/convergence/apportioning-representatives-in-the-united-states-congress-deans-method-of-apportionment]  The case *Department of Commerce v. Montana* went to the Supreme Court, which decided in favor of the Huntington-Hill allocation in the 1990 census.^[https://www.law.cornell.edu/supct/html/91-860.ZS.html]  

Montana may yet become a significant beneficiary of Huntington-Hill in 2020.

There is great uncertainty about the census and apportionment, and the December 2020 estimate might be inconsequential.^[April 26: Some surprises but Montana was the record breaking beneficiary of Huntington Hill:  two seat with a 1.43 quota] Even in this example stakes involve a single seat shifting from New York to Montana. 

Given these preliminary numbers, it's hard to argue that Montana deserves a second House seat, The Constitution created an entire chamber dedicating to protecting an unfair numerical advantage of small states. The Senate bias is bad, and House shouldn't make matters worse.  


### Addendum:  Why does the census website show that we assign individual seats in order from 51 to 435?

A clever way to regenerate the Huntington Hill method is described on the census, described as the method of equal proportions, and detailed here: <https://www.census.gov/prod/cen2010/briefs/c2010br-08.pdf>

The process works like this:

1.  Assign one seat to every one of the states.

2.  Assign the remaining 385 seats (i.e., seats numbered 51 through 435) as follows:
    a.  Compute an initial "priority score" that takes the population divided by the geometric mean of 1 and 2.
    
    b.  Every state's initial priority score is its population / sqrt (1\*2), which is its population / \~ 1.41.
    
    c.  Identify the state with the highest priority score and assign the 51st seat to that state. Because every priority score is the same at this point, the 51st seat goes to the state with the greatest population.
    
    The 51st seats thus goes to California.
    
    d.  Reduce that state's priority score by calculating its population divided by the geometric mean of n and n+1, where n is the new, higher number of the state's seats
    
    e.  For the state with the 51st seat, we now use the geometric mean 2 and 3, which is sqrt(2\*3) or sqrt(6), 2.449. Its priority score goes down accordingly.
    
    f.  Reorder the priority scores. The 52nd seat goes to the state with the newly determined high priority score. In all likelihood, the next seat goes to a different state that now has 2 seats. This year, the 52nd seat goes to Texas. Just like California, its priority score is  recomputed to a lower value.
    
    g.  Repeat steps e and f until all 435 seats have been allocated. In each iteration, the priority score is recalculated and reduced, and the state awarded a given seats (probably) moves somewhere further back in the line.

Look at assignments if the continued past seat 435; we get New York at 436 and Minnesota at 437.  Earlier, we saw those were the two states that lost seats when we simply altered the US population denominator.

When we iterate like this, Alabama wins of the 435th seat, while New York comes in 436th. If Alabama had 6,209 fewer residents it would have traded places with New York and lost its seventh seat. 

So why is Montana (and not Alabama) the relevant comparison in my analysis? *Because Montana gets an enomrous break in the adjustment from the Webster method to Huntington-Hill.*  Alabama, by comparison, winds up with 7 seats using either the Huntington-Hill or Webster method.

Again, this is all based on a recent population estimate generated before the inauguration.  The tables below show the size of population change that would drop a state below seat number 435 or move it up to that position.  If the final apportionment figures show these changes, the results will change.  Montana, Alabama, and New York could all produce drama when the results are announced.  

<div class="layout-chunk" data-layout="l-body">

| Seat Number|State    | Total Seats| Population| Loss Drops a Seat| Pct to Drop Seat|
|-----------:|:--------|-----------:|----------:|-----------------:|----------------:|
|         431|Montana  |           2|  1,080,577|             7,966|            0.737|
|         432|Illinois |          17| 12,587,530|            78,840|            0.626|
|         433|Florida  |          29| 21,733,312|           120,792|            0.556|
|         434|Texas    |          39| 29,360,759|           162,868|            0.555|
|         435|Alabama  |           7|  4,921,532|             6,209|            0.126|



| Seat Number|State      | Total Seats| Population| Gain to Add Seat| Pct to Add Seat|
|-----------:|:----------|-----------:|----------:|----------------:|---------------:|
|         436|New York   |          25| 19,336,776|           24,427|           0.126|
|         437|Minnesota  |           7|  5,657,342|           25,554|           0.452|
|         438|Ohio       |          15| 11,693,217|           71,495|           0.611|
|         439|California |          52| 39,368,078|          499,080|           1.268|
|         440|Virginia   |          11|  8,590,563|          134,381|           1.564|

</div>



```{.r .distill-force-highlighting-css}
```
