# ldbounds 2.0.2

* Fixed documentation


---


# ldbounds 2.0.1

* Bug fixes


---


# ldbounds 2.0.0

* Print Methods

	   * Created print.ldBounds: almost identical to summary 
              (just leaves off the exit probabilities)
	      
	   * Created print.ldPower: almost identical to summary
              (just leaves off the exit probs, and puts the
              lower/upper probs up with the boundaries, instead
              of separating)
	      
	   * Edited summary.ldPower: to give power when drift is
              known (put space before probs table)
	      
* Better two-sided bounds and defaults rather than weird
           parameters
	   
	   * Created sides argument for ldBounds
	   
	   * Added ability to specify the number of looks, which
              then automatically creates equally spaced analysis
              times
	      
	   * Sides=2, but with only one alpha, etc, creates
              symmetric bounds, splitting the provided alpha in
              half for each, upper and lower bounds
	      
	   * Print and summary functions updated to account for
              this
	      
* ldPower to use output from ldBounds

	   * ldPower can now accept an ldBounds object as the first
              argument, in which case, it does not require za,
              zb, t, t2
	      
	   * Defaults to drift=0 when nothing is specified
	   
* Nominal alpha level at each look provided (for
           one-sided or symmetric bounds)
	   
	   * ldBounds now has additional value: nominal alpha at
              each look.
	      
	   * Summary.ldBounds displays this
	   
* asf changed from asf/alpha to asf

	   * asf now uses just asf rather than asf alpha (this
              was only a change to alphas function)
	      
* Weird integer errors

	   * ldBounds and alphas now have checks that take into
              account floats
	      
* Ability to specify non-alpha spending boundaries

	   * Created new function commonbounds to calculate
              Pocock or O{'}Brien-Fleming (non-alpha-spending)
              bounds at equally spaced times.
	      
	   * These also become ldBounds objects
	   
	   * Adjusted print and summary functions to accommodate
	   
* Calculate adjusted p-values

	   * Added p-value functionality to ldPower function;
              stepwise and likelihood ratio ordering available.
	      
	   * Uses a new adj.p function which, ironically, calls
              ldPower.
	      
	   * Edited printing and summary functions to use this
              as well.
	      
* Graphics

	   * Now uses base graphics, very simple
	   
	   * Option to show on either z-value or b-value scale
	   
	   * Ability to add to existing plot
	   
* Conditional power

	   * Added condpower, a function to calculate simple
              conditional power
	      
