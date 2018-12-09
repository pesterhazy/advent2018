## Day 08

- Failed to realize that array indices are 1-based (did not read spec closely enough)
- When the answer is wrong, it's hard to tell if the problem is in my code or if my understanding of the problem is wrong
- In the process of debugging this, I made a change. What I thought was a simplification changed the behavior. Now I had 2 bugs, not 1
- *Lack of visibility*: Often while debugging, you want to know, What arguments is the function called with? What value does it return?
- Still struggling with CIDER - it's too much work to reconnect to a running `clj` repl

Conclusions

- Use `clojure.tools.trace` more often (especially with recursive functions)
- Keep aliases around in `~/.clojure/deps.edn` to easily add a dependency
- Need to smooth out my REPL editor workflow. What value is CIDER actually giving me? Should I just go back to inf-clojure?

## Day 09

I found this problem very hard to solve efficiently with functional data structures.

- The naive solution based on PersistentVector is way too slow

   ```
   advent.puzzle09=> (time (winner 452 10000))
   "Elapsed time: 6494.007989 msecs"
   14259
   ```

- The solution based on sorted-map worked well but only after I discovered `subseq` and `rsubseq` - really essential for working with sorted-maps.

- Algorithmic complexity makes a huge difference - from 12h down to seconds.

- Whenever you need to scan a data structure to find an element, that's a big red flag.

- Programming to interface is good but it can also hold you back if the interface you choose is not correct. In this case, the protocol I started with had two separate methods for getting the nth-ccw marble and removing the nth-ccw marble - which requires doing the work twice.

- This problem lends itself more to a solution based on mutable data structures. [This Python solution](https://www.reddit.com/r/adventofcode/comments/a4i97s/2018_day_9_solutions/ebepyc7/) is not just easier but also simpler than its functional equivalent.

- When things got slow, I started adding type hints. Not having done this before, this feels like trial and error rather than a systematic. The syntax is not particular clear, or well documented.

- Know your data structures! Browsing other people's solutions, I learned about Deques and the ability to rotate them by pushing to one side and removing from the other.

Conclusions

- I now have serious FOMO w/r/t mutable data structures
- Learn the Java class library
- Engage more with other language communities. Python, Ruby and Java can have elegant solutions. The right data structures are more important than the language.
