Day 07

- Failed to realize that array indices are 1-based (did not read spec closely enough)
- When the answer is wrong, it's hard to tell if the problem is in my code or if my understanding of the problem is wrong
- In the process of debugging this, I made a change. What I thought was a simplification changed the behavior. Now I had 2 bugs, not 1
- *Lack of visibility*: Often while debugging, you want to know, What arguments is the function called with? What value does it return?
- Still struggling with CIDER - it's too much work to reconnect to a running `clj` repl

Conclusions

- Use `clojure.tools.trace` more often
- Keep aliases around in `~/.clojure/deps.edn` to easily add a dependency
- Need to smooth out my REPL editor workflow. What value is CIDER actually giving me? Should I just go back to inf-clojure?
