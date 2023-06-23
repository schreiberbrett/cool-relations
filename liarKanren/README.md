# liarKanren

Relations in miniKanren that are lies. Although the relations appear to be pure, and run in all directions with its arguments in any state of freshness or groundness, they are lies. The underlying code here inspects the arguments passed in and does different things depending on their groundness, similar to Prolog's `var/1`.

The relations supported [Mercury language](https://mercurylang.org/) do something similar.

According to Joel Spolsky in his essay [The Law of Leaky Abstractions](https://www.joelonsoftware.com/2002/11/11/the-law-of-leaky-abstractions/), "all non-trivial abstractions, to some degree, are leaky". Continuing the metaphor, I'll try my best to make these liar relations as watertight as possible.

For an approach to writing arithmetic relations truthfully, see:
* <https://okmij.org/ftp/Prolog/index.html#pure-arithm>, in Prolog
* <https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-arith.scm>, in miniKanren