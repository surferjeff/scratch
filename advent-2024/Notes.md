

See 072c9f9f797e9608488afba3f2fbae4e19e4d301

First I implemented a loop with a tail recursion and got a stack overflow.
So I learn Seq.unfold.  It worked, but was slow and the code was hard
to understand.  So, I switched to a while loop and mutable code and
everything improved.  Add 'continue' and 'return' statements to F#, and the
code would just about be perfect.
