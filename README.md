# Some testing data
```scala
simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE))   // => ALTs(List(ONE, CHAR(a)))
simp(((CHAR('a') | ZERO) ~ ONE) | (((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO)))   // => CHAR(a)

matcher(("a" ~ "b") ~ "c", "ab")   // => false
matcher(("a" ~ "b") ~ "c", "abc")  // => true

// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000)          // => false
matcher(EVIL, "a" * 1000 ++ "b")   // => true

// size without simplifications
size(der('a', der('a', EVIL)))             // => 36
size(der('a', der('a', der('a', EVIL))))   // => 83

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 7
size(simp(der('a', der('a', der('a', EVIL))))) // => 7
```

---
Python needs around 30 seconds for matching 28 a's with EVIL. Java 9 and later increase this to an "astonishing" 40000 a's in 30 seconds. Matching strings with 5 Million a's using this matcher should be in the range of a few of seconds.

We can use the following function below to help with the time
```scala
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  "%.5f".format((end - start)/(i * 1.0e9))
}

for (i <- 0 to 5000000 by 500000) {
  println(s"$i ${time_needed(2, matcher(EVIL, "a" * i))} secs.") 
}
```

This matcher is very powerful, here is another "power" test
```scala
/*
  the Iterator produces the rexp
  SEQ(SEQ(SEQ(..., ONE | ONE) , ONE | ONE), ONE | ONE)
  where SEQ is nested 50 times.
*/
simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next()) == ONE
```
