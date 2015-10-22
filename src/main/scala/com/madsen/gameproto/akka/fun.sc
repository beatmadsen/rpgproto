val x = Seq(1, 2, 3, 4, 5, 6, 7, 8)

val l = x.indexOfSlice(Seq(3, 4))

val (a, b) = x.splitAt(l + 2)