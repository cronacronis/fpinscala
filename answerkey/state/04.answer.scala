// A simple recursive solution
def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  if (count <= 0) 
    (List(), rng)
  else {
    val (x, r1)  = rng.nextInt
    val (xs, r2) = ints(count - 1)(r1)
    (x :: xs, r2)
  }

// A tail-recursive solution
def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
  def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
    if (count <= 0)
      (xs, r)
    else {
      val (x, r2) = r.nextInt
      go(count - 1, r2, x :: xs)
    }
  go(count, rng, List())
}

// Using Stream
def ints3(count: Int)(rng: RNG): (List[Int], RNG) = {
	val newVals = Stream.unfold(rng){
		//f: S => Option[(A, S)]
		initGen => {
			val generated = initGen.nextInt
			Option.map2(Some(generated), Some(generated._2))((_, _))
		}
	}	
	val genStream = newVals take(count)
	genStream.foldRight(List[Int](), rng){
		case((curInt, curRng), (nlist, prevRng)) => {
			((curInt :: nlist),  curRng)
		}
	
	}
}
