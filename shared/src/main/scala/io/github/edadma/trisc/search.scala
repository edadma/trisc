package io.github.edadma.trisc

def search[T, E](seq: IndexedSeq[T], elem: E, lt: (E, T) => Boolean, eq: (E, T) => Boolean): Either[Int, Int] =
  def search(low: Int, high: Int): Either[Int, Int] =
    // If element not found
    if (low > high) Left(low) // return the insertion point
    else
      // Getting the middle element
      var middle = low + (high - low) / 2

      // If element found
      if eq(elem, seq(middle)) then
        Right(middle)
        // If element is before middle element
      else if lt(elem, seq(middle)) then
        // Searching in the left half
        search(low, middle - 1)
      else
        // Searching in the right half
        search(middle + 1, high)

  search(0, seq.length - 1)
end search
